package fr.acinq.eclair.transactions

import java.nio.ByteOrder

import fr.acinq.bitcoin.Crypto.{PrivateKey, sha256}
import fr.acinq.bitcoin.Script.{pay2wpkh, pay2wsh, write}
import fr.acinq.bitcoin._
import fr.acinq.eclair.channel.Helpers.Funding
import fr.acinq.eclair.transactions.Scripts.toLocalDelayed
import fr.acinq.eclair.transactions.Transactions.{addSigs, _}
import fr.acinq.eclair.wire.UpdateAddHtlc
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.util.{Failure, Random, Success, Try}

/**
  * Created by PM on 16/12/2016.
  */
@RunWith(classOf[JUnitRunner])
class TransactionsSpec extends FunSuite {

  test("encode/decode sequence and locktime (one example)") {

    val txnumber = 0x11F71FB268DL

    val (sequence, locktime) = encodeTxNumber(txnumber)
    assert(sequence == 0x80011F71L)
    assert(locktime == 0x20FB268DL)

    val txnumber1 = decodeTxNumber(sequence, locktime)
    assert(txnumber == txnumber1)
  }

  test("reconstruct txnumber from sequence and locktime") {

    for (i <- 0 until 1000) {
      val txnumber = Random.nextLong() & 0xffffffffffffL
      val (sequence, locktime) = encodeTxNumber(txnumber)
      val txnumber1 = decodeTxNumber(sequence, locktime)
      assert(txnumber == txnumber1)
    }
  }

  test("compute fees") {
    // see BOLT #3 specs
    val htlcs = Set(
      Htlc(OUT, UpdateAddHtlc(0, 0, MilliSatoshi(5000000).amount, 552, Hash.Zeroes, BinaryData("")), None),
      Htlc(OUT, UpdateAddHtlc(0, 0, MilliSatoshi(1000000).amount, 553, Hash.Zeroes, BinaryData("")), None),
      Htlc(IN, UpdateAddHtlc(0, 0, MilliSatoshi(7000000).amount, 550, Hash.Zeroes, BinaryData("")), None),
      Htlc(IN, UpdateAddHtlc(0, 0, MilliSatoshi(800000).amount, 551, Hash.Zeroes, BinaryData("")), None)
    )
    val spec = CommitmentSpec(htlcs, feeRatePerKw = 5000, toLocalMsat = 0, toRemoteMsat = 0)
    val fee = Transactions.commitTxFee(5000, Satoshi(546), spec)
    assert(fee == Satoshi(5340))
  }

  test("check pre-computed transaction weights") {
    val localRevocationPriv = PrivateKey(BinaryData("cc" * 32), compressed = true)
    val localPaymentPriv = PrivateKey(BinaryData("dd" * 32), compressed = true)
    val remotePaymentPriv = PrivateKey(BinaryData("ee" * 32), compressed = true)
    val remoteRevocationPriv = PrivateKey(BinaryData("ef" * 32), compressed = true)
    val localFinalPriv = PrivateKey(BinaryData("ff" * 32), compressed = true)
    val finalPubKeyScript = Script.pay2wpkh(PrivateKey(BinaryData("ff" * 32), compressed = true).publicKey)
    val toLocalDelay = 144
    val feeRatePerKw = 1000

    {
      // ClaimP2WPKHOutputTx
      // first we create a fake commitTx tx, containing only the output that will be spent by the ClaimP2WPKHOutputTx
      val pubKeyScript = write(pay2wpkh(localPaymentPriv.publicKey))
      val commitTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(20000), pubKeyScript) :: Nil, lockTime = 0)
      val claimP2WPKHOutputTx = makeClaimP2WPKHOutputTx(commitTx, localPaymentPriv.publicKey, finalPubKeyScript, feeRatePerKw)
      // we use dummy signatures to compute the weight
      val weight = Transaction.weight(addSigs(claimP2WPKHOutputTx, localPaymentPriv.publicKey, "bb" * 71).tx)
      assert(claimP2WPKHOutputWeight == weight)
    }

    {
      // ClaimHtlcDelayedTx
      // first we create a fake htlcSuccessOrTimeoutTx tx, containing only the output that will be spent by the ClaimDelayedOutputTx
      val pubKeyScript = write(pay2wsh(toLocalDelayed(localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey)))
      val htlcSuccessOrTimeoutTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(20000), pubKeyScript) :: Nil, lockTime = 0)
      val claimHtlcDelayedTx = makeClaimDelayedOutputTx(htlcSuccessOrTimeoutTx, localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey, finalPubKeyScript, feeRatePerKw)
      // we use dummy signatures to compute the weight
      val weight = Transaction.weight(addSigs(claimHtlcDelayedTx, "bb" * 71).tx)
      assert(claimHtlcDelayedWeight == weight)
    }

    {
      // MainPunishmentTx
      // first we create a fake commitTx tx, containing only the output that will be spent by the MainPunishmentTx
      val pubKeyScript = write(pay2wsh(toLocalDelayed(localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey)))
      val commitTx = Transaction(version = 0, txIn = Nil, txOut = TxOut(Satoshi(20000), pubKeyScript) :: Nil, lockTime = 0)
      val mainPunishmentTx = makeMainPunishmentTx(commitTx, localRevocationPriv.publicKey, finalPubKeyScript, toLocalDelay, localPaymentPriv.publicKey, feeRatePerKw)
      // we use dummy signatures to compute the weight
      val weight = Transaction.weight(addSigs(mainPunishmentTx, "bb" * 71).tx)
      assert(mainPunishmentWeight == weight)
    }
  }

  test("generate valid commitment and htlc transactions") {
    val localFundingPriv = PrivateKey(BinaryData("aa" * 32) :+ 1.toByte)
    val remoteFundingPriv = PrivateKey(BinaryData("bb" * 32) :+ 1.toByte)
    val localRevocationPriv = PrivateKey(BinaryData("cc" * 32) :+ 1.toByte)
    val localPaymentPriv = PrivateKey(BinaryData("dd" * 32) :+ 1.toByte)
    val remotePaymentPriv = PrivateKey(BinaryData("ee" * 32) :+ 1.toByte)
    val remoteRevocationPriv = PrivateKey(BinaryData("cc" * 32) :+ 1.toByte)
    val finalPubKeyScript = Script.pay2wpkh(PrivateKey(BinaryData("ee" * 32), true).publicKey)
    val commitInput = Funding.makeFundingInputInfo(BinaryData("12" * 32), 0, Btc(1), localFundingPriv.publicKey, remoteFundingPriv.publicKey)
    val toLocalDelay = 144
    val localDustLimit = Satoshi(542)
    val feeRatePerKw = 1000

    val paymentPreimage1 = BinaryData("11" * 32)
    val htlc1 = UpdateAddHtlc(0, 0, millibtc2satoshi(MilliBtc(100)).amount * 1000, 300, sha256(paymentPreimage1), BinaryData(""))
    val paymentPreimage2 = BinaryData("22" * 32)
    val htlc2 = UpdateAddHtlc(0, 1, millibtc2satoshi(MilliBtc(200)).amount * 1000, 300, sha256(paymentPreimage2), BinaryData(""))
    val spec = CommitmentSpec(
      htlcs = Set(
        Htlc(OUT, htlc1, None),
        Htlc(IN, htlc2, None)
      ),
      feeRatePerKw = feeRatePerKw,
      toLocalMsat = millibtc2satoshi(MilliBtc(400)).amount * 1000,
      toRemoteMsat = millibtc2satoshi(MilliBtc(300)).amount * 1000)

    val commitTxNumber = 0x404142434445L
    val commitTx = {
      val txinfo = makeCommitTx(commitInput, commitTxNumber, localPaymentPriv.toPoint, remotePaymentPriv.toPoint, true, localDustLimit, localPaymentPriv.publicKey, localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey, remotePaymentPriv.publicKey, spec)
      val localSig = Transactions.sign(txinfo, localPaymentPriv)
      val remoteSig = Transactions.sign(txinfo, remotePaymentPriv)
      Transactions.addSigs(txinfo, localFundingPriv.publicKey, remoteFundingPriv.publicKey, localSig, remoteSig)
    }

    {
      assert(getCommitTxNumber(commitTx.tx, localPaymentPriv.publicKey, remotePaymentPriv.publicKey) == commitTxNumber)
      val hash: Array[Byte] = Crypto.sha256(localPaymentPriv.publicKey.toBin ++ remotePaymentPriv.publicKey.toBin)
      val num = Protocol.uint64(hash.takeRight(8), ByteOrder.BIG_ENDIAN) & 0xffffffffffffL
      val check = ((commitTx.tx.txIn(0).sequence & 0xffffff) << 24) | (commitTx.tx.lockTime)
      assert((check ^ num) == commitTxNumber)
    }
    val (htlcTimeoutTxs, htlcSuccessTxs) = makeHtlcTxs(commitTx.tx, localDustLimit, localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey, localPaymentPriv.publicKey, remotePaymentPriv.publicKey, spec)

    assert(htlcTimeoutTxs.size == 1)
    assert(htlcSuccessTxs.size == 1)

    {
      // either party spends local->remote htlc output with htlc timeout tx
      val htlcTimeoutTx = htlcTimeoutTxs(0)
      val localSig = sign(htlcTimeoutTx, localPaymentPriv)
      val remoteSig = sign(htlcTimeoutTx, remotePaymentPriv)
      val signed = addSigs(htlcTimeoutTx, localSig, remoteSig)
      assert(checkSpendable(signed).isSuccess)
    }

    {
      // local spends delayed output of htlc timeout tx
      val htlcTimeoutTx = htlcTimeoutTxs(0)
      val claimHtlcDelayed = makeClaimDelayedOutputTx(htlcTimeoutTx.tx, localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey, finalPubKeyScript, feeRatePerKw)
      val localSig = sign(claimHtlcDelayed, localPaymentPriv)
      val signedTx = addSigs(claimHtlcDelayed, localSig)
      assert(checkSpendable(signedTx).isSuccess)
    }

    {
      // remote spends local->remote htlc output directly in case of success
      val claimHtlcSuccessTx = makeClaimHtlcSuccessTx(commitTx.tx, remotePaymentPriv.publicKey, localPaymentPriv.publicKey, remoteRevocationPriv.publicKey, finalPubKeyScript, htlc1)
      val localSig = sign(claimHtlcSuccessTx, remotePaymentPriv)
      val signed = addSigs(claimHtlcSuccessTx, localSig, paymentPreimage1)
      assert(checkSpendable(signed).isSuccess)
    }

    {
      // local spends remote->local htlc output with htlc success tx using payment preimage
      val htlcSuccessTx = htlcSuccessTxs(0)
      val localSig = sign(htlcSuccessTx, localPaymentPriv)
      val remoteSig = sign(htlcSuccessTx, remotePaymentPriv)
      val signedTx = addSigs(htlcSuccessTx, localSig, remoteSig, paymentPreimage2)
      assert(checkSpendable(signedTx).isSuccess)
      // check remote sig
      assert(checkSig(htlcSuccessTx, remoteSig, remotePaymentPriv.publicKey))
    }

    {
      // local spends delayed output of htlc success tx
      val htlcSuccessTx = htlcSuccessTxs(0)
      val claimHtlcDelayed = makeClaimDelayedOutputTx(htlcSuccessTx.tx, localRevocationPriv.publicKey, toLocalDelay, localPaymentPriv.publicKey, finalPubKeyScript, feeRatePerKw)
      val localSig = sign(claimHtlcDelayed, localPaymentPriv)
      val signedTx = addSigs(claimHtlcDelayed, localSig)
      assert(checkSpendable(signedTx).isSuccess)
    }

    {
      // remote spends main output
      val claimP2WPKHOutputTx = makeClaimP2WPKHOutputTx(commitTx.tx, remotePaymentPriv.publicKey, finalPubKeyScript, feeRatePerKw)
      val localSig = sign(claimP2WPKHOutputTx, remotePaymentPriv)
      val signedTx = addSigs(claimP2WPKHOutputTx, remotePaymentPriv.publicKey, localSig)
      assert(checkSpendable(signedTx).isSuccess)
    }

    {
      // remote spends remote->local htlc output directly in case of timeout
      val claimHtlcTimeoutTx = makeClaimHtlcTimeoutTx(commitTx.tx, remotePaymentPriv.publicKey, localPaymentPriv.publicKey, remoteRevocationPriv.publicKey, finalPubKeyScript, htlc2)
      val localSig = sign(claimHtlcTimeoutTx, remotePaymentPriv)
      val signed = addSigs(claimHtlcTimeoutTx, localSig)
      assert(checkSpendable(signed).isSuccess)
    }

  }

  test("new version of offered HTLC can be claimed with the payment preimage or the revocation private key") {
    val localFundingPriv = PrivateKey(BinaryData("30ff4956bbdd3222d44cc5e8a1261dab1e07957bdac5ae88fe3261ef321f3749") :+ 1.toByte)
    val remoteFundingPriv = PrivateKey(BinaryData("1552dfba4f6cf29a62a0af13c8d6981d36d0ef8d61ba10fb0fe90da7634d7e13") :+ 1.toByte)
    val localRevocationPriv = PrivateKey(BinaryData("131526c63723ff1d36c28e61a8bdc86660d7893879bbda4cfeaad2022db7c109") :+ 1.toByte)
    val localPaymentPriv = PrivateKey(BinaryData("e937268a37a774aa948ebddff3187fedc7035e3f0a029d8d85f31bda33b02d55") :+ 1.toByte)
    val remotePaymentPriv = PrivateKey(BinaryData("ce65059278a571ee4f4c9b4d5d7fa07449bbe09d9c716879343d9e975df1de33") :+ 1.toByte)
    val paymentPreimage = BinaryData("0102030405060708010203040506070801020304050607080102030405060708")
    val redeemScript = Scripts.htlcOffered(localPaymentPriv.publicKey, remotePaymentPriv.publicKey, localRevocationPriv.publicKey, Crypto.hash160(paymentPreimage))
    val tx = Transaction(version = 2,
      txIn = Nil,
      txOut = TxOut(MilliBtc(42), Script.pay2wsh(redeemScript)) :: Nil,
      lockTime = 0
    )

    // spend with payment preimage
    val tx2 = {
      val tmp = Transaction(version = 2, txIn = TxIn(OutPoint(tx, 0), Nil, TxIn.SEQUENCE_FINAL) :: Nil, txOut = TxOut(MilliBtc(42), pay2wpkh(remotePaymentPriv.publicKey)) :: Nil, 0)
      val sig = Transaction.signInput(tmp, 0, redeemScript, fr.acinq.bitcoin.SIGHASH_ALL, tx.txOut(0).amount, SigVersion.SIGVERSION_WITNESS_V0, remotePaymentPriv)
      tmp.updateWitness(0, ScriptWitness(sig :: paymentPreimage :: Script.write(redeemScript) :: Nil))
    }
    Transaction.correctlySpends(tx2, tx :: Nil, ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)

    // spend with remote key and revocation key
    val tx3 = {
      val tmp = Transaction(version = 2, txIn = TxIn(OutPoint(tx, 0), Nil, TxIn.SEQUENCE_FINAL) :: Nil, txOut = TxOut(MilliBtc(42), pay2wpkh(remotePaymentPriv.publicKey)) :: Nil, 0)
      val sig1 = Transaction.signInput(tmp, 0, redeemScript, fr.acinq.bitcoin.SIGHASH_ALL, tx.txOut(0).amount, SigVersion.SIGVERSION_WITNESS_V0, remotePaymentPriv)
      val sig2 = Transaction.signInput(tmp, 0, redeemScript, fr.acinq.bitcoin.SIGHASH_ALL, tx.txOut(0).amount, SigVersion.SIGVERSION_WITNESS_V0, localRevocationPriv)
      tmp.updateWitness(0, ScriptWitness(BinaryData.empty :: sig1 :: sig2 :: BinaryData.empty :: Script.write(redeemScript) :: Nil))
    }
    Transaction.correctlySpends(tx3, tx :: Nil, ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)


    // spend with remote key and local key
    val tx4 = {
      val tmp = Transaction(version = 2, txIn = TxIn(OutPoint(tx, 0), Nil, TxIn.SEQUENCE_FINAL) :: Nil, txOut = TxOut(MilliBtc(42), pay2wpkh(remotePaymentPriv.publicKey)) :: Nil, 0)
      val sig1 = Transaction.signInput(tmp, 0, redeemScript, fr.acinq.bitcoin.SIGHASH_ALL, tx.txOut(0).amount, SigVersion.SIGVERSION_WITNESS_V0, remotePaymentPriv)
      val sig2 = Transaction.signInput(tmp, 0, redeemScript, fr.acinq.bitcoin.SIGHASH_ALL, tx.txOut(0).amount, SigVersion.SIGVERSION_WITNESS_V0, localPaymentPriv)
      tmp.updateWitness(0, ScriptWitness(BinaryData.empty :: sig1 :: sig2 :: BinaryData.empty :: Script.write(redeemScript) :: Nil))
    }
    Transaction.correctlySpends(tx4, tx :: Nil, ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)
  }

  test("new version of received HTLC can be claimed with the payment preimage or the revocation private key") {
    val localFundingPriv = PrivateKey(BinaryData("30ff4956bbdd3222d44cc5e8a1261dab1e07957bdac5ae88fe3261ef321f3749") :+ 1.toByte)
    val remoteFundingPriv = PrivateKey(BinaryData("1552dfba4f6cf29a62a0af13c8d6981d36d0ef8d61ba10fb0fe90da7634d7e13") :+ 1.toByte)
    val localRevocationPriv = PrivateKey(BinaryData("131526c63723ff1d36c28e61a8bdc86660d7893879bbda4cfeaad2022db7c109") :+ 1.toByte)
    val localPaymentPriv = PrivateKey(BinaryData("e937268a37a774aa948ebddff3187fedc7035e3f0a029d8d85f31bda33b02d55") :+ 1.toByte)
    val remotePaymentPriv = PrivateKey(BinaryData("ce65059278a571ee4f4c9b4d5d7fa07449bbe09d9c716879343d9e975df1de33") :+ 1.toByte)
    val paymentPreimage = BinaryData("0102030405060708010203040506070801020304050607080102030405060708")
    val redeemScript = Scripts.htlcReceived(localPaymentPriv.publicKey, remotePaymentPriv.publicKey, localRevocationPriv.publicKey, Crypto.hash160(paymentPreimage), 144)
    val tx = Transaction(version = 2,
      txIn = Nil,
      txOut = TxOut(MilliBtc(42), Script.pay2wsh(redeemScript)) :: Nil,
      lockTime = 0
    )

    // spend with payment preimage , local key and remote key
    val tx2 = {
      val tmp = Transaction(version = 2, txIn = TxIn(OutPoint(tx, 0), Nil, TxIn.SEQUENCE_FINAL) :: Nil, txOut = TxOut(MilliBtc(42), pay2wpkh(remotePaymentPriv.publicKey)) :: Nil, 0)
      val sig1 = Transaction.signInput(tmp, 0, redeemScript, fr.acinq.bitcoin.SIGHASH_ALL, tx.txOut(0).amount, SigVersion.SIGVERSION_WITNESS_V0, remotePaymentPriv)
      val sig2 = Transaction.signInput(tmp, 0, redeemScript, fr.acinq.bitcoin.SIGHASH_ALL, tx.txOut(0).amount, SigVersion.SIGVERSION_WITNESS_V0, localPaymentPriv)
      tmp.updateWitness(0, ScriptWitness(BinaryData.empty :: sig1 :: sig2 :: paymentPreimage :: Script.write(redeemScript) :: Nil))
    }
    Transaction.correctlySpends(tx2, tx :: Nil, ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)

    // spend with remote key after a delay
    val tx3 = {
      val tmp = Transaction(version = 2, txIn = TxIn(OutPoint(tx, 0), Nil, TxIn.SEQUENCE_LOCKTIME_MASK) :: Nil, txOut = TxOut(MilliBtc(42), pay2wpkh(remotePaymentPriv.publicKey)) :: Nil, lockTime = 145)
      val sig = Transaction.signInput(tmp, 0, redeemScript, fr.acinq.bitcoin.SIGHASH_ALL, tx.txOut(0).amount, SigVersion.SIGVERSION_WITNESS_V0, remotePaymentPriv)
      tmp.updateWitness(0, ScriptWitness(sig :: BinaryData.empty :: Script.write(redeemScript) :: Nil))
    }
    Transaction.correctlySpends(tx3, tx :: Nil, ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)


    // spend with revocation key
    val tx4 = {
      val tmp = Transaction(version = 2, txIn = TxIn(OutPoint(tx, 0), Nil, TxIn.SEQUENCE_FINAL) :: Nil, txOut = TxOut(MilliBtc(42), pay2wpkh(remotePaymentPriv.publicKey)) :: Nil, 0)
      val sig2 = Transaction.signInput(tmp, 0, redeemScript, fr.acinq.bitcoin.SIGHASH_ALL, tx.txOut(0).amount, SigVersion.SIGVERSION_WITNESS_V0, localRevocationPriv)
      tmp.updateWitness(0, ScriptWitness(sig2 :: Script.write(redeemScript) :: Nil))
    }
    Transaction.correctlySpends(tx4, tx :: Nil, ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)
  }

  def checkSuccessOrFailTest[T](input: Try[T]) = input match {
    case Success(_) => ()
    case Failure(t) => fail(t)
  }
}
