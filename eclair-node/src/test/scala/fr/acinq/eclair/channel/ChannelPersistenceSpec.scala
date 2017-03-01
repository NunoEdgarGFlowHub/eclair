package fr.acinq.eclair.channel

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{TestFSMRef, TestKit, TestProbe}
import fr.acinq.eclair.TestConstants.{Alice, Bob}
import fr.acinq.eclair.{Pipe, TestBitcoinClient, TestConstants, TestkitBaseClass}
import fr.acinq.eclair.blockchain._
import fr.acinq.eclair.channel.states.StateTestsHelperMethods
import fr.acinq.eclair.payment.{LocalPaymentHandler, Relayer}
import fr.acinq.eclair.wire._
import org.scalatest.FunSuiteLike

import scala.concurrent.duration._

/**
  * Created by fabrice on 28/02/17.
  */
class ChannelPersistenceSpec extends TestKit(ActorSystem("test")) with FunSuiteLike {

  //  type FixtureParam = Tuple7[TestFSMRef[State, Data, Channel], TestFSMRef[State, Data, Channel], ActorRef, ActorRef, ActorRef, ActorRef, ActorRef]
  //
  //  override def withFixture(test: OneArgTest) = {
  //    val pipe = system.actorOf(Props(new Pipe()))
  //    val alice2blockchain = TestProbe()
  //    val blockchainA = system.actorOf(Props(new PeerWatcher(new TestBitcoinClient())))
  //    val bob2blockchain = TestProbe()
  //    val paymentHandlerA = system.actorOf(Props(new LocalPaymentHandler()), name = "payment-handler-a")
  //    val paymentHandlerB = system.actorOf(Props(new LocalPaymentHandler()), name = "payment-handler-b")
  //    val relayerA = system.actorOf(Relayer.props(Alice.nodeParams.privateKey, paymentHandlerA), "relayer-a")
  //    val relayerB = system.actorOf(Relayer.props(Bob.nodeParams.privateKey, paymentHandlerB), "relayer-b")
  //    val router = TestProbe()
  //    val alice: TestFSMRef[State, Data, Channel] = TestFSMRef(new Channel(Alice.nodeParams, pipe, alice2blockchain.ref, router.ref, relayerA))
  //    val bob: TestFSMRef[State, Data, Channel] = TestFSMRef(new Channel(Bob.nodeParams, pipe, bob2blockchain.ref, router.ref, relayerB))
  //    within(30 seconds) {
  //      val aliceInit = Init(Alice.channelParams.globalFeatures, Alice.channelParams.localFeatures)
  //      val bobInit = Init(Bob.channelParams.globalFeatures, Bob.channelParams.localFeatures)
  //      relayerA ! alice
  //      relayerB ! bob
  //      alice ! INPUT_INIT_FUNDER(Bob.id, 0, TestConstants.fundingSatoshis, TestConstants.pushMsat, Alice.channelParams, bobInit)
  //      bob ! INPUT_INIT_FUNDEE(Alice.id, 0, Bob.channelParams, aliceInit)
  //      pipe ! (alice, bob)
  //      alice2blockchain.expectMsgType[MakeFundingTx]
  //      alice2blockchain.forward(blockchainA)
  //      alice2blockchain.expectMsgType[WatchSpent]
  //      alice2blockchain.expectMsgType[WatchConfirmed]
  //      alice2blockchain.forward(blockchainA)
  //      alice2blockchain.expectMsgType[PublishAsap]
  //      alice2blockchain.forward(blockchainA)
  //      bob2blockchain.expectMsgType[WatchSpent]
  //      bob2blockchain.expectMsgType[WatchConfirmed]
  //    }
  //    test((alice, bob, pipe, relayerA, relayerB, paymentHandlerA, paymentHandlerB))
  //  }

  test("recv FundingLocked") {
    val pipe = system.actorOf(Props(new Pipe()))
    val alice2blockchain = TestProbe()
    val blockchainA = system.actorOf(Props(new PeerWatcher(new TestBitcoinClient())))
    val bob2blockchain = TestProbe()
    val paymentHandlerA = system.actorOf(Props(new LocalPaymentHandler()), name = "payment-handler-a")
    val paymentHandlerB = system.actorOf(Props(new LocalPaymentHandler()), name = "payment-handler-b")
    val relayerA = system.actorOf(Relayer.props(Alice.nodeParams.privateKey, paymentHandlerA), "relayer-a")
    val relayerB = system.actorOf(Relayer.props(Bob.nodeParams.privateKey, paymentHandlerB), "relayer-b")
    val router = TestProbe()
    val alice: TestFSMRef[State, Data, Channel] = TestFSMRef(new Channel(Alice.nodeParams, pipe, alice2blockchain.ref, router.ref, relayerA))
    val bob: TestFSMRef[State, Data, Channel] = TestFSMRef(new Channel(Bob.nodeParams, pipe, bob2blockchain.ref, router.ref, relayerB))
    val dbA = Channel.makeChannelDb(Alice.nodeParams.db)
    val dbB = Channel.makeChannelDb(Bob.nodeParams.db)

    within(30 seconds) {
      val aliceInit = Init(Alice.channelParams.globalFeatures, Alice.channelParams.localFeatures)
      val bobInit = Init(Bob.channelParams.globalFeatures, Bob.channelParams.localFeatures)
      relayerA ! alice
      relayerB ! bob
      alice ! INPUT_INIT_FUNDER(Bob.id, 4242, TestConstants.fundingSatoshis, TestConstants.pushMsat, Alice.channelParams, bobInit)
      bob ! INPUT_INIT_FUNDEE(Alice.id, 4242, Bob.channelParams, aliceInit)
      pipe ! (alice, bob)
      alice2blockchain.expectMsgType[MakeFundingTx]
      alice2blockchain.forward(blockchainA)
      alice2blockchain.expectMsgType[WatchSpent]
      alice2blockchain.expectMsgType[WatchConfirmed]
      alice2blockchain.expectMsgType[PublishAsap]
      alice2blockchain.forward(blockchainA)
      bob2blockchain.expectMsgType[WatchSpent]
      bob2blockchain.expectMsgType[WatchConfirmed]
      awaitCond(alice.stateName == WAIT_FOR_FUNDING_CONFIRMED)
      awaitCond(bob.stateName == WAIT_FOR_FUNDING_CONFIRMED)

      pipe ! INPUT_DISCONNECTED
      awaitCond(alice.stateName == OFFLINE)
      awaitCond(bob.stateName == OFFLINE)
      alice.stop()
      bob.stop()

      val pipe1 = system.actorOf(Props(new Pipe()))
      val alice1: TestFSMRef[State, Data, Channel] = TestFSMRef(new Channel(Alice.nodeParams, pipe1, alice2blockchain.ref, router.ref, relayerA))
      awaitCond(!dbA.values.isEmpty)
      alice1 ! INPUT_RESTORED(dbA.values.headOption.get.id, dbA.values.headOption.get.state)
      alice1 ! INPUT_RECONNECTED(pipe1)

      val bob1: TestFSMRef[State, Data, Channel] = TestFSMRef(new Channel(Bob.nodeParams, pipe1, bob2blockchain.ref, router.ref, relayerB))
      bob1 ! INPUT_RESTORED(dbB.values.headOption.get.id, dbB.values.headOption.get.state)
      bob1 ! INPUT_RECONNECTED(pipe1)

      awaitCond(alice1.stateName == WAIT_FOR_FUNDING_CONFIRMED)
      awaitCond(bob1.stateName == WAIT_FOR_FUNDING_CONFIRMED)

      alice2blockchain.expectMsgType[WatchSpent]
      alice2blockchain.expectMsgType[WatchConfirmed]
      alice2blockchain.forward(blockchainA)
      awaitCond(alice1.stateName == WAIT_FOR_FUNDING_LOCKED)

      bob2blockchain.expectMsgType[WatchSpent]
      bob2blockchain.expectMsgType[WatchConfirmed]
      bob1 ! WatchEventConfirmed(BITCOIN_FUNDING_DEPTHOK, 400000, 42)
      awaitCond(bob1.stateName == WAIT_FOR_FUNDING_LOCKED)

      awaitCond(alice1.stateName == NORMAL)
      awaitCond(bob1.stateName == NORMAL)
    }
  }
}