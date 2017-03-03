package fr.acinq.eclair.db

import fr.acinq.bitcoin.BinaryData
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.eclair.channel.{Data, HasCommitments}
import fr.acinq.eclair.crypto.TransportHandler.Serializer
import fr.acinq.eclair.io.{LightningMessageSerializer, PeerRecord}
import fr.acinq.eclair.wire.LightningMessage

/**
  * Created by PM on 28/02/2017.
  */
object Dbs {

  def makeChannelDb(db: SimpleDb): SimpleTypedDb[Long, HasCommitments] = {
    def channelid2String(id: Long) = s"channel-$id"

    def string2channelid(s: String) = if (s.startsWith("channel-")) Some(s.stripPrefix("channel-").toLong) else None

    new SimpleTypedDb[Long, HasCommitments](
      channelid2String,
      string2channelid,
      new Serializer[HasCommitments] {
        override def serialize(t: HasCommitments): BinaryData = JavaSerializer.serialize(t)

        override def deserialize(bin: BinaryData): HasCommitments = JavaSerializer.deserialize[HasCommitments](bin)
      },
      db
    )
  }

  def makeAnnouncementDb(db: SimpleDb): SimpleTypedDb[String, LightningMessage] = {
    // we use a single key: router.state
    new SimpleTypedDb[String, LightningMessage](
      s => s,
      s => if (s.startsWith("ann-")) Some(s) else None,
      LightningMessageSerializer,
      db
    )
  }

  def makePeerDb(db: SimpleDb): SimpleTypedDb[PublicKey, PeerRecord] = {
    def peerid2String(id: PublicKey) = s"peer-$id"

    def string2peerid(s: String) = if (s.startsWith("peer-")) Some(PublicKey(BinaryData(s.stripPrefix("peer-")))) else None

    new SimpleTypedDb[PublicKey, PeerRecord](
      peerid2String,
      string2peerid,
      new Serializer[PeerRecord] {
        override def serialize(t: PeerRecord): BinaryData = JavaSerializer.serialize(t)

        override def deserialize(bin: BinaryData): PeerRecord = JavaSerializer.deserialize[PeerRecord](bin)
      },
      db
    )
  }

}
