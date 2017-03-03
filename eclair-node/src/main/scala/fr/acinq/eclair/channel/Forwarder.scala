package fr.acinq.eclair.channel

import akka.actor.{Actor, ActorLogging, ActorRef}
import fr.acinq.eclair.NodeParams
import fr.acinq.eclair.db.Dbs
import fr.acinq.eclair.wire.{LightningMessage, Error}

/**
  * Created by fabrice on 27/02/17.
  */

case class StoreAndForward(messages: Seq[LightningMessage], data: HasCommitments)

class Forwarder(nodeParams: NodeParams) extends Actor with ActorLogging {

  def receive = {
    case destination: ActorRef => context become main(destination)
  }

  def main(destination: ActorRef, channelId: Option[Long] = None): Receive = {

    case destination: ActorRef => context become main(destination, channelId)

    case error: Error => destination forward error

    case msg: LightningMessage => destination forward msg

    case StoreAndForward(messages, data) =>
      if (messages.size > 0) {
        log.debug(s"sending ${messages.map(_.getClass.getSimpleName).mkString(" ")}")
        messages.foreach(destination forward _)
      }
      val nextId = Helpers.getChannelId(data)
      nodeParams.channelsDb.put(nextId, data)

      channelId.map(previousId => {
        if (previousId != nextId) {
          nodeParams.channelsDb.delete(previousId)
        }
      })
      context become main(destination, Some(nextId))
  }
}

