package fr.acinq.eclair.channel

import akka.actor.{Actor, ActorLogging, ActorRef}
import fr.acinq.eclair.NodeParams
import fr.acinq.eclair.wire.{Error, LightningMessage}

/**
  * Created by fabrice on 27/02/17.
  */

case class StoreAndForward(stateData: Data, outgoing: Seq[LightningMessage])

class Forwarder(nodeParams: NodeParams) extends Actor with ActorLogging {

  // caller is responsible for sending the destination before anything else
  // the general case is that destination can die anytime and it is managed by the caller
  def receive = main(context.system.deadLetters)

  def main(destination: ActorRef): Receive = {

    case destination: ActorRef => context become main(destination)

    case error: Error => destination ! error

    case StoreAndForward(stateData: HasCommitments, outgoing) =>
      nodeParams.channelsDb.put(stateData.channelId, stateData)
      outgoing.foreach(destination forward _)

    case StoreAndForward(_, outgoing) =>
      outgoing.foreach(destination forward _)
  }
}
