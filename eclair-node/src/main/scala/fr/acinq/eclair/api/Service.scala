package fr.acinq.eclair.api

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.CacheDirectives.{`max-age`, `no-store`, public}
import akka.http.scaladsl.model.headers.HttpOriginRange.*
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.util.Timeout
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import de.heikoseeberger.akkahttpjson4s.Json4sSupport.ShouldWritePretty
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.{BinaryData, MilliSatoshi, Satoshi}
import fr.acinq.eclair.channel._
import fr.acinq.eclair.io.Switchboard.{NewChannel, NewConnection}
import fr.acinq.eclair.payment.CreatePayment
import fr.acinq.eclair.wire.NodeAnnouncement
import grizzled.slf4j.Logging
import org.json4s.JsonAST.{JInt, JString}
import org.json4s.{JValue, jackson}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Created by PM on 25/01/2016.
  */

// @formatter:off
case class JsonRPCBody(jsonrpc: String = "1.0", id: String = "scala-client", method: String, params: Seq[JValue])
case class Error(code: Int, message: String)
case class JsonRPCRes(result: AnyRef, error: Option[Error], id: String)
case class Status(node_id: String)
// @formatter:on

trait Service extends Logging {

  implicit def ec: ExecutionContext = ExecutionContext.Implicits.global

  implicit val serialization = jackson.Serialization
  implicit val formats = org.json4s.DefaultFormats + new BinaryDataSerializer + new StateSerializer + new ShaChainSerializer + new PublicKeySerializer + new PrivateKeySerializer + new ScalarSerializer + new PointSerializer + new TransactionWithInputInfoSerializer
  implicit val timeout = Timeout(30 seconds)
  implicit val shouldWritePretty: ShouldWritePretty = ShouldWritePretty.True

  import Json4sSupport.{json4sMarshaller, json4sUnmarshaller}

  def switchboard: ActorRef

  def router: ActorRef

  def register: ActorRef

  def paymentInitiator: ActorRef

  def paymentHandler: ActorRef

  def system: ActorSystem

  val customHeaders = `Access-Control-Allow-Origin`(*) ::
    `Access-Control-Allow-Headers`("Content-Type, Authorization") ::
    `Access-Control-Allow-Methods`(PUT, GET, POST, DELETE, OPTIONS) ::
    `Cache-Control`(public, `no-store`, `max-age`(0)) ::
    `Access-Control-Allow-Headers`("x-requested-with") :: Nil

  def getChannel(channelIdHex: String): Future[ActorRef] =
    for {
      channels <- (register ? 'channels).mapTo[Map[String, ActorRef]]
    } yield channels.get(channelIdHex).getOrElse(throw new RuntimeException("unknown channel"))

  val route =
    respondWithDefaultHeaders(customHeaders) {
      pathSingleSlash {
        post {
          entity(as[JsonRPCBody]) {
            req =>
              val f_res: Future[AnyRef] = req match {
                case JsonRPCBody(_, _, "connect", JString(host) :: JInt(port) :: JString(pubkey) :: Nil) =>
                  (switchboard ? NewConnection(PublicKey(pubkey), new InetSocketAddress(host, port.toInt), None)).mapTo[String]
                case JsonRPCBody(_, _, "open", JString(host) :: JInt(port) :: JString(pubkey) :: JInt(fundingSatoshi) :: JInt(pushMsat) :: Nil) =>
                  (switchboard ? NewConnection(PublicKey(pubkey), new InetSocketAddress(host, port.toInt), Some(NewChannel(Satoshi(fundingSatoshi.toLong), MilliSatoshi(pushMsat.toLong))))).mapTo[String]
                case JsonRPCBody(_, _, "peers", _) =>
                  (switchboard ? 'peers).mapTo[Iterable[PublicKey]].map(_.map(_.toBin))
                case JsonRPCBody(_, _, "channels", _) =>
                  (register ? 'channels).mapTo[Map[Long, ActorRef]].map(_.keys)
                case JsonRPCBody(_, _, "channel", JString(channelIdHex) :: Nil) =>
                  getChannel(channelIdHex).flatMap(_ ? CMD_GETINFO).mapTo[RES_GETINFO]
                case JsonRPCBody(_, _, "network", _) =>
                  (router ? 'nodes).mapTo[Iterable[NodeAnnouncement]].map(_.map(_.nodeId))
                case JsonRPCBody(_, _, "genh", _) =>
                  (paymentHandler ? 'genh).mapTo[BinaryData]
                case JsonRPCBody(_, _, "send", JInt(amountMsat) :: JString(paymentHash) :: JString(nodeId) :: Nil) =>
                  (paymentInitiator ? CreatePayment(amountMsat.toLong, paymentHash, PublicKey(nodeId))).mapTo[String]
                case JsonRPCBody(_, _, "close", JString(channelIdHex) :: JString(scriptPubKey) :: Nil) =>
                  getChannel(channelIdHex).flatMap(_ ? CMD_CLOSE(scriptPubKey = Some(scriptPubKey))).mapTo[String]
                case JsonRPCBody(_, _, "close", JString(channelIdHex) :: Nil) =>
                  getChannel(channelIdHex).flatMap(_ ? CMD_CLOSE(scriptPubKey = None)).mapTo[String]
                case JsonRPCBody(_, _, "help", _) =>
                  Future.successful(List(
                    "info: display basic node information",
                    "connect (host, port, nodeId): opens a secure connection with another lightning node",
                    "connect (host, port, nodeId, fundingSat, pushMsat): open a channel with another lightning node",
                    "peers: list existing peers",
                    "channels: list existing channels",
                    "channel (channelId): retrieve detailed information about a given channel",
                    "send (amount, paymentHash, nodeId): send a payment to a lightning node",
                    "close (channel_id): close a channel",
                    "close (channel_id, scriptPubKey): close a channel and send the funds to the given scriptPubKey",
                    "help: display this message"))
                case _ => Future.failed(new RuntimeException("method not found"))
              }

              onComplete(f_res) {
                case Success(res) => complete(JsonRPCRes(res, None, req.id))
                case Failure(t) => complete(StatusCodes.InternalServerError, JsonRPCRes(null, Some(Error(-1, t.getMessage)), req.id))
              }
          }
        }
      }
    }
}
