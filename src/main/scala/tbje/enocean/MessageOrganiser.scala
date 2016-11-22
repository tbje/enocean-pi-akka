package tbje.enocean

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import akka.io.IO
import akka.util.{ ByteString => BS, CompactByteString => CBS }
import ch.jodersky.flow.{ AccessDeniedException, Serial, SerialSettings }
import util._

object MessageOrganiser {
  def props(port: String, settings: SerialSettings, parser: ActorRef): Props = Props(new MessageOrganiser(port, settings, parser))

  sealed trait Result
  case class Incomplete(data: BS) extends Result
  case class Complete(data: BS, dataLen: Int, optLen: Int) extends Result
  case class TooLong(data: BS, dataLen: Int, optLen: Int, rest: BS) extends Result
  def checkData(data: BS): Result = data match {
    case 0x55 +: len1 +: len2 +: opt +: rest =>
      val dataLen = toInt(CBS(len1, len2))
      val optLen = toInt(CBS(opt))
      val totalLen = 7 + dataLen + optLen
      data.length match {
        case `totalLen` => Complete(data, dataLen, optLen)
        case x if x > totalLen => TooLong(data.take(totalLen), dataLen, optLen, data.drop(totalLen))
        case x if x < totalLen => Incomplete(data)
      }
    case 0x55 +: rest => Incomplete(data)
    case x if x.length > 0 =>
      checkData(x.dropWhile(_ != 0x55))
    case _ => Incomplete(data)
  }

}

class MessageOrganiser(port: String, serialSettings: SerialSettings, parser: ActorRef) extends Actor with ActorLogging with SettingsActor {
  import context._
  import MessageOrganiser._

  if(!settings.test)
    IO(Serial) ! Serial.Open(port, serialSettings)
  else
    context.parent ! Controller.SerialOpened(context.system.deadLetters)

  private[this] val init: Receive = {
    case Serial.CommandFailed(cmd: Serial.Open, reason: AccessDeniedException) =>
      log.error("You're not allowed to open that port!")
    case Serial.CommandFailed(cmd: Serial.Open, reason) =>
      log.error("Could not open port for some other reason: " + reason.getMessage)
    case Serial.Opened(settings) => {
      log.debug(s"Port $port opened")
      context.parent ! Controller.SerialOpened(sender)
      context become running(sender)
    }
  }

  override val receive: Receive = init

  import util.DSL._
  private def formatData(data: BS): String = data.hex

  private[this] def running(operator: ActorRef, data: BS = BS.empty): Receive = {
    case Serial.Received(received) =>
      MessageOrganiser.checkData(data ++ received) match {
        case Complete(data, dataLen, optLen) =>
          parser ! Parser.Parse(data, dataLen, optLen)
          context become running(operator)
        case TooLong(data, dataLen, optLen, rest) =>
          parser ! Parser.Parse(data, dataLen, optLen)
          context become running(operator, rest)
        case Incomplete(data) =>
          context become running(operator, data)
      }
      log.debug(s"Handling data: ${formatData(data ++ received)}")
  }

}
