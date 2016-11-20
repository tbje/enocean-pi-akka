package tbje.enocean

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import akka.util.{ ByteString => BS, CompactByteString => CBS }
import ch.jodersky.flow.Serial
import util._

object MessageOrganiser {
  def props(operator: ActorRef, parser: ActorRef): Props = Props(new MessageOrganiser(operator, parser))

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

class MessageOrganiser(operator: ActorRef, parser: ActorRef) extends Actor with ActorLogging with SettingsActor {
  import context._
  import MessageOrganiser._

  import util.DSL._
  private def formatData(data: BS): String = data.hex

  override def receive: Receive = running()

  private[this] def running(data: BS = BS.empty): Receive = {
    case Serial.Received(received) =>
      MessageOrganiser.checkData(data ++ received) match {
        case Complete(data, dataLen, optLen) =>
          parser ! Parser.Parse(data, dataLen, optLen)
          context become running()
        case TooLong(data, dataLen, optLen, rest) =>
          parser ! Parser.Parse(data, dataLen, optLen)
          context become running(rest)
        case Incomplete(data) =>
          context become running(data)
      }
      println(s"Received data: ${formatData(data)}")
  }

}
