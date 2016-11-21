package tbje.enocean

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import akka.util.{ ByteString => BS, CompactByteString => CBS }
import tbje.enocean.util._
import ch.jodersky.flow.Serial

object Sender {
  def props() = Props(new Sender())

  case class Message(command: Int, data: BS, opt: BS)

}

class Sender() extends Actor with ActorLogging {

  def createPacket(command: Int, data: BS, opt: BS): BS = {
    val header = fromInt(data.length) ++ CBS(opt.length, command)
    val headerCrc = Crc8(header)
    val body = data ++ opt
    val bodyCrc = Crc8(body)
    CBS(0x55) ++ header ++ CBS(headerCrc) ++ body ++ CBS(bodyCrc)
  }

  import Sender._

  private[this] def operational(operator: ActorRef): Receive = {
    case Message(command, data, opt) =>
      operator ! Serial.Write(createPacket(command, data, opt))
  }

  override def receive: Receive = {
    case Controller.SerialOpened(operator) =>
      context become operational(operator)
  }
}
