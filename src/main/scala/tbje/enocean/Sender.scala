package tbje.enocean

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import akka.util.{ ByteString => BS, CompactByteString => CBS }
import tbje.enocean.util._

object Sender {
  def props(operator: ActorRef) = Props(new Sender(operator))

  case class Message(command: Int, data: BS, opt: BS)

}

class Sender(operator: ActorRef) extends Actor with ActorLogging {

  def createPacket(command: Int, data: BS, opt: BS): BS = {
    val header = fromInt(data.length) ++ CBS(opt.length, command)
    val headerCrc = Crc8(header)
    val body = data ++ opt
    val bodyCrc = Crc8(body)
    CBS(0x55) ++ header ++ CBS(headerCrc) ++ body ++ CBS(bodyCrc)
  }

  import Sender._

  override def receive: Receive = {
    case Message(command, data, opt) =>
      operator ! createPacket(command, data, opt)
  }
}
