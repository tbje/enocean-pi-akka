package tbje.enocean

import akka.actor.{ Actor, ActorLogging, Props }
import akka.util.{ ByteString => BS, CompactByteString => CBS }
import tbje.enocean.util._

object Sender {
  def props() = Props(new Sender())

  case class Message(command: Int, data: BS, opt: BS)
}

class Sender extends Actor with ActorLogging {

  def createPacket(command: Int, data: BS, opt: BS): BS = {
    val header = fromInt(data.length) ++ CBS(opt.length, command)
    val headerCrc = Crc8(header)
    val body = data ++ opt
    val bodyCrc = Crc8(body)
    CBS(0x55) ++ header ++ CBS(headerCrc) ++ body ++ CBS(bodyCrc)
  }

  import Sender._

  override def receive: Receive = {
    case Message(command, data, opt) => createPacket(command, data, opt)
  }
}
