package tbje.enocean

import akka.actor.{ Actor, ActorLogging }
import akka.util.{ ByteString => BS, CompactByteString => CBS }
import tbje.enocean.util._

class Sender extends Actor with ActorLogging {

  def createPacket(command: Int, data: BS, opt: BS): BS = {
    val header = fromInt(data.length) ++ CBS(opt.length, command)
    val headerCrc = Crc8(header)
    val body = data ++ opt
    val bodyCrc = Crc8(body)
    CBS(0x55) ++ header ++ CBS(headerCrc) ++ body ++ CBS(bodyCrc)
  }

  override def receive: Receive = Actor.emptyBehavior

}
