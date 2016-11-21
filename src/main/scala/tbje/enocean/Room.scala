package tbje.enocean

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import akka.util.{ ByteString => BS, CompactByteString => CBS }
import com.github.nscala_time.time.Imports._

class Room(serialSender: ActorRef, name: String, roomId: Int) extends Actor with ActorLogging {
  import Room._

  var hist: List[(DateTime, Parser.ParseResult)] = List()

  var state = 50

  override val receive: Receive = {
    case Room.HistRequest(id) =>
      sender ! Room.Hist(name, roomId, hist)
    case Room.InfoRequest(id) =>
      sender ! Room.Info(id, name, state, roomId, hist.headOption.map(_._1))
    case Room.SetRequest(newState) =>
      state = newState
      sender ! Room.SetReply.OK(name, state)
    case msg @ Parser.Learn(sender, _, rorg, func, eeptype, manufacturer, learnType, learnStatus) =>
      hist = ((DateTime.now -> msg) :: hist).take(20)
      serialSender ! createLearnMessage(roomId)
    case msg @ Parser.IRTV(sender: BS, dest: BS, valvePos: Int, eneryHarvesting: Boolean, sufficienEnergy: Boolean, dbm: Int, intTemp: Int) =>
      hist = ((DateTime.now -> msg) :: hist).take(20)
      serialSender ! createMessage(state, roomId)
    case other => log.debug(other.toString())
  }
}

object Room {
  case class InfoRequest(id: Int)

  case class Info(id: Int, name: String, state: Int, roomId: Int, lastSeen: Option[DateTime])

  case class HistRequest(id: Int)

  case class Hist(name: String, id: Int, hist: List[(DateTime, Parser.ParseResult)])

  case class SetRequest(state: Int)

  sealed trait SetReply
  import util.DSL._

  private[this] val baseId = CBS(0xff, 0xbe, 0x2f, 0x80)

  private[this] val broadcast  = CBS(0xff, 0xff, 0xff, 0xff)

  private def createMessage(valvePos: Int, id: Int): Sender.Message = {
    val db0NoLearn  = bit"00001000"
    val db1Summer   = bit"00001000"
    val db1Regular  = bit"00000000"
    val db1 = db1Regular
    val temp = 22
    val db2 = temp * 255 / 20
    val db3 = valvePos
    val dbs = CBS(db3, db2, db1, db0NoLearn)
    val sender = baseId.take(3) ++ CBS(baseId(3)+ id)
    val dbm = 0xff
    val sec = 0x00
    val status = 0x00
    val data = CBS(0xa5) ++ dbs ++ sender ++ CBS(status)
    val opt = CBS(0x01) ++ broadcast ++ CBS(dbm, sec)
    Sender.Message(0x01, data, opt)
  }

  private def createLearnMessage(id: Int): Sender.Message = {
    val db0Learn    = bit"11110000"
    // taken from data sent from micropelt
    val dbs = CBS(0x54, 0xa0, 0x0f, db0Learn)
    val sender = baseId.take(3) ++ CBS(baseId(3)+ id)
    val dbm = 0xff
    val sec = 0x00
    val status = 0x00
    val data = CBS(0xa5) ++ dbs ++ sender ++ CBS(status)
    val opt = CBS(0x01) ++ broadcast ++ CBS(dbm, sec)
    Sender.Message(0x01, data, opt)
  }

  object SetReply {
    case class OK(name: String, state: Int) extends SetReply
    case class Failure(name: String, reason: String) extends SetReply
  }

  def props(serialSender: ActorRef, name: String, id: Int): Props = Props(new Room(serialSender, name, id))
}
