package tbje.enocean

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import ch.jodersky.flow.{ SerialSettings }
import akka.util.{ ByteString => BS, CompactByteString => CBS, Timeout }

object Controller {
  case object InfoRequest
  case object InfoRequestTimout
  case class SetRequest(id: Int, state: Int)

  def props(port: String, settings: SerialSettings): Props = Props(new Controller(port, settings))

  private val deviceId = CBS(0xff, 0xbe, 0x2f, 0x80)

  private val mappedDevices: Map[BS, (String, Int)] = Map(
    CBS(0xff,0x8a,0xc0,0x00) -> ("Bedroom" -> 1),
    CBS(0xff,0xbe,0x2f,0x80) -> ("Babyroom" -> 2),
    CBS(0xff,0xb0,0xe4,0x80) -> ("Bathroom" -> 3),
    CBS(0xff,0xe1,0x49,0x80) -> ("Kitchen" -> 4),
    CBS(0xff,0xd6,0x65,0x80) -> ("Livingroom" -> 5))

  private val fromId: Map[Int, (String, BS)] =
    mappedDevices.map{ case (a, (c,d)) => d -> (c -> a) }

}

class Controller(port: String, settings: SerialSettings) extends Actor with ActorLogging {

  private[this] def createRoom(name: String, id: Int) = context.actorOf(Room.props(name, id), s"room-${name.toLowerCase()}")

  import Controller._
  private var rooms: Map[Int, ActorRef] =
    mappedDevices map { case (address, (name, id)) =>
      id -> createRoom(name, id)
    }

  log.info("Starting controller")

  def createMessageOrganiser(parser: ActorRef) =
    context.actorOf(MessageOrganiser.props(port, settings, parser), "organiser")

  def createParser() =
    context.actorOf(Parser.props(mappedDevices, rooms), "parser")

  val parser = createParser()
  val organiser = createMessageOrganiser(parser)

  var roomReqSeq = 0

  import context.dispatcher
  import concurrent.duration._

  implicit val to = Timeout(1 second)
  override val receive: Receive = {
    case InfoRequest =>
      val sen = sender()
      createCollectorActor(sender(), rooms.map(_._2)(collection.breakOut), roomReqSeq)

    case SetRequest(id, state) if rooms.contains(id) =>
      rooms(id) forward Room.SetRequest(state)
  }

  def createCollectorActor(terminal: ActorRef, rooms: Seq[ActorRef], id: Int) =
    context.actorOf(Props(new Actor {
      val len = rooms.length
      var collected = Seq[Room.Info]()
      for(r <- rooms) r ! Room.InfoRequest(id)
      val timeout = context.system.scheduler.scheduleOnce(1 second, self, Controller.InfoRequestTimout)
      override val receive: Receive = {
        case msg @ Room.Info(name, state, `id`) =>
          collected = msg +: collected
          if (collected.length == len) {
            timeout.cancel
            terminal.tell(collected, context.parent)
            context stop self
          }
        case Controller.InfoRequestTimout =>
          terminal.tell(collected, context.parent)
            context stop self
      }
    }))

}
