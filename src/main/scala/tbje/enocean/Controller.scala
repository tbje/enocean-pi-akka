package tbje.enocean

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import ch.jodersky.flow.{ SerialSettings }
import akka.util.{ ByteString => BS, CompactByteString => CBS, Timeout }

object Controller {
  case object InfoRequest
  case object InfoRequestTimout
  case class SetRequest(id: Int, state: Int)
  case class SerialOpened(operator: ActorRef)

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

class Controller(port: String, serialSettings: SerialSettings) extends Actor with ActorLogging with SettingsActor {
  import Controller._

  private[this] def createRoom(sender: ActorRef, name: String, id: Int) =
    context.actorOf(Room.props(sender, name, id), s"room-${name.toLowerCase()}")

  context.actorOf(Props(new Actor with ActorLogging {
    import ch.jodersky.flow.{ AccessDeniedException, Serial, SerialSettings }
    import akka.io.IO
    import context.system
    if(!settings.test) {
      IO(Serial) ! Serial.Open(port, serialSettings)
    } else {
      context.parent ! SerialOpened(context.system.deadLetters)
      context stop self
    }

    override val receive: Receive = {
      case Serial.CommandFailed(cmd: Serial.Open, reason: AccessDeniedException) =>
        log.error("You're not allowed to open that port!")
        context stop self
      case Serial.CommandFailed(cmd: Serial.Open, reason) =>
        log.error("Could not open port for some other reason: " + reason.getMessage)
        context stop self
      case Serial.Opened(settings) => {
        log.info("Port opened")
        context.parent ! SerialOpened(sender)
        context stop self
      }
    }
  }), "init-serial")

  log.info("Starting controller")

  def createMessageOrganiser(operator: ActorRef, parser: ActorRef) =
    context.actorOf(MessageOrganiser.props(operator, parser), "organiser")

  def createParser(rooms: Map[Int, ActorRef]) =
    context.actorOf(Parser.props(mappedDevices, rooms), "parser")

  var roomReqSeq = 0

  import context.dispatcher
  import concurrent.duration._

  private def createSender(operator: ActorRef) =
    context.actorOf(Sender.props(operator), "serial-sender")

  private[this] val init : Receive = {
    case SerialOpened(operator) =>
      val serialSender = createSender(operator)
      import Controller._
      val rooms: Map[Int, ActorRef] =
        mappedDevices map { case (address, (name, id)) =>
          id -> createRoom(serialSender, name, id)
        }
      val parser = createParser(rooms)
      val organiser = createMessageOrganiser(operator, parser)
      context become running(operator, parser, organiser, rooms)
  }

  implicit val to = Timeout(1 second)
  override val receive: Receive = init

  private[this] def running(operator: ActorRef, parser: ActorRef, organiser: ActorRef, rooms: Map[Int, ActorRef]): Receive = {
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
