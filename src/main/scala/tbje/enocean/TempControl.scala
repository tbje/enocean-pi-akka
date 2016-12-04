package tbje.enocean

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.event.Logging
import akka.util.Timeout
import ch.jodersky.flow.{ Parity, SerialSettings }
import annotation.tailrec
import org.slf4j.{LoggerFactory}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.StdIn
import scala.util.Success

object TempControl {
  def main(args: Array[String]): Unit = {
    val settings = SerialSettings(57600, 8, false, Parity(0))
    val system = ActorSystem("temp-control")
    val controller = system.actorOf(Controller.props("/dev/ttyAMA0", settings), name = "controller")
    new TempControl(system, controller).run
  }
}

class TempControl(system: ActorSystem, controller: ActorRef) extends CommandLineInterface {

  val context = LoggerFactory.getILoggerFactory() match {
    case context: ch.qos.logback.classic.LoggerContext =>
      context.getLoggerList().foreach(l =>
        l.getLogger().foreach { logger =>
          logger.iteratorForAppenders() foreach { appender =>
            println(appender)
          }
        }
      )
  }

  private val log = Logging(system, getClass.getName)

  private def console(x: Any) = println(x)

  import util.TerminalColor._

  import akka.pattern.ask

  implicit val timeout = Timeout(1 second)
  import system.dispatcher

  val web = new WebServer(controller)

  @tailrec
  private def commandLoop(): Unit =
    Command(StdIn.readLine()) match {
      case Command.List =>
        (controller ? Controller.InfoRequest).mapTo[Seq[Room.Info]] onComplete {
          case Success(x) =>
            if (x.isEmpty) console(s"No rooms replied")
              x foreach { case Room.Info(_, name, state, roomId, lastSeen) =>
                val lastSeenStr = lastSeen.map(x => cyan"${x.toString("hh:mm:ss")}").getOrElse("never seen")
                console(s"Room ${red"$roomId"} ${green"$name"} at ${yellow"$state%"} - $lastSeenStr.")
              }
          case _ => console("Failed ...")
        }
        commandLoop()
      case Command.History(room) =>
        (controller ? Room.HistRequest(room)).mapTo[Room.Hist] onComplete {
          case Success(Room.Hist(name, roomId, elements)) =>
            console(s"Room ${red"$roomId"} ${green"$name"}:")
            elements.foreach { case (time, msg) =>
              console(s"  ${red"${time.toString("hh:mm:ss")}"} ${green"$msg"}")
            }
          case _ => console("Failed ...")
        }
          commandLoop()
      case Command.Set(room, pct) =>
        console(s"Setting room ${magenta"$room"} to ${yellow"$pct%"}.")
          (controller ? Controller.SetRequest(room, pct)).mapTo[Room.SetReply] onComplete {
            case Success(Room.SetReply.OK(name, state)) =>
              console(s"Room ${green"$name"} set to ${yellow"$pct%"}.")
            case Success(Room.SetReply.Failure(name, error)) =>
              console(s"Not able to set room ${red"$name"} set to ${yellow"$pct%"}, $error.")
            case _ => console("Failed ...")
          }
        commandLoop()
      case Command.Quit =>
        console(red"Shutting down...")
      case Command.Help =>
        console("Help:\n" +
          f"${yellow"s"}${magenta"|"}${yellow"set"} [${magenta"room"}] [${green"opening"}] - instruct ${magenta"room"} to open valve at ${green"opening"}. i.e. ${yellow"set"} ${magenta"2"} ${green"75"}%n" +
          f"${yellow"l"}${magenta"|"}${yellow"list"} to ${green"list rooms"}%n" +
          f"${yellow"hi"}${magenta"|"}${yellow"hist"} [${magenta"room"}] - history for room ${magenta"room"}%n" +
          f"${yellow"q"}${magenta"|"}${yellow"quit"} to ${green"quit"}%n" +
          f"${yellow"he"}${magenta"|"}${yellow"help"} to display this screen.")
        commandLoop()
      case Command.Unknown(command) =>
        console(s"Unknown command ${command}!")
        commandLoop()
  }

  def run(): Unit = {
    console(f"${magenta"${system.name}"} running and ready for your input: "
      + s"`${yellow"q"}` or `${yellow"he"}`")
    commandLoop()
    web.stop()
    system.terminate()
    Await.ready(system.whenTerminated, Duration.Inf)
  }



}
