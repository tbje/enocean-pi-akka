package tbje.enocean

import akka.actor.ActorSystem
import akka.event.Logging
import ch.jodersky.flow.{ Parity, SerialSettings }
import annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.StdIn

object TempControl {
  def main(args: Array[String]): Unit = {
    val settings = SerialSettings(57600, 8, false, Parity(0))
    val system = ActorSystem("temp-control")
    system.actorOf(Controller.props("/dev/ttyAMA0", settings), name = "controller")
    new TempControl(system).run
  }
}

class TempControl(system: ActorSystem) extends CommandLineInterface {

  private val log = Logging(system, getClass.getName)

  private def console(x: Any) = println(x)

  @tailrec
  private def commandLoop(): Unit =
    Command(StdIn.readLine()) match {
      case Command.Quit =>
        system.terminate()
      case Command.Help =>
        console(f"%nq to quit, h for help")
        commandLoop()
      case Command.Unknown(command) =>
        console(s"Unknown command ${command}!")
        commandLoop()
    }

  def run(): Unit = {
    console(f"${system.name} running%nEnter "
      + Console.YELLOW + "commands" + Console.RESET
      + " into the terminal: "
      + Console.YELLOW + "[e.g. `q` or `quit`]" + Console.RESET)
    commandLoop()
    Await.ready(system.whenTerminated, Duration.Inf)
  }


}
