package tbje.enocean

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import akka.io.IO
import akka.util.ByteString
import ch.jodersky.flow.{ AccessDeniedException, Serial, SerialSettings }


object MessageOrganiser {
  def props(port: String, settings: SerialSettings): Props = Props(new Controller(port, settings))
}

class MessageOrganiser(port: String, settings: SerialSettings) extends Actor with ActorLogging{
  import context._
  IO(Serial) ! Serial.Open(port, settings)


  private[this] val init: Receive = {
    case Serial.CommandFailed(cmd: Serial.Open, reason: AccessDeniedException) =>
      println("You're not allowed to open that port!")
    case Serial.CommandFailed(cmd: Serial.Open, reason) =>
      println("Could not open port for some other reason: " + reason.getMessage)
    case Serial.Opened(settings) => {
      println("Port opened")
      context become running(sender)
    }
  }

  override val receive: Receive = init

  private def formatData(data: ByteString): String =
    (for {
      d <- data
    } yield f"0x$d%02x") mkString ""

  private[this] def running(operator: ActorRef): Receive = {
    case Serial.Received(data) =>
      log.info(s"Received data: ${formatData(data)}")
  }


}
