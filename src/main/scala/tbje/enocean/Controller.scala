package tbje.enocean

import akka.actor.{ Actor, ActorLogging, ActorRef, Props }
import akka.io.IO
import ch.jodersky.flow.{ Serial, SerialSettings, AccessDeniedException }

object Controller {
  def props(port: String, settings: SerialSettings): Props = Props(new Controller(port, settings))
}

class Controller(port: String, settings: SerialSettings) extends Actor with ActorLogging {

  import context._

  context.actorOf(MessageOrganiser.props(port, settings), "organiser")

  override val receive: Receive = Actor.emptyBehavior

}
