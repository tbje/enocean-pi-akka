package tbje.enocean

import akka.actor.{ Actor, ActorLogging, Props }
import ch.jodersky.flow.{ SerialSettings }

object Controller {
  def props(port: String, settings: SerialSettings): Props = Props(new Controller(port, settings))
}

class Controller(port: String, settings: SerialSettings) extends Actor with ActorLogging {

  context.actorOf(MessageOrganiser.props(port, settings), "organiser")

  override val receive: Receive = Actor.emptyBehavior


}
