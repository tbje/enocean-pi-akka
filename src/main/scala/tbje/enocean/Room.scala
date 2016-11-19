package tbje.enocean

import akka.actor.{ Actor, ActorLogging, Props }


class Room(name: String, id: Int) extends Actor with ActorLogging {

  var state = 50

  override val receive: Receive = {
    case Room.InfoRequest(id) =>
      sender ! Room.Info(name, state, id)
    case Room.SetRequest(newState) =>
      state = newState
      sender ! Room.SetReply.OK(name, state)
    case other => log.debug(other.toString())
  }
}

object Room {
  case class InfoRequest(id: Int)

  case class Info(name: String, state: Int, id: Int)

  case class SetRequest(state: Int)

  sealed trait SetReply

  object SetReply {
    case class OK(name: String, state: Int) extends SetReply
    case class Failure(name: String, reason: String) extends SetReply
  }

  def props(name: String, id: Int): Props = Props(new Room(name, id))
}
