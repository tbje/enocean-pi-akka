package tbje.enocean

import akka.actor.{ Actor, ActorSystem }

object Settings {
  def apply(system: ActorSystem) = new Settings(system)
}

class Settings(system: ActorSystem) {

  val test: Boolean =
    system.settings.config.getBoolean("temp-control.test")

}

trait SettingsActor { _: Actor =>
  val settings = new Settings(context.system)
}
