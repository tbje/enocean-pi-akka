package tbje.enocean

import akka.actor.{ ActorRef, ActorSystem }
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._

import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.pattern.ask
import scala.concurrent.duration._

class WebServer(controller: ActorRef) {
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher
  implicit val timeout = akka.util.Timeout(4 seconds)
  private val log = Logging(system, getClass.getName)

  val settings = Settings(system)

  val route =
    pathSingleSlash {
      get {
        val req = (controller ? Controller.InfoRequest).mapTo[Seq[Room.Info]] map { x =>
          if (x.isEmpty) "<h1>No rooms replied</h1>"
          x.sortBy(_.roomId).foldLeft("<h1>Rooms overview:</h1>") { case (acc, Room.Info(_, name, state, roomId, lastSeen)) =>
            val lastSeenStr = lastSeen.map(x => x.toString("hh:mm:ss")).getOrElse("never seen")
            acc + s"Room $roomId $name at $state% - $lastSeenStr<br/>"
          }
        }
        val resp = req.map{resp => HttpEntity(ContentTypes.`text/html(UTF-8)`, resp)}
        complete(resp)
      }
    }

  val bindingFuture = Http().bindAndHandle(route, settings.ip, 8080)

  def stop() = bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ => system.terminate()) // and shutdown when done
}
