package echo_server

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{TextMessage, BinaryMessage, Message}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Flow

import scala.io.StdIn

object Server extends App {

  implicit val actorSystem = ActorSystem("akka-system")
  implicit val flowMaterializer = ActorMaterializer()

  val config = actorSystem.settings.config
  val interface = config.getString("app.interface")
  val port = config.getInt("app.port")
  val route = EchoHTTP.route ~ EchoWS.route
  val binding = Http().bindAndHandle(route, interface, port)

  // don't close yet, wait for ENTER...
  Console.err.println("Press ENTER to stop...")
  StdIn.readLine()

  import actorSystem.dispatcher
  binding.flatMap(_.unbind()).onComplete(_ => actorSystem.shutdown())
  Console.err.println("Server shut down!")

}

/**
  * Define handlers external to the Server, otherwise classloader havoc...
  */
object EchoHTTP {
  def route: Route = path("echo-http") {
    complete("")
  }
}

object EchoWS {
  val echoFlow = Flow[Message].map {
    case msg: TextMessage.Strict => msg
    case msg: BinaryMessage.Strict => msg
    case x => TextMessage("Unsupported message type for " + x)
  }

  def route: Route = path("echo-ws") {
    get {
      handleWebsocketMessages(echoFlow)
    }
  }
}
