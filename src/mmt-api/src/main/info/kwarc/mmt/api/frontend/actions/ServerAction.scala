package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.web.{Server, ServerExtension, Util}

/** Shared base class for Actions for controlling the webserver */
sealed abstract class ServerAction extends Action

case object ServerInfoAction extends ServerAction with ResponsiveAction {
  def apply() = {
    controller.server match {
      case None => respond("no server active")
      case Some(s) => {
        respond(s"Server listening on http://${s.bindHost}:${s.port}")
        logGroup {
          controller.extman.get(classOf[ServerExtension]).foreach {se =>
            respond(s"/:${se.pathPrefix}/ => ${se.getClass.getName}")
          }
        }
      }
    }
  }
  def toParseString: String = "show server"
}
object ServerInfoActionCompanion extends ObjectActionCompanion(ServerInfoAction, "get information about the currently running server", "show server")

case class ServerOn(port: Int = 8080, bindHost : String = "127.0.0.1") extends ServerAction {
  def apply(): Unit = {
    controller.server match {
      case Some(serv) => logError("server already started on  " + serv.bindHost + ":" + serv.port)
      case None if Util.isTaken(port) => logError("port " + port + " is taken, server not started.")
      case _ =>
        val serv = new Server(port, bindHost, controller)
        serv.start
        log("Server started at http://" + bindHost + ":" + port)
        controller.server = Some(serv)
    }
  }
  def toParseString = s"server on $port${if(bindHost == "127.0.0.1") "" else " " + bindHost}"
}
object ServerOnCompanion extends ActionCompanion("start up the HTTP server", "server on") {
  import Action._
  def parserActual(implicit state: ActionState) = ((int ~ (str?))?) ^^ {
    case Some(i ~ None) => ServerOn(i)
    case Some(i ~ Some(s)) => ServerOn(i, s)
    case None => ServerOn()
  }
}

case object ServerOff extends ServerAction {
  def apply(): Unit = {
    controller.server match {
      case Some(serv) =>
        serv.stop
        log("Server stopped")
        controller.server = None
      case None => log("server not running")
    }
  }
  def toParseString = "server off"
}
object ServerOffCompanion extends ObjectActionCompanion(ServerOff, "shut down the web server", "server off")
