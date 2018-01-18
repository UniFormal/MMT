package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.web.{Server, ServerExtension, Util}

/** Shared base class for Actions for controlling the webserver */
sealed abstract class ServerAction extends ActionImpl {
  protected def server(implicit controller: Controller): Option[Server] = controller.server
  protected def server_=(server: Option[Server])(implicit controller: Controller) = controller.server = server
}

case object ServerInfoAction extends ServerAction with ResponsiveAction {
  def apply(implicit controller: Controller) = server match {
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
  def toParseString: String = "show server"
}
object ServerInfoActionCompanion extends ActionObjectCompanionImpl[ServerInfoAction.type]("get information about the currently running server", "show server")

case class ServerOn(port: Int, bindHost : String = "127.0.0.1") extends ServerAction {
  def apply(implicit controller: Controller) : Unit = server match {
    case Some(serv) => logError("server already started on  " + serv.bindHost + ":" + serv.port)
    case None if Util.isTaken(port) => logError("port " + port + " is taken, server not started.")
    case _ =>
      val serv = new Server(port, bindHost, controller)
      serv.start
      log("Server started at http://" + bindHost + ":" + port)
      server = Some(serv)
  }
  def toParseString = s"server on $port${if(bindHost == "127.0.0.1") "" else " " + bindHost}"
}
object ServerOnCompanion extends ActionCompanionImpl[ServerOn]("start up the HTTP server", "server on") {
  import Action._
  def parserActual(implicit state: ActionState) = (int ~ (str?)) ^^ {
    case i ~ None => ServerOn(i)
    case i ~ Some(s) => ServerOn(i, s)
  }
}

case object ServerOff extends ServerAction {
  def apply(implicit controller: Controller) : Unit = {
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
object ServerOffCompanion extends ActionObjectCompanionImpl[ServerOff.type]("shut down the web server", "server off")