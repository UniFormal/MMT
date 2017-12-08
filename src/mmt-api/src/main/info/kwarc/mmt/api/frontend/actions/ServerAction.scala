package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.web.{Server, Util}

/** Shared base class for Actions for controlling the webserver */
sealed abstract class ServerAction extends ActionImpl {}

/** start up the HTTP server
  *
  * concrete syntax: server on port:INT [hostname:STRING]
  *
  * See info.kwarc.mmt.api.web.Server for the supported HTTP requests.
  * tiscaf.jar must be on the Java classpath before executing this Action.
  *
  * @param hostname the hostname (i.e. ip) to listen to. Defaults to 0.0.0.0 (everything) for backwards compatibility.
  * @param port the port to listen to
  */
case class ServerOn(port: Int, hostname : String = "0.0.0.0") extends ServerAction {
  def apply(controller: Controller) : Unit = {

    def logError(msg: String) = controller.report("error", msg)
    def log(msg: String) = controller.report("controller", msg)

    controller.server match {
      case Some(serv) => logError("server already started on  " + serv.hostname + ":" + serv.port)
      case None if Util.isTaken(port) => logError("port " + port + " is taken, server not started.")
      case _ =>
        val serv = new Server(port, hostname, controller)
        serv.start
        log("Server started at http://" + hostname + ":" + port)
        controller.server = Some(serv)
    }
  }
  def toParseString = s"server on $port ${if(hostname == "0.0.0.0") "" else " " + hostname}"
}
object ServerOnCompanion extends ActionCompanionImpl[ServerOn]("start up the HTTP server", "server on") {
  import Action._
  def parserActual(implicit state: ActionState) = (int ~ (str?)) ^^ {
    case i ~ None => ServerOn(i)
    case i ~ Some(s) => ServerOn(i, s)
  }
}

/** shut down the web server
  *
  * concrete syntax: server off
  */
case object ServerOff extends ServerAction {
  def apply(controller: Controller) : Unit = {

    def log(msg: String) = controller.report("controller", msg)

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