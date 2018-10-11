package info.kwarc.mmt.MitM.Server

import info.kwarc.mmt.api.frontend.actions._
import info.kwarc.mmt.api.web.Util

/**
  * Actions controlling the MiTM SCSCP Server
  */
trait Actions {
  this: Server =>

  case object SCSCPInfoAction extends Action with ResponsiveAction {
    def apply(): Unit = scscpServer match {
      case None => respond("no SCSCP server active")
      case Some(s) => {
        respond(s"SCSCP Server listening on ${s.hostname}:${s.port}")
        respond(s"Registered HEADs: ")
        logGroup {
          s.getHandlerNames.foreach { hn =>
            log(s"$hn => ${s.getHandler(hn).getClass.toString}")
          }
        }
      }
    }

    def toParseString: String = "show scscp"
  }

  object SCSCPInfoActionCompanion extends ObjectActionCompanion(SCSCPInfoAction, "get information about the currently running SCSCP server", "show scscp")
    with MiTMExtension

  case class SCSCPOn(port: Int = 26133, bindHost: String = "127.0.0.1") extends Action {
    def apply(): Unit = scscpServer match {
      case Some(s) =>
        logError("SCSCP server already started on  " + s.hostname + ":" + s.port)
      case None if Util.isTaken(port) =>
        logError("port " + port + " is taken, SCSCP server not started.")
      case _ =>
        startServer(bindHost, port)
    }

    def toParseString = s"scscp on $port${if (bindHost == "127.0.0.1") "" else " " + bindHost}"
  }

  object SCSCPOnCompanion extends ActionCompanion("start up the SCSCP server", "scscp on") with MiTMExtension {

    import Action._

    def parserActual(implicit state: ActionState) = ((int ~ (str ?)) ?) ^^ {
      case None => SCSCPOn()
      case Some(i ~ None) => SCSCPOn(i)
      case Some(i ~ Some(s)) => SCSCPOn(i, s)
    }
  }

  case object SCSCPOff extends Action {
    def apply() = scscpServer match {
      case Some(serv) => stopServer()
      case None => log("SCSCP not running")
    }

    def toParseString = "scscp off"
  }

  object SCSCPOffCompanion extends ObjectActionCompanion(SCSCPOff, "shut down the SCSCP server", "scscp off")
    with MiTMExtension

}
