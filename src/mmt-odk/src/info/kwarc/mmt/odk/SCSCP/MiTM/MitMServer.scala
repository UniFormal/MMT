package info.kwarc.mmt.odk.SCSCP.MiTM

import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.frontend.actions.ServerInfoAction.{controller, logGroup, respond}
import info.kwarc.mmt.api.frontend.actions._
import info.kwarc.mmt.api.objects.{OMA, OMID, Term}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.web.{ServerExtension, Util}
import info.kwarc.mmt.odk.OpenMath.Coding.OMMMTCoding
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.Protocol.SCSCPCallArguments
import info.kwarc.mmt.odk.SCSCP.Server._

import scala.concurrent.Future

/**
  * SCSCP server that implements the MitM protocol.
  */
class MitMServer extends Extension {
  override def logPrefix: String = "mitm"

  implicit val ec = scala.concurrent.ExecutionContext.global
  private var scscpServer: Option[SCSCPServer] = None

  /**
    * starts the server on a given hostname and port
    *
    * @param hostname
    * @param port
    */
  def startServer(hostname: String, port: Int): Unit = {
    if(scscpServer.nonEmpty){ throw new Exception("can not have multiple scscp servers")}

    try {
      val theServer = SCSCPServer("MitMServer", "1.0", "MitMServer", { s => report("scscp", s) }, host=hostname, port=port)
      log(s"Starting SCSCP Server on $hostname:$port")
      controller.extman.get(classOf[MiTMHandler]).foreach({h =>
        theServer.register(h.symbol, h)
      })
      scscpServer = Some(theServer)
    } catch {
      case e: java.net.BindException =>
        log("Can not start SCSCP Server: Port already taken")
        return
      case f: Exception =>
        log(s"SCSCP Server Error: $f")
        return
    }

    Future {
      try {
        scscpServer.get.processForever()
        log("SCSCP Server stopped")
        scscpServer = None
      } catch {
        case e: Exception =>
          logError("SCSCP Server Error: " + e)
      }

    }
  }

  /**
    * stops the SCSCP Server
    */
  def stopServer(): Unit = {
    scscpServer.foreach(_.quit(None))
  }

  override def start(args: List[String]): Unit = {

    // add all the action companions
    controller.extman.addExtension(SCSCPActions.SCSCPInfoActionCompanion)
    controller.extman.addExtension(SCSCPActions.SCSCPOnCompanion)
    controller.extman.addExtension(SCSCPActions.SCSCPOffCompanion)

    // add all the handlers
    controller.extman.addExtension(EvalHandler)

    // TODO: Do we still need this
    val database = new MitMDatabase(scscpServer.get)
    controller.extman.addExtension(database.GetAllServersHandler)
    controller.extman.addExtension(database.RegisterServerHandler)
    controller.extman.addExtension(database.RemoveServerHandler)
  }

  override def destroy: Unit = {
    // stop the server (if any)
    stopServer()

    // remove all our extensions
    val exts = controller.extman.get(classOf[MiTMExtension])
    exts.foreach(controller.extman.removeExtension(_))
  }

  /**
    * Actions to control the SCSCP server
    */
  object SCSCPActions {

    case object SCSCPInfoAction extends Action with ResponsiveAction {
      def apply() = scscpServer match {
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
      def apply() = scscpServer match {
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

      def parserActual(implicit state: ActionState) = ((int ~ (str ?))?) ^^ {
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
}


/** any extension that is related to MiTM and should be removed upon destroy */
trait MiTMExtension extends Extension


/** a handler for the MiTM Server */
abstract class MiTMHandler(val symbol: OMSymbol) extends MiTMExtension with SCSCPHandler

abstract class MMTHandler(override val symbol: OMSymbol) extends MiTMHandler(symbol) {
  /** a coding to decode / encode MMT terms */
  protected lazy val coding = new OMMMTCoding(URI(controller.getBase.toPath))

  def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
    val theTerm = OMApplication(symbol, parameters.toList, None, None)
    val theResult = eval(coding.encode(theTerm).asInstanceOf[OMA], client, arguments)
    coding.decode(theResult).asInstanceOf[OMExpression]
  }

  /** evaluates a single MMT term on the given handler */
  def eval(term: OMA, client: SCSCPServerClient, arguments: SCSCPCallArguments): Term
}