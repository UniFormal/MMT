package info.kwarc.mmt.MitM.Server

import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.objects.{OMA, Term}
import info.kwarc.mmt.odk.OpenMath.Coding.OMMiTMCoding
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.Protocol.SCSCPCallArguments
import info.kwarc.mmt.odk.SCSCP.Server._

import scala.concurrent.Future

/**
  * SCSCP server that implements the MitM protocol.
  */
class Server extends Extension with Actions {
  override def logPrefix: String = "mitmserver"

  implicit val ec = scala.concurrent.ExecutionContext.global
  var scscpServer: Option[SCSCPServer] = None

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
    controller.extman.addExtension(SCSCPInfoActionCompanion)
    controller.extman.addExtension(SCSCPOnCompanion)
    controller.extman.addExtension(SCSCPOffCompanion)

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

}


/** any extension that is related to MiTM and should be removed upon destroy */
trait MiTMExtension extends Extension

/** a handler for the MiTM Server */
abstract class MiTMHandler(val symbol: OMSymbol) extends MiTMExtension with SCSCPHandler

abstract class MMTHandler(override val symbol: OMSymbol) extends MiTMHandler(symbol) {
  /** a coding to decode / encode MMT terms */
  protected lazy val coding = new OMMiTMCoding(controller)

  def handle(client: SCSCPServerClient, arguments: SCSCPCallArguments, parameters: OMExpression*): OMExpression = {
    val theTerm = OMApplication(symbol, parameters.toList, None, None)
    val theResult = eval(coding.encode(theTerm).asInstanceOf[OMA], client, arguments)
    coding.decode(theResult).asInstanceOf[OMExpression]
  }

  /** evaluates a single MMT term on the given handler */
  def eval(term: OMA, client: SCSCPServerClient, arguments: SCSCPCallArguments): Term
}