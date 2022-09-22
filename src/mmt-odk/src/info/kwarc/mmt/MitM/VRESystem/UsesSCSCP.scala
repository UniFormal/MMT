package info.kwarc.mmt.MitM.VRESystem

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.web.ServerError
import info.kwarc.mmt.odk.OpenMath.Coding.OMMiTMCoding
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient
import info.kwarc.mmt.odk.SCSCP.Protocol._

/** A trait used for systems that use scscp
 * 
 * The location is obtained from the MMT config entry ForeingConf("mitm", id, host::port::Nil), see [[SCSCPLocation]] for default values.
 */
trait UsesSCSCP extends VRESystem {
  
  /** the head to wrap all calls to SCSCP in */
  val head: OMSymbol

  // will be defined after running start
  private var location: SCSCPLocation = null
  
  /** get the location of the scscp server from the MMT configuration */
  override def start(args: List[String]): Unit = {
    super.start(args)
    val entry = controller.getConfig.getForeignEntries("mitm").find(_.key == id)
    location = entry match {
      case Some(frontend.ForeignConf(_, _, values)) => values match {
        case host::portS::Nil =>
          val port = utils.stringToInt(portS).getOrElse {throw LocalError("port must be numeric, found " + portS)}
          SCSCPLocation(Some(host), Some(port))
        case host:: Nil => SCSCPLocation(Some(host), None)
        case Nil => SCSCPLocation(None, None)
        case _ => throw LocalError("at most two arguments expected, found " + values.mkString(", "))        
      }
      case _ =>
        logError("no config entry found for " + id + ", using default values")
        SCSCPLocation(None, None)
    }
    log(s"starting $id at $location")
  }

  /** generates a new call id for SCSCP */
  private def newCallId = s"$id:${java.lang.System.currentTimeMillis().toString}:${scala.util.Random.nextInt().toString}"

  /** the coding used to encode / decode MiTM objects */
  private lazy val coding = new OMMiTMCoding(controller)

  /** makes a call to scscp */
  def scscpcall(t: Term)(implicit trace: MitMComputationTrace): Term = {
    val client = try {
      SCSCPClient(location.host, location.port)
    } catch {
      case e : java.net.ConnectException =>
        val err = LocalError("error initializing server, returning original term unchanged")
        err.setCausedBy(Error(e))
        trace += MitMFailure(err)
        return t
    }
    val ret = try {
      val om = coding.decodeExpression(t)
      trace += SCSCPSend(id, t, om)
      val call = new SCSCPCall(head, SCSCPCallArguments(newCallId, Some(SCSCPReturnObject), null), om)
      val res = client(call).fetch() match {
        case SCSCPObjectReturned(r, _) => r
        case SCSCPTerminated(e, _) => throw LocalError("SCSCP call Error: "+e.toString)
        case _ => throw LocalError("SCSCP Call returned unexpected result")

      }
      res
    } catch {
      case e: Exception =>
        client.quit(Some("exception occurred"))
        val err = LocalError("SCSCP call failed, throwing exception").setCausedBy(e)
        trace += MitMFailure(err)
        throw err
    }
    val mitm = coding.encode(ret)
    trace += SCSCPReceive(id, ret, mitm)
    mitm

  }
}

 
case class SCSCPLocation(hostO: Option[String], portO: Option[Int]) {
  def host = hostO getOrElse "localhost"
  def port = portO getOrElse 1473
  override def toString = s"$host:$port"
}

case class SCSCPSend(system: String, in: Term, sent: OMAny) extends MitMComputationStep {
  val header = s"SCSCP call to $system" 
  val terms = List("in" -> in, "sent" -> sent)
}

case class SCSCPReceive(system: String, received: OMAny, out: Term) extends MitMComputationStep {
  val header = "SCSCP call returned"
  val terms = List("received" -> received, "out" -> out)
}