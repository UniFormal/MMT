package info.kwarc.mmt.MiTM.VRESystem

import info.kwarc.mmt.MiTM.Config.MiTMSystemLocation
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.odk.OpenMath.Coding.OMMiTMCoding
import info.kwarc.mmt.odk.OpenMath.OMSymbol
import info.kwarc.mmt.odk.SCSCP.Client.SCSCPClient
import info.kwarc.mmt.odk.SCSCP.Protocol.{SCSCPCall, SCSCPCallArguments, SCSCPReturnObject}

/** A trait used for systems that use scscp */
trait UsesSCSCP { this : VRESystem =>

  /** the head to wrap all calls to SCSCP in */
  val head: OMSymbol
  /** the location of the scscp server */
  def location: MiTMSystemLocation

  /** generates a new call id for SCSCP */
  private def newCallId = s"$id:${java.lang.System.currentTimeMillis().toString}:${scala.util.Random.nextInt().toString}"

  /** the coding used to encode / decode MiTM objects */
  protected lazy val coding = new OMMiTMCoding(controller)

  /** makes a call to scscp */
  def scscpcall(t : Term) : Term = {
    val client = SCSCPClient(location.hostname,location.port)
    val ret = try {
      val call = new SCSCPCall(head, SCSCPCallArguments(newCallId, Some(SCSCPReturnObject), null), coding.decodeExpression(t))
      client(call).fetchExpression()
    } catch {
      case e: Exception =>
        client.quit(Some("Exception occurred"))
        throw e
    }
    coding.encode(ret)
  }
}
