package info.kwarc.mmt.MitM.Server

import info.kwarc.mmt.api.objects.{OMA, Term}
import info.kwarc.mmt.odk.OpenMath.OMSymbol
import info.kwarc.mmt.odk.SCSCP.Protocol.SCSCPCallArguments
import info.kwarc.mmt.odk.SCSCP.Server.SCSCPServerClient

import info.kwarc.mmt.MitM.VRESystem._

/** 
 *  turns [[MitMComputation]] into an SCSCP handler that is then used in [[MitMComputationServer]]
 */
object EvalHandler extends MitMHandler(OMSymbol("mitmEval", "mitm_transient", None, None)) {
  val min: Int = 1
  val max: Int = 1
  val signature = None

  private lazy val mitmComp = new MitMComputation(controller)

  /** handles a single call to the SCSCP Handler */
  def eval(term: OMA, client: SCSCPServerClient, arguments: SCSCPCallArguments): Term = {
    log(s"client ${client.identifier} -> MiTM: ${term.toString}")
    implicit val trace = new MitMComputationTrace(false)
    val result = mitmComp.simplify(term.args.head, None)
    log(s"MitM -> client ${client.identifier}: ${result.toString}")
    result
  }
}
