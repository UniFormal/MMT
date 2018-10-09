package info.kwarc.mmt.odk.SCSCP.MiTM

import info.kwarc.mmt.api.objects.{OMA, Term}
import info.kwarc.mmt.odk.OpenMath.OMSymbol
import info.kwarc.mmt.odk.SCSCP.Protocol.SCSCPCallArguments
import info.kwarc.mmt.odk.SCSCP.Server.SCSCPServerClient

object EvalHandler extends MMTHandler(OMSymbol("mitmEval", "mitm_transient", None, None)) {
  val min: Int = 1
  val max: Int = 1
  val signature = None
  /** the odk plugin, so that we can interact and call it */
  protected lazy val odkPlugin: info.kwarc.mmt.odk.Plugin = controller.extman.get(classOf[info.kwarc.mmt.odk.Plugin]).head

  /** handles a single call to the SCSCP Handler */
  def eval(term: OMA, client: SCSCPServerClient, arguments: SCSCPCallArguments): Term = {
    log(s"client ${client.identifier} -> MiTM: ${term.toString}")
    val result = odkPlugin.simplify(term, None)
    log(s"MitM -> client ${client.identifier}: ${result.toString}")
    result
  }
}