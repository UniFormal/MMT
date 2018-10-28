package info.kwarc.mmt.MitM.Server

import info.kwarc.mmt.api.objects.{OMA, OMID, Term}
import info.kwarc.mmt.odk.OpenMath.OMSymbol
import info.kwarc.mmt.odk.SCSCP.Protocol.SCSCPCallArguments
import info.kwarc.mmt.odk.SCSCP.Server.SCSCPServerClient
import info.kwarc.mmt.MitM.VRESystem._
import info.kwarc.mmt.api.GlobalName

/** 
 *  turns [[MitMComputation]] into an SCSCP handler that is then used in [[MitMComputationServer]]
 */
object EvalHandler extends MitMHandler(OMSymbol("mitmEval", "mitm_transient", None, None)) {
  val min: Int = 1
  val max: Int = 1
  val signature = None

  private lazy val mitmComp = new MitMComputation(controller)

  /** finds the system and term to be applied here */
  def matchSystem(term: OMA): (Term, Option[VREWithAlignmentAndSCSCP]) = term match {
    case OMA(_, List(comp)) => (comp, None)
    case OMA(_, List(comp, OMID(symbol: GlobalName))) => (comp, controller.extman.get(classOf[VREWithAlignmentAndSCSCP]).find(_.sym == symbol))
  }

  /** handles a single call to the SCSCP Handler */
  def eval(term: OMA, client: SCSCPServerClient, arguments: SCSCPCallArguments): Term = {
    val (query, system) = matchSystem(term)
    val theQuery = if(system.isDefined) system.get.translateToMitM(query) else query
    log(s"client ${client.identifier} -> MiTM: ${term.toString}")
    implicit val trace = new MitMComputationTrace(None)
    val result = mitmComp.simplify(theQuery, None)
    log(s"MitM -> client ${client.identifier}: ${result.toString}")
    if (system.isDefined) system.get.translateToSystem(result) else result
  }
}
