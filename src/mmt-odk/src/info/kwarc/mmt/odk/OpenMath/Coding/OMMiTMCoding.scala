package info.kwarc.mmt.odk.OpenMath.Coding

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{OMLIT, Term}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.MitM.MitM
import info.kwarc.mmt.odk.{IntegerLiterals, StringLiterals}
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.odk.SCSCP.Protocol.OpenMathError

/** an encoding of OpenMath objects used by everything MiTM */
class OMMiTMCoding(controller: Controller) extends OMMMTCoding(URI(controller.getBase.toPath)) {
  override def actencode(om: OMAny)(implicit toplevel: OMAny): Term = om match {
    case OMSymbol("false","logic1",_,_) => MitM.ff
    case OMSymbol("true","logic1",_,_) => MitM.tt
    case OMInteger(i,_) => IntegerLiterals.of(i)
    case OMString(s,_) => StringLiterals.of(s)
    case OMError(nm,params,tid,cdbase) =>
      throw OpenMathError(nm.name + ": " + params)
    case _ => super.actencode(om)
  }

  override def decodeAnyVal(t: Term): OMAnyVal = t match {
    case OMLIT(value : BigInt,_) => OMInteger(value,None)
    case OMLIT(value : String,_) => OMString(value,None)
    case OMLIT(value : Double,_) => OMFloat(value,None)
    case MitM.ff => OMSymbol("false","logic1",None,None)
    case MitM.tt => OMSymbol("true","logic1",None,None)
    case _ => super.decodeAnyVal(t)
  }
}
