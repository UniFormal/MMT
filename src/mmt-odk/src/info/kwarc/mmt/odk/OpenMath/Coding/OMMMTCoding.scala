package info.kwarc.mmt.odk.OpenMath.Coding
import info.kwarc.mmt.api.{LocalName, MPath, NamespaceMap, Path}
import info.kwarc.mmt.odk.OpenMath._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.OMLiteral._

/**
  * Decode / Encode MMT Terms as OpenMath objects.
  */
class OMMMTCoding(default : MPath = Path.parseM("http://cds.omdoc.org/?Default",NamespaceMap.empty)) extends OMCoding[Term] {
  def encode(om : OMAny) : Term = om match {
    case OMObject(expr,_,_,_) => encode(expr)
    case OMReference(href,id) => ???
    case OMInteger(i,id) => OMI(i)
    case OMFloat(r,id) => OMF(r)
    case OMString(s,id) => OMSTR(s)
    case OMBytes(list,id) => ???
    case OMSymbol(name,cd,id,cdbase) => OMS(Path.parseM(cd,NamespaceMap.empty) ? name)
    case OMVariable(name,id) => OMV(name)
    case OMForeign(any,encoding,id,cdbase) => ???
    case OMApplication(f,args,id,cdbase) => OMA(encode(f),args map encode)
    case OMAttribution(OMAttributionPairs(pairslist,inner_id,inner_cdbase),expr,id,cdbase) => ???
    case OMBinding(binder,OMBindVariables(vars,inner_id),expr,id,cdbase) => OMBIND(encode(binder),vars map (v =>
      VarDecl(LocalName(v.name),None,None,None)),encode(expr))
    case OMVarVar(omvar) => OMV(omvar.name)
    case v @ OMAttVar(OMAttributionPairs(pairslist,inner_id,inner_cdbase),va,id) => OMV(v.name)
    case OMError(sym,params,id,cdbase) => OMSTR("Error: " + sym.cd + "?" + sym.name) // TODO
    case OMAttributionPairs(pairs,id,cdbase) => ???
    case OMBindVariables(vars,id) => ???
  }

  def decodeAnyVal(t : Term) : OMAnyVal = ???
  def decode(t : Term) : OMAny = ??? // should probably recurse into the above
}