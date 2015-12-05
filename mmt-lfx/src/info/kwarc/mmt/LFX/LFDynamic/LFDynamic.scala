package info.kwarc.mmt.LFX.LFDynamic

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.URI

object LFDynamic {
  val baseURI = DPath(URI.http colon "cds.omdoc.org") / "LFX"
  val thname = "Dynamic"
  val path = baseURI ? thname
  def lfsubsymbol(name : String) = path ? name
}

class DynamicSymbol(name:String) {
  val path = LFDynamic.path ? name
  val term = OMS(path)
}

object VarType extends DynamicSymbol("Var") {
  def apply(t : Term) = OMA(OMID(path),List(t))
  def unapply(t : Term) : Option[Term] = t match {
    case OMA(OMID(this.path), List(a)) => Some(a)
    case _ => None
  }
}

object Assign extends DynamicSymbol("ass") {
  def apply(v : OMV, t:Term, body:Term) = OMA(OMID(path),List(v,t,body))
  def unapply(t : Term) : Option[(OMV,Term,Term)] = t match {
    case OMA(OMID(this.path), List(a:OMV,b:Term,c:Term)) => Some((a,b,c))
    case _ => None
  }
}