package info.kwarc.mmt.LFX.LFSubTyped

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.objects.{OMID, OMA, Term, OMS}
import info.kwarc.mmt.api.utils.URI

object LFSubTyped {
  val baseURI = DPath(URI.http colon "cds.omdoc.org") / "LFX"
  val thname = "Sub"
  val path = baseURI ? thname
  def lfsubsymbol(name : String) = path ? name
}

class LFSubSymbol(name:String) {
  val path = LFSubTyped.path ? name
  val term = OMS(path)
}

object subtypeOf extends LFSubSymbol("subtypeOf") {
  def apply(t : Term) = OMA(OMID(path),List(t))
  def unapply(t : Term) : Option[Term] = t match {
    case OMA(OMID(this.path), List(a)) => Some(a)
    case _ => None
  }
}