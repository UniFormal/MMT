package info.kwarc.mmt.LFX.LFIntersectionTypes

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.objects.{OMID, OMA, Term, OMS}
import info.kwarc.mmt.api.utils.URI

/**
  * Created by raupi on 26.11.15.
  */
object LFIntersectionTypes {
  val baseURI = DPath(URI.http colon "cds.omdoc.org") / "LFX"
  val thname = "Intersect"
  val path = baseURI ? thname
  def lfsubsymbol(name : String) = path ? name
}

class SubSymbol(name:String) {
  val path = LFIntersectionTypes.path ? name
  val term = OMS(path)
}

/* class LFSubSymbol(name:String) {
  val path = LFIntersectionTypes.baseURI ? "LFIntersectionTypes" ? name
  val term = OMS(path)
} */

object TypeIntersection extends SubSymbol("typeintersection") {
  def apply(t1 : Term, t2 : Term) = OMA(OMID(path),List(t1,t2))
  def apply(t:Term*) : Term = OMA(OMID(path),t.toList)
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(OMID(this.path), List(a,b)) => Some((a,b))
    case OMA(OMID(this.path),l) if l.length>2 => Some(l.head,apply(l.tail :_*))
    case _ => None
  }
}