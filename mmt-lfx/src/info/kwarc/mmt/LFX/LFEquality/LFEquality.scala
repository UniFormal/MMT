package info.kwarc.mmt.LFX.LFEquality

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.objects.{OMA, Term, OMS}
import info.kwarc.mmt.api.utils.URI

/**
  * Created by raupi on 16.02.16.
  */
object LFEquality {
  val baseURI = DPath(URI.http colon "cds.omdoc.org") / "LFX"
  val thname = "EqTypes"
  val path = baseURI ? thname
  def lfeqsymbol(name : String) = path ? name
}

class EqSymbol(name:String) {
  val path = LFEquality.path ? name
  val term = OMS(path)
}

object eqtype extends EqSymbol("eqtype") {
  def apply(t1:Term,t2:Term) = OMA(this.term,List(t1,t2))
  def unapply(t:Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(a,b)) => Some(a,b)
    case _ => None
  }
}

object refl extends EqSymbol("refl") {
  def apply(t1:Term) = OMA(this.term,List(t1))
  def unapply(t:Term) : Option[Term] = t match {
    case OMA(this.term,List(a)) => Some(a)
    case _ => None
  }
}

object cong extends EqSymbol("cong") {
  def apply(a:Term,c:Term,p:Term) = OMA(this.term,List(a,c,p))
  def unapply(t:Term) : Option[(Term,Term,Term)] = t match {
    case OMA(this.term,List(a,c,p)) => Some((a,c,p))
    case _ => None
  }
}