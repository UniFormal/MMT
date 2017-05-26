package info.kwarc.mmt.LFX.WTypes

import info.kwarc.mmt.LFX.LFX
import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.objects._

object WTypes {
  val baseURI = LFX.ns / "WTypes"
  val thname = "Symbols"
  val path = baseURI ? thname
}

object WType {
  val path = WTypes.path ? "wtype"
  val term = OMS(path)
  def apply(name : LocalName, tp : Term, body : Term) = OMBIND(this.term, OMV(name) % tp, body)
  def apply(con: Context, body : Term) = OMBIND(this.term, con, body)
  def unapply(t : Term) : Option[(LocalName,Term,Term)] = t match {
    case OMBIND(OMS(this.path), Context(VarDecl(n,None,Some(a),None,_), rest @ _*), s) =>
      val newScope = if (rest.isEmpty) s else apply(Context(rest:_*), s)
      Some(n,a,newScope)
    case _ => None
  }
}

object sup {
  val path = WTypes.path ? "sup"
  val term = OMS(path)
  def apply(a:Term,f:Term) = OMA(this.term,List(a,f))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(a,f)) => Some((a,f))
    case _ => None
  }
}

object rec {
  val path = WTypes.path ? "rec"
  val term = OMS(path)
  def apply(c:Term,p:Term) = OMA(this.term,List(c,p))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(c,p)) => Some((c,p))
    case _ => None
  }
}
