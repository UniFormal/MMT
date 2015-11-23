package info.kwarc.mmt.LFX.LFWTypes

import info.kwarc.mmt.api.{LocalName, DPath}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.utils.URI

/**
 * Created by raupi on 19.10.15.
 */
object LFWTypes {
  val baseURI = DPath(URI.http colon "cds.omdoc.org") / "LFX"
  val thname = "WType"
  val th2name = "LFW"
  val path1 = baseURI ? thname
  val path2 = baseURI ? th2name
}

object WType {
  val path = LFWTypes.path1 ? "wtype"
  val term = OMS(path)
  def apply(name : LocalName, tp : Term, body : Term) = OMBIND(this.term, OMV(name) % tp, body)
  def apply(con: Context, body : Term) = OMBIND(this.term, con, body)
  def unapply(t : Term) : Option[(LocalName,Term,Term)] = t match {
    case OMBIND(OMS(this.path), Context(VarDecl(n,Some(a),None,_), rest @ _*), s) =>
      val newScope = if (rest.isEmpty) s else apply(Context(rest:_*), s)
      Some(n,a,newScope)
    case _ => None
  }
}

object sup {
  val path = LFWTypes.path1 ? "sup"
  val term = OMS(path)
  def apply(a:Term,f:Term) = OMA(this.term,List(a,f))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(a,f)) => Some((a,f))
    case _ => None
  }
}

object rec {
  val path = LFWTypes.path1 ? "rec"
  val term = OMS(path)
  def apply(c:Term,p:Term) = OMA(this.term,List(c,p))
  def unapply(t : Term) : Option[(Term,Term)] = t match {
    case OMA(this.term,List(c,p)) => Some((c,p))
    case _ => None
  }
}