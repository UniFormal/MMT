package info.kwarc.mmt.LFFiniteTypes

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.objects.{Term, OMA, OMS}
import info.kwarc.mmt.api.utils.URI

/**
 * Created by raupi on 19.10.15.
 */
object LFFiniteTypes {
  val baseURI = DPath(URI.http colon "cds.omdoc.org") / "LFX"
  val thname = "LFFinite"
  val path = baseURI ? thname
  def symbol(name : String) = path ? name
}

object EmptyType {
  val path = (LFFiniteTypes.baseURI ? "EmptyType") ? "emptyType"
  val term = OMS(path)
}

object EmptyFun {
  val path = (LFFiniteTypes.baseURI ? "EmptyFunction") ? "emptyFun"
  val term = OMS(path)
  def apply(tp:Term) = OMA(this.term,List(tp))
  def unapply(t:Term) : Option[Term] = t match {
    case OMA(this.term,List(tp)) => Some(tp)
    case _ => None
  }
}