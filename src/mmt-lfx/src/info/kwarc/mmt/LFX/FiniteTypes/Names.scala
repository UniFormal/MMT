package info.kwarc.mmt.LFX.FiniteTypes

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.objects.{Term, OMA, OMS}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.LFX.LFX

object FiniteTypes {
  val baseURI = LFX.ns / "Finite"
  val thname = "Symbols"
  val path = baseURI ? thname
  def symbol(name : String) = path ? name
}

object EmptyType {
  val path = FiniteTypes.path ? "emptyType"
  val term = OMS(path)
}

object EmptyFun {
  val path = FiniteTypes.path ? "emptyFun"
  val term = OMS(path)
  def apply(tp:Term) = OMA(this.term,List(tp))
  def unapply(t:Term) : Option[Term] = t match {
    case OMA(this.term,List(tp)) => Some(tp)
    case _ => None
  }
}

object Unit {
  val path = FiniteTypes.baseURI ? "LFFinite" ? "Unit"
  val term = OMS(path)
}