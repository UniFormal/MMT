package info.kwarc.mmt.LFX.FiniteTypes

import info.kwarc.mmt.LFX.Coproducts.{Coprod, inl, inr}
import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.objects.{OMA, OMS, Term}
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

object Finite {
  def apply(i : Int) = {
    require(i>=0)
    if (i == 0) EmptyType.term
    Coprod((1 to i).map(_ => Unit.term):_*)
  }
}

object ElemOfUnit {
  val name = "ElemOfOneType"
  val path = (FiniteTypes.baseURI ? "LFFinite") ? name
  val term = OMS(path)
}