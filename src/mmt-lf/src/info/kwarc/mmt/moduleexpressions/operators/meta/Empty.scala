package info.kwarc.mmt.moduleexpressions.operators.meta

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.{Recurse, Simplifiability, Simplify, UnaryConstantScala}
import info.kwarc.mmt.moduleexpressions.operators.Combinators

/** operator for the empty theory of a given meta-theory */
object Empty extends UnaryConstantScala(Combinators._path, "empty") {
  val label = LocalName("empty")
}

/** Empty(p) ---> p{} */
object ComputeEmpty extends ComputationRule(Extends.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
    val mt = tm match {
      case Empty(OMMOD(p)) => Some(p)
      case OMS(Empty.path) => None
      case _ => return Recurse
    }
    val thy = new AnonymousTheory(mt, Nil)
    val dn = DiagramNode(Empty.label, thy)
    val ad = new AnonymousDiagram(List(dn), Nil, Some(dn.label))
    Simplify(ad.toTerm)
  }
}
