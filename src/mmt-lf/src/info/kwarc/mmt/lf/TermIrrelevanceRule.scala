package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import objects._
import frontend._
import checking._

object TermIrrelevanceRule extends ParametricRule {
  def apply(controller: Controller, home: Term, args: List[Term]) = {
    args match {
      case List(OMS(p)) => new TermIrrelevanceRule(List(Apply.path), p)
      case _ => throw ParseError("exactly one identifier expected")
    }
  }
}
// @FLorian debatable, but needed when e.g. a) the proof is itself pi-bound and
// b) is not the first argument. Then the solver will try to prove I : {<previous_args>}|-<prop>
// and fail, because pi
