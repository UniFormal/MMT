package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import objects._
import frontend._
import checking._

class SimpleIrrelevanceRule(p : GlobalName) extends TypeBasedSolutionRule(List(Apply.path), p) {
  def solve(solver: Solver)(tp: Term)(implicit stack: Stack, history: History): Option[Term] = {
    val ret = solver.proveWithoutSolution(tp)
      ret
  }
}

object TermIrrelevanceRule extends ParametricRule {
  def apply(controller: Controller, home: Term, args: List[Term]) = {
    args match {
      case List(OMS(p)) => new SimpleIrrelevanceRule(p)//new TermIrrelevanceRule(List(Apply.path), p)
      case _ => throw ParseError("exactly one identifier expected")
    }
  }
}

object PiIrrelevanceRule extends TypeBasedSolutionRule(List(Apply.path), Pi.path) {
  def solve(solver: Solver)(tp: Term)(implicit stack: Stack, history: History): Option[Term] = tp match {
    case Pi(v,dom,cod) =>
      solver.rules.get(classOf[TypeBasedSolutionRule]).find(r => r.applicable(cod)) match {
        case Some(rule) =>
          val ret = rule.solve(solver)(cod)(stack ++ VarDecl(v,dom),history)
            ret.map(Lambda(v,dom,_))
        case _ => None
      }
    case _ => None
  }

  override def applicable(tm: Term): Boolean = tm match {
    case Pi(_,dom,cod) => true
    case Arrow(_,_) => true
    case _ => false
  }
}
