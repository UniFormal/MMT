package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import objects._
import frontend._
import checking._

/**
 * Irrelevance rules for (usually atomic) types that introduce proof irrelevance (as opposed to complex that happen to be irrelevant because their components are):
 * - terms of these types are always equal
 * - unknowns terms of these types are found using proof search; if provided, a default value is used to recover if search fails   
 */
class SimpleIrrelevanceRule(p : GlobalName, default: Option[Term] = None) extends TypeBasedSolutionRule(List(Apply.path), p) {
  def solve(solver: Solver)(tp: Term)(implicit stack: Stack, history: History): Option[Term] = {
    solver.prove(tp, allowUnknowns = false) orElse {default flatMap {d =>
      tp match {
        case Apply(OMS(`p`),prop) =>
          solver.error("using default value to solve " + solver.presentObj(tp), Some(Level.Gap))
          Some(Apply(d,prop))
        case _ => None
      }
    }}
  }
  /** always true */
  override def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = Some(true)
}

object TermIrrelevanceRule extends ParametricRule {
  def apply(controller: Controller, home: Term, args: List[Term]) = {
    args match {
      case List(OMS(p)) => new SimpleIrrelevanceRule(p)
      case List(OMS(p), default) => new SimpleIrrelevanceRule(p,Some(default))
      case _ => throw ParseError("exactly one identifier expected")
    }
  }
}

/**
 * x:A |- ? : B   --->   |- ? : Pi x:A.B 
 */
object PiIrrelevanceRule extends TypeBasedSolutionRule(Nil, Pi.path) with PiOrArrowRule {
  def solve(solver: Solver)(tp: Term)(implicit stack: Stack, history: History): Option[Term] = tp match {
    case Pi(x,a,b) =>
      solver.findUniqueInhabitant(b)(stack ++ VarDecl(x,a), history) map {t =>
         Lambda(x,a,t)
      }
    case _ => None
  }
}
