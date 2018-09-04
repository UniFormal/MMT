package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import objects._
import frontend._
import checking._

/**
 * tries to solve an unknown by proof search; if provided, recovers by returning a dummy term as a default   
 */
class SimpleIrrelevanceRule(p : GlobalName, default: Option[Term] = None) extends TypeBasedSolutionRule(List(Apply.path), p) {
  def solve(solver: Solver)(tp: Term)(implicit stack: Stack, history: History): Option[Term] = {
    solver.prove(tp) orElse {default flatMap {d =>
      tp match {
        case Apply(OMS(`p`),prop) =>
          solver.warning("using default value to solve " + solver.presentObj(tp))
          Some(Apply(d,prop))
        case _ => None
      }
    }}
  }
}

object TermIrrelevanceRule extends ParametricRule {
  def apply(controller: Controller, home: Term, args: List[Term]) = {
    args match {
      case List(OMS(p)) => new SimpleIrrelevanceRule(p)//new TermIrrelevanceRule(List(Apply.path), p)
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
