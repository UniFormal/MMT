package info.kwarc.mmt.LFX.LFIntersectionTypes

import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf._

/**
  * Created by raupi on 26.11.15.
  */
object IntersectionTypeRule extends InferenceRule(TypeIntersection.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case TypeIntersection(t1,t2) =>
        val tp1 = solver.inferType(t1,covered)
        val tp2 = solver.inferType(t2,covered)
        if (tp1.isEmpty || tp2.isEmpty) None
        else if (tp1==tp2) tp1
        else if (solver.safecheck(Subtyping(stack,tp1.get,tp2.get)).getOrElse(false)){
          solver.check(Subtyping(stack,tp1.get,tp2.get))
          tp2
        }
        else if (solver.safecheck(Subtyping(stack,tp2.get,tp1.get)).getOrElse(false)) {
          solver.check(Subtyping(stack,tp2.get,tp1.get))
          tp1
        }
        else None
      case _ => None
    }
  }
}

object IntersectionSubtypeRule extends SubtypingRule {
  val head = TypeIntersection.path

  def applicable(tp1: Term, tp2: Term) : Boolean = tp1!=tp2 && TypeIntersection.unapply(tp1).isDefined

  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean] =
    tp1 match {
      case TypeIntersection(t1,t2) =>
        if (solver.safecheck(Subtyping(stack,t1,tp2)).getOrElse(false))
          Some(solver.check(Subtyping(stack,t1,tp2)))
        else Some(solver.check(Subtyping(stack,t2,tp2)))
      case _ => throw Backtrack()
    }
}

object ApplyIntersectionHelper {
  def apply(f: Term, t: Term, fT: Term)(implicit stack: Stack, solver: Solver, history: History): Option[Term] = {
    solver.safeSimplifyUntil(fT)(TypeIntersection.unapply)._2 match {
      case Some((fTa, fTb)) =>
        history += "type of " + solver.presentObj(fT) + " is intersection type"
        val ttp = solver.inferType(t).getOrElse {
          history += "couldn't infer type of " + solver.presentObj(t)
          return None
        }
        Common.makePi(solver, fTa) match {
          case Pi(x, a1, a2) =>
            if (solver.safecheck(Subtyping(stack, ttp, a1)).getOrElse(false)) {
              solver.check(Subtyping(stack, ttp, a1))
              history += "type of " + solver.presentObj(t) + " matches " + solver.presentObj(fTa)
              return Some(a2 ^? OMV(x) / t)
            }
            history += "type of " + solver.presentObj(t) + " does not match " + solver.presentObj(fTa)
            ApplyIntersectionHelper(f, t, fTb)(stack, solver, history)
        }
      case _ => Common.makePi(solver, fT) match {
        case Pi(x, a1, a2) =>
          val ttp = solver.inferType(t).getOrElse {
            history += "couldn't infer type of " + solver.presentObj(t)
            return None
          }
          if (solver.safecheck(Subtyping(stack, ttp, a1)).getOrElse(false)) {
            solver.check(Subtyping(stack, ttp, a1))
            history += "type of " + solver.presentObj(t) + " matches " + solver.presentObj(fT)
            return Some(a2 ^? OMV(x) / t)
          } else None
        case _ =>
          val unks = solver.getUnsolvedVariables
          if (fT.freeVars.exists(unks.isDeclared(_))) {
            // this might be convertible into a function type once the unknown variable is solved
            history += "does not look like a function type at this point"
            solver.error("this is not a function type (type level rewriting is not supported)")
          } else {
            solver.error("this is not a function type")
          }
          None
      }
    }
  }
}

object ApplyIntersection extends EliminationRule(Apply.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
    case Apply(f,t) =>
      history += "inferring type of function " + solver.presentObj(f)
      val fTOpt = solver.inferType(f)(stack, history.branch).map(x => solver.safeSimplifyUntil(x)(TypeIntersection.unapply)._1)
      fTOpt match {
        case Some(TypeIntersection(t1,t2)) =>
          history += "intersection type is: " + solver.presentObj(TypeIntersection(t1,t2))
          ApplyIntersectionHelper(f,t,TypeIntersection(t1,t2))(stack,solver,history)
        case Some(x) => throw Backtrack()
        case _ =>
          history += "failed"
          solver.inferType(t)(stack, history.branch) // inference of the argument may solve some variables
          None
      }
    case _ => None // should be impossible
  }
}