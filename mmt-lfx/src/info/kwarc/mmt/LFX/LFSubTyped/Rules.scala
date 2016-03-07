package info.kwarc.mmt.LFX.LFSubTyped

import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.{LocalName, objects}
import info.kwarc.mmt.api.objects._
import objects.Conversions._
import info.kwarc.mmt.lf._

/** Declared Subtypes **/
object SubUniverseRule extends UniverseRule(subtypeOf.path) {
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = tm match {
    case subtypeOf(t) => solver.check(Inhabitable(stack,t))
    case _ => false
  }
}

object SubUniverseType extends InferenceRule(subtypeOf.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case subtypeOf(t) =>
        history +="type of "+solver.presentObj(tm)+" is type of "+solver.presentObj(t)
        solver.inferType(t,covered)
      case _ => None
    }
  }
}

object SubtypeOfTypeRule extends SubtypingRule {
  val head = subtypeOf.path

  def applicable(tp1: Term, tp2: Term): Boolean = tp1 match {
    case subtypeOf(t) => true
    case _ => false
  }

  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean] =
    solver.safeSimplifyUntil(tp1)(subtypeOf.unapply)._1 match {
    case subtypeOf(t) =>
      val tpt = solver.inferType(t)
      if (tpt.isDefined) Some(solver.check(Subtyping(stack,tpt.get,tp2))) else Some(false)
    case _ => throw RuleNotApplicable
  }
}


object SubtypeOfRule extends SubtypingRule {
  val head = subtypeOf.path

  def applicable(tp1: Term, tp2: Term) : Boolean = tp1!=tp2 && subtypeOf.unapply(tp1).isEmpty && !PiRule.applicable(tp1,tp2)

  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean] =
    solver.inferType(tp1) match {
      case Some(t) => solver.safeSimplifyUntil(t)(subtypeOf.unapply)._1 match {
        case subtypeOf(s) => Some(solver.check(Subtyping(stack,s,tp2)))
        case _ => throw RuleNotApplicable
      }
      case _ => history += "could not infer type of "+solver.presentObj(tp1)
        None
    }
}

object PiRule extends SubtypingRule {
  val head = Pi.path

  def applicable(tp1: Term, tp2: Term) : Boolean = (tp1,tp2) match {
    case (Pi(v,a,b),Pi(w,c,d)) => true
    case _ => false
  }

  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean] = (tp1,tp2) match {
    case (Pi(x1,a1,b1),Pi(x2,a2,b2)) =>
      val (xn,_) = Context.pickFresh(stack.context, x1)
      Some(solver.check(Subtyping(stack,a2,a1)) && solver.check(Subtyping(stack ++ xn % a2, b1 ^? (x1/OMV(xn)),b2 ^? (x2/OMV(xn)))))
    case _ => throw RuleNotApplicable
  }
}

/** Subtyping Judgments **/

object SubJudgUniverseRule extends UniverseRule(subtypeJudg.path) {
  def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Boolean = tm match {
    case subtypeJudg(t1,t2) => solver.check(Inhabitable(stack,t1)) && solver.check(Inhabitable(stack,t2))
    case _ => false
  }
}

object SubJudgUniverseType extends InferenceRule(subtypeJudg.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    tm match {
      case subtypeJudg(t1,t2) => Some(OMS(Typed.kind))
      case _ => None
    }
  }
}

object SubJudgRule extends SubtypingRule {
  val head = subtypeJudg.path

  def applicable(tp1: Term, tp2: Term) : Boolean = true

  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean] = {
    solver.prove(subtypeJudg(tp1, tp2)).getOrElse {
      throw RuleNotApplicable
    }
    Some(true)
  }
}

/** Predicate Subtypes **/

object Predsubtype extends FormationRule(predsubtp.path,OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
    case predsubtp(tpA, p) =>
      solver.safeSimplifyUntil(solver.inferType(p,covered).getOrElse(return None))(Pi.unapply)._1 match {
        case Pi(x,tpx,f) =>
          solver.check(Subtyping(stack,tpA,tpx))
          solver.check(Typing(stack ++ x%tpA,f,OMS(Typed.ktype),None))
          Some(OMS(Typed.ktype))
        case _ => throw RuleNotApplicable
      }
    case _ => throw RuleNotApplicable
  }
}

object Predsubrule extends SubtypingRule {
  val head = predsubtp.path

  def applicable(tp1: Term, tp2: Term): Boolean = tp1 match {
    case predsubtp(tpA, p) => true
    case _ => false
  }

  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History): Option[Boolean] = tp1 match {
    case predsubtp(tpA, p) =>
      solver.safeSimplifyUntil(solver.inferType(p, true).getOrElse(return None))(Pi.unapply)._1 match {
        case Pi(x, tpx, f) =>
          def default = {
            val (y, sub) = Context.pickFresh((stack ++ x % tpx).context, LocalName("_"))
            Some(solver.check(Typing(stack ++ x % tpx ++ y % solver.simplify(Apply(p, x)), x, tp2)))
          }
          solver.safeSimplifyUntil(tp2)(predsubtp.unapply)._1 match {
            case predsubtp(tpB, q) =>
              solver.safeSimplifyUntil(solver.inferType(q, true).getOrElse(return default))(Pi.unapply)._1 match {
                case Pi(x2, tpB2, g) =>
                  /* x:A, P(x) |- x : B,   x:A |- P(x) => Q(x) ---> {A | P(x)} <: {B | Q(x)} */
                  history += "Trying to prove " + solver.presentObj(Arrow(Apply(p, x), Apply(q, x)))
                  val ret = solver.prove(Arrow(Apply(p, x), Apply(q, x)))(stack ++ x % tpA, history)
                  if (ret.isDefined) default else Some(false)
                case _ =>
                  default
                case _ =>
                  default
              }
            case _ => throw RuleNotApplicable
          }
        case _ => throw RuleNotApplicable
      }
  }
}

object PredsubTyping extends TypingRule(predsubtp.path) {
  def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) : Boolean = tp match {
    case predsubtp(tpA,p) =>
      solver.check(Typing(stack,tm,tpA))
      val ret = solver.prove(Apply(p,tm))
      if (ret.isDefined) true else false
    case _ => throw RuleNotApplicable
  }
}

object PredOfTerm extends EliminationRule(PredOf.path,OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term] = tm match {
    case PredOf(t) =>
      val tp = solver.inferType(t).getOrElse(throw RuleNotApplicable)
      solver.safeSimplifyUntil(tp)(predsubtp.unapply)._1 match {
        case predsubtp(tpA,p) =>
          Some(Apply(p,t))
        case _ => None
      }
    case _ => throw RuleNotApplicable
  }
}