package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import utils.MyList.fromList

class PiTerm extends InferenceRule(LF.lftheory ? "Pi") {
   def apply(solver: Solver)(tm: Term)(implicit context: Context) : Option[Term] = {
      tm match {
        case Pi(x,a,b) =>
           if (solver.checkTyping(a, LF.ktype))
              solver.inferType(b)(context ++ x % a)
           else
              None
        case _ => None
      }
   }
}

class LambdaTerm extends InferenceRule(LF.lftheory ? "lambda") {
   def apply(solver: Solver)(tm: Term)(implicit context: Context) : Option[Term] = {
      tm match {
        case Lambda(x,a,t) =>
           solver.inferType(t)(context ++ x % a) map {b => Pi(x,a,b)}
        case _ => None
      }
   }
}

class ApplyTerm extends InferenceRule(LF.lftheory ? "apply") {
   def apply(solver: Solver)(tm: Term)(implicit context: Context) : Option[Term] = tm match {
     case Apply(f,t) =>
        solver.inferType(f) flatMap {
           case Pi(x,a,b) =>
              if (solver.checkTyping(t, a))
                 Some(b ^ (x / t))
              else
                 None
           case _ => None
        }
     case _ => None
   }
}

class PiType extends TypingRule(LF.lftheory ? "Pi") {
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit context: Context) : Boolean = {
      (tm,tp) match {
         case (Lambda(x,t,s),Pi(x2,t2,a)) =>
            solver.checkEquality(t,t2,Some(LF.ktype))(context)
            solver.checkTyping(s, a ^ (x2 / OMV(x)))(context ++ x % t)
         case (tm, Pi(x2, t2, a)) =>
            if (context.isDeclared(x2)) {
               val x = OMV("new") //TODO invent new variable name
               solver.checkTyping(Apply(tm, x), a ^ (x2 / x))(context ++ x % t2)
            } else
               solver.checkTyping(Apply(tm, OMV(x2)), a)(context ++ x2 % t2)
      }
   }
}

class Eta extends EqualityRule(LF.lftheory ? "Pi") {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit context: Context): Boolean = {
      val Pi(x, a, b) = tp //TODO invent new variable name if context.isDeclared(x)
      val tm1Eval = OMA(tm1, List(OMV(x)))
      val tm2Eval = OMA(tm2, List(OMV(x)))
      solver.checkEquality(tm1Eval, tm2Eval, Some(b))(context ++ x % a)
   }
}

class Beta extends ComputationRule(LF.lftheory ? "apply") {
   def apply(solver: Solver)(tm: Term)(implicit context: Context) : Option[Term] = tm match {
      case Apply(Lambda(x,a,t),s) => Some(t ^ (x / s))
      case _ => None
   }
}

class Solve extends SolutionRule(LF.lftheory ? "apply") {
   def apply(solver: Solver)(unknown: LocalName, args: List[Term], tm2: Term)(implicit context: Context): Boolean = {
      val vars = args mapPartial {
         case OMV(x) => if (context.isDeclared(x)) Some(x) else None
         case _ => None
      }
      if (vars.distinct.length == args.length) {
         //all arguments are distinct object variables
         val con : Context = vars map {x => context(x)}
         solver.checkEquality(OMV(unknown), Lambda(con, tm2), None)
      }
      else false
   }
}
