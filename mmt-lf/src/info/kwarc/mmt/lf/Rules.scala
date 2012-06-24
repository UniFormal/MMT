package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import objects._
import objects.Conversions._

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
           case Pi(x,a,b) => Some(b ^ (x / t))
           case _ => None
        }
     case _ => None
   }
}

class Beta extends ComputationRule(LF.lftheory ? "apply") {
   def apply(solver: Solver)(tm: Term)(implicit context: Context) : Option[Term] = tm match {
      case Apply(Lambda(x,a,t),s) => Some(t ^ (x / s))
      case _ => None
   }
}
