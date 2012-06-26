package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import utils.MyList.fromList

object UnivTerm extends InferenceRule(LF.Ptype) {
   def apply(solver: Solver)(tm: Term)(implicit context: Context) : Option[Term] = tm match {
      case LF.ktype => Some(LF.kind)
      case _ => None
   }
}

object PiTerm extends InferenceRule(LF.PPi) {
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

object LambdaTerm extends InferenceRule(LF.Plambda) {
   def apply(solver: Solver)(tm: Term)(implicit context: Context) : Option[Term] = {
      tm match {
        case Lambda(x,a,t) =>
           solver.inferType(t)(context ++ x % a) map {b => Pi(x,a,b)}
        case _ => None
      }
   }
}

object ApplyTerm extends InferenceRule(LF.Papply) {
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

object PiType extends TypingRule(LF.PPi) {
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit context: Context) : Boolean = {
      (tm,tp) match {
         case (Lambda(x,t,s),Pi(x2,t2,a)) =>
            solver.checkEquality(t,t2,Some(LF.ktype))(context)
            val asub = if (x2 == x) a else a ^ (x2 / OMV(x))  
            solver.checkTyping(s, asub)(context ++ x % t2)
         case (tm, Pi(x2, t2, a)) =>
            if (context.isDeclared(x2)) {
               val x = OMV("new") //TODO invent new variable name
               solver.checkTyping(Apply(tm, x), a ^ (x2 / x))(context ++ x % t2)
            } else
               solver.checkTyping(Apply(tm, OMV(x2)), a)(context ++ x2 % t2)
      }
   }
}

object Eta extends EqualityRule(LF.PPi) {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit context: Context): Boolean = {
      val Pi(x, a, b) = tp
      //TODO pick new variable name properly; currently, a quick optimization 
      val (name, s, t) = (tm1, tm2) match {
         case (Lambda(v, a1, s), Lambda(w, a2, t)) if v == w =>
            solver.checkEquality(a1, a2, Some(LF.ktype))
            (v, Some(s), Some(t)) 
         case _ if ! context.isDeclared(x) => (x, None, None)
         case _ => (x / "", None, None)
      }
      val tm1Eval = s getOrElse Apply(tm1, OMV(name))
      val tm2Eval = t getOrElse Apply(tm2, OMV(name))
      solver.checkEquality(tm1Eval, tm2Eval, Some(b))(context ++ name % a)
   }
}

object Injective extends EqualityRule(LF.Papply) {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit context: Context): Boolean = {
      val ApplySpine(f, s) = tm1
      val ApplySpine(g, t) = tm2
      if (f == g && s.length == t.length) {
         (s zip t) forall {case (a,b) => solver.checkEquality(a,b, None)}
/*         solver.inferType(f) map solver.simplify match {
            case Some(Pi(x,a,b)) =>
              solver.checkEquality(s, t, Some(a))
            case _ => false
         }*/
      } else
        false
   }
}

object Beta extends ComputationRule(LF.Papply) {
   def apply(solver: Solver)(tm: Term)(implicit context: Context) : Option[Term] = tm match {
      case Apply(Lambda(x,a,t),s) =>
         solver.checkTyping(s, a)
         Some(t ^ (x / s))
      case Apply(f,t) =>
         // simplify f recursively to see if it becomes a Lambda
         val fS = solver.simplify(f)
         if (f != fS) apply(solver)(Apply(fS,t))
         else None
      case _ => None
   }
}

object ExpandArrow extends ComputationRule(LF.Parrow) {
   def apply(solver: Solver)(tm: Term)(implicit context: Context) : Option[Term] = tm match {
      case Arrow(a,b) => Some(Pi(LocalName.Anon, a, b))
      case _ => None
   }
}

object Solve extends SolutionRule(LF.Papply) {
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
