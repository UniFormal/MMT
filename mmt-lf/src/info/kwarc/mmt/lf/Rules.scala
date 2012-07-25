package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import utils.MyList.fromList

/** the type inference rule type:kind */
object UnivTerm extends InferenceRule(Univ.ktype) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = tm match {
      case LF.ktype => Some(LF.kind)
      case _ => None
   }
}

/** the type inference rule x:A:type|-B:U  --->  Pi x:A.B : U
 * This rule works for any universe U */
object PiTerm extends InferenceRule(Pi.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = {
      tm match {
        case Pi(x,a,b) =>
           if (solver.checkTyping(a, LF.ktype))
              solver.inferType(b)(stack ++ x % a)
           else
              None
        case _ => None
      }
   }
}

/** the type inference rule x:A|-t:B  --->  lambda x:A.t : Pi x:A.B
 * This rule works for B:U for any universe U */
object LambdaTerm extends InferenceRule(Lambda.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = {
      tm match {
        case Lambda(x,a,t) =>
           solver.inferType(t)(stack ++ x % a) map {b => Pi(x,a,b)}
        case _ => None
      }
   }
}

/** the type inference rule f : Pi x:A.B  ,  t : A  --->  f t : B [x/t]
 * This rule works for B:U for any universe U */
object ApplyTerm extends InferenceRule(Apply.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = tm match {
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

/** the type checking rule x:A|-f x:B  --->  f : Pi x:A.B */
object PiType extends TypingRule(Pi.path) {
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack) : Boolean = {
      (tm,tp) match {
         case (Lambda(x,t,s),Pi(x2,t2,a)) =>
            solver.checkEquality(t,t2,Some(LF.ktype))(stack)
            val asub = if (x2 == x) a else a ^ (x2 / OMV(x))  
            solver.checkTyping(s, asub)(stack ++ x % t2)
         case (tm, Pi(x2, t2, a)) =>
            if (stack.context.isDeclared(x2)) {
               val x = OMV(x2 / "")
               solver.checkTyping(Apply(tm, x), a ^ (x2 / x))(stack ++ x % t2)
            } else
               solver.checkTyping(Apply(tm, OMV(x2)), a)(stack ++ x2 % t2)
      }
   }
}

/** the extensionality rule (equivalent to Eta) x:A|-f x = g x  --->  f = g  : Pi x:A. B
 * If possible, the name of the new variable x is taken from f, g, or their type; otherwise, a fresh variable is invented. */
object Extensionality extends EqualityRule(Pi.path) {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack): Boolean = {
      val Pi(x, a, b) = tp
      //TODO pick new variable name properly; currently, a quick optimization 
      val (name, s, t) = (tm1, tm2) match {
         case (Lambda(v, a1, s), Lambda(w, a2, t)) if v == w =>
            solver.checkEquality(a1, a2, Some(LF.ktype))
            (v, Some(s), Some(t)) 
         case _ if ! stack.context.isDeclared(x) => (x, None, None)
         case _ => (x / "", None, None)
      }
      val tm1Eval = s getOrElse Apply(tm1, OMV(name))
      val tm2Eval = t getOrElse Apply(tm2, OMV(name))
      solver.checkEquality(tm1Eval, tm2Eval, Some(b))(stack ++ name % a)
   }
}

/** the beta-reduction rule s : A  --->  (lambda x:A.t) s = t [x/s]
 * If not applicable, the function term is simplified recursively and the rule tried again.
 * This rule also normalizes nested applications so that it implicitly implements the currying rule (f s) t = f(s,t).
 */ 
object Beta extends ComputationRule(Apply.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = tm match {
      case Apply(Lambda(x,a,t), s) =>
         solver.checkTyping(s, a)
         Some(t ^ (x / s))
      //using ApplySpine here also normalizes curried application by merging them into a single one
      case ApplySpine(f, args) =>
         // simplify f recursively to see if it becomes a Lambda
         val fS = solver.simplify(f)
         if (f != fS) apply(solver)(ApplySpine(fS,args : _*))
         else None
      case _ => None
   }
}

/** A simplification rule that implements A -> B = Pi x:A.B  for fresh x.
 * LocalName.Anon is used for x */ 
object ExpandArrow extends ComputationRule(Arrow.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = tm match {
      case Arrow(a,b) => Some(Pi(LocalName.Anon, a, b))
      case _ => None
   }
}

/** This rule tries to solve for an unkown by applying lambda-abstraction on both sides and eta-reduction on the left.
 *  Its effect is, for example, that X x = t is reduced to X = lambda x.t where X is a meta- and x an object variable. 
 */
object Solve extends SolutionRule(Apply.path) {
   def apply(solver: Solver)(tm1: Term, tm2: Term)(implicit stack: Stack): Boolean = {
      tm1 match {
         case Apply(t, OMV(x)) if stack.context.isDeclared(x) =>
             if (t.freeVars contains x) return false
             stack.context(x) match {
                case VarDecl(_, Some(a), _, _*) => 
                   solver.checkEquality(t, Lambda(x, a, tm2), None) // tpOpt map {tp => Pi(x,a,tp)}
                case _ => false
             }
         case _ => false
      }
   }
}

/** This rule implements equality in the initial model.
 *  Two terms formed by applying the same undefined function symbol to the same number of arguments are equal iff
 *  the arguments are equal component-wise.
 *  This rule may be implicitly present in many languages or may be present with a different name.
 */
//TODO: avoid losing the results of type inference
object Initial extends AtomicEqualityRule(Apply.path) {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack): Boolean = {
      val ApplySpine(t1, args1) = tm1
      val ApplySpine(t2, args2) = tm2
      if (args1.length == args2.length) {
         val argsEqual = (args1 zip args2) forall {case (a,b) => solver.checkEquality(a,b, None)}
         if (argsEqual) solver.checkEquality(t1,t2, None) else false
      } else false
/*         solver.inferType(f) map solver.simplify match {
            case Some(Pi(x,a,b)) =>
              solver.checkEquality(s, t, Some(a))
            case _ => false
         }*/
   }
}
