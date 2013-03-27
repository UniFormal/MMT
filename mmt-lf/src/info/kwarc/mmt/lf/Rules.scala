package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import uom._
import utils.MyList.fromList

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
            solver.checkTyping(t,LF.ktype)(stack) //necessary because checkEquality does not check typing
            solver.checkEquality(t,t2,Some(LF.ktype))(stack)
            // solver.checkTyping(t2,LF.ktype)(stack) is redundant after the above have succeeded, but checking it anyway might help solve variables
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
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack) : Option[Term] = {
      var reduced = false // remembers if there was a reduction
      // auxiliary recursive function to beta-reduce as often as possible
      // returns Some(reducedTerm) or None if no reduction
      def reduce(f: Term, args: List[Term]): Option[Term] = (f,args) match {
         case (Lambda(x,a,t), s :: rest) => 
            solver.checkTyping(s, a) //TODO what if false?
            reduced = true
            reduce(t ^ (x / s), rest)
         case (f, Nil) =>
            //all arguments were used
            //only possible if there was a reduction, so no need for 'if (reduced)'
            Some(f)
         case _ => 
            // simplify f recursively to see if it becomes a Lambda
            val fS = solver.simplify(f)
            if (f != fS) reduce(fS, args)
            else {
              //no more reduction possible
              if (reduced)
                Some(ApplySpine(f,args : _*))
              else
                None
            }
      }
      tm match {
         //using ApplySpine here also normalizes curried application by merging them into a single one
         case ApplySpine(f, args) => reduce(f, args)
         case _ => None
      }
   }
}

object UnsafeBeta extends BreadthRule(Apply.path){
   val apply = (args: List[Term]) => {
      var reduced = false // remembers if there was a reduction
      // auxiliary recursive function to beta-reduce as often as possible
      // returns Some(reducedTerm) or None if no reduction
      def reduce(f: Term, args: List[Term]): Change = (f,args) match {
         case (Lambda(x,a,t), s :: rest) => 
            reduced = true
            reduce(t ^ (x / s), rest)
         case (f, Nil) =>
            //all arguments were used
            //only possible if there was a reduction, so no need for 'if (reduced)'
            GlobalChange(f)
         case _ => 
            // simplify f recursively to see if it becomes a Lambda
              if (reduced)
                GlobalChange(ApplySpine(f,args : _*))
              else
                NoChange
      }
      reduce(args.head, args.tail)
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
                   //TODO remove x from context
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

/** the proof step ?:Pi x:A.B ----> lambda x:A.(?:B)
 *
 * This rule works for any universe U
 */
class PiOrArrowIntroRule(op: GlobalName) extends IntroProvingRule(op) {
   def apply(tp: Term)(implicit stack: Stack) : Option[ApplicableProvingRule] = {
      tp match {
        case Pi(x,a,b) =>
           val cont = new ApplicableProvingRule {
             def label = "Pi introduction"
             def apply = Lambda(x,a,Hole(b))
           }
           Some(cont)
        case _ => None
      }
   }
}

object PiIntroRule extends PiOrArrowIntroRule(Pi.path)
object ArrowIntroRule extends PiOrArrowIntroRule(Arrow.path)


/** the proof step ?:A ----> e(?,...?)  for e:Pi x1:A1,...,xn:An.A' where A' ^ s = A for some substitution s
 *
 * This rule works for any universe U and the case n=0.
 * This rule replace ?'s in the result with their terms if they can be inferred through unification.
 */
class PiOrArrowElimRule(op: GlobalName) extends ElimProvingRule(op) {
   def apply(ev: Term, fact: Term, goal: Term)(implicit stack: Stack) : Option[ApplicableProvingRule] = {
      // tp must be of the form Pi bindings.scope
      val (bindings, scope) = FunType.unapply(fact).get
      // the free variables of scope (we drop the types because matching does not need them)
      val unknowns: Context = bindings.flatMap {
         case (Some(x),_) => List(VarDecl(x,None,None))
         case _ => Nil
      }
      // fact may contain free variables from stack.context, so make sure there are no name clashes
      // sub is a renaming from unknowns to unknownsFresh
      val (unknownsFresh, sub) = Context.makeFresh(unknowns, stack.context.map(_.name))
      val scopeFresh = scope ^ sub
      // match goal against scope, trying to solve for scope's free variables
      // TODO using a first-order matcher is too naive in general - for the general case, we need to use the Solver
      val matcher = new Matcher(unknownsFresh)
      val matchFound = matcher(stack.context, goal, scopeFresh)
      if (!matchFound) return None
      val solution = matcher.getSolution
      // sub is a renaming, so it's more efficient to compose the substitutions before applying them
      val subSolution = sub ^ solution
      // now scope ^ solution == goal
      val cont = new ApplicableProvingRule {
          def label = ev.toString
          def apply = {
             val args = bindings map {
                // named bound variables that are substituted by solution can be filled in
                // others are holes representing subgoals
                case (Some(x), xtp) =>
                   solution(x).getOrElse(Hole(xtp ^ subSolution))
                case (None, anontp) =>
                   Hole(anontp ^ subSolution)
             }
             ApplyGeneral(ev, args)
          }
      }
      Some(cont)
   }
}

object PiElimRule extends PiOrArrowElimRule(Pi.path)
object ArrowElimRule extends PiOrArrowElimRule(Arrow.path)