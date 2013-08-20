package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import uom._
import utils.MyList.fromList

object Common {
   def isType(solver: Solver, a: Term)(implicit stack: Stack, history: History) =
      solver.check(Typing(stack, a, OMS(Typed.ktype), Some(OfType.path)))(history + "type of bound variable must be a type")
}

import Common._

/** the type inference rule x:A:type|-B:U  --->  Pi x:A.B : U
 * This rule works for any universe U */
object PiTerm extends InferenceRule(Pi.path, OfType.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Option[Term] = {
      tm match {
        case Pi(x,a,b) =>
           isType(solver,a)
           solver.inferType(b)(stack ++ x % a, history) flatMap {bT =>
                 if (bT.freeVars contains x) {
                    solver.error("type of Pi-term has been inferred as shown, but contains free variable " + x)
                    None
                 } else
                    Some(bT)
           }
        case _ => None // should be impossible
      }
   }
}

/** the type inference rule x:A|-t:B  --->  lambda x:A.t : Pi x:A.B
 * This rule works for B:U for any universe U */
object LambdaTerm extends InferenceRule(Lambda.path, OfType.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Option[Term] = {
      tm match {
        case Lambda(x,a,t) =>
           isType(solver,a)
           solver.inferType(t)(stack ++ x % a, history) map {b => Pi(x,a,b)}
        case _ => None // should be impossible
      }
   }
}

/** the type inference rule f : Pi x:A.B  ,  t : A  --->  f t : B [x/t]
 * This rule works for B:U for any universe U */
object ApplyTerm extends InferenceRule(Apply.path, OfType.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Option[Term] = tm match {
     case Apply(f,t) =>
        solver.inferType(f)(stack, history.branch) flatMap {
           case tp @ Pi(x,a,b) =>
               history += "function type is: " + solver.presentObj(tp) 
               solver.check(Typing(stack, t, a))(history + "argument must have domain type")
               Some(b ^ (x / t))
           case _ =>
              // TODO simplify type to make it a Pi
              None
        }
     case _ => None // should be impossible
   }
}

/** the type checking rule x:A|-f x:B  --->  f : Pi x:A.B */
object PiType extends TypingRule(Pi.path) {
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) : Boolean = {
      (tm,tp) match {
         case (Lambda(x,t,s),Pi(x2,t2,a)) =>
            //checking of t:type necessary because checkEquality does not check typing
            isType(solver,t)
            solver.check(Equality(stack,t,t2,Some(OMS(Typed.ktype))))(history+"domains must be equal")
            // solver.checkTyping(t2,LF.ktype)(stack) is redundant after the above have succeeded, but checking it anyway might help solve variables
            val asub = if (x2 == x) a else a ^ (x2 / OMV(x))  
            solver.check(Typing(stack ++ x % t2, s, asub))(history + "type checking rule for Pi")
         case (tm, Pi(x2, t2, a)) =>
            val j = if (stack.context.isDeclared(x2)) {
               val x = OMV(x2 / "")
               Typing(stack ++ x % t2,  Apply(tm, x), a ^ (x2 / x))
            } else
               Typing(stack ++ x2 % t2, Apply(tm, OMV(x2)), a)
            solver.check(j)(history + "type checking rule for Pi")
      }
   }
}

/** the extensionality rule (equivalent to Eta) x:A|-f x = g x  --->  f = g  : Pi x:A. B
 * If possible, the name of the new variable x is taken from f, g, or their type; otherwise, a fresh variable is invented. */
object Extensionality extends TypeBasedEqualityRule(Pi.path) {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Boolean = {
      val Pi(x, a, b) = tp
      //TODO pick new variable name properly; currently, a quick optimization 
      val (y, u1, u2) = (tm1, tm2) match {
         case (Lambda(x1, a1, t1), Lambda(x2, a2, t2)) if x1 == x2 =>
            solver.check(Equality(stack, a1, a2, Some(OMS(Typed.ktype))))
            (x1, Some(t1), Some(t2)) 
         case _ if ! stack.context.isDeclared(x) => (x, None, None)
         case _ => (x / "", None, None)
      }
      val tm1Eval = u1 getOrElse Apply(tm1, OMV(y))
      val tm2Eval = u2 getOrElse Apply(tm2, OMV(y))
      solver.check(Equality(stack ++ y % a, tm1Eval, tm2Eval, Some(b)))
   }
}

/** Congruence for Lambda
 *  
 *  We cannot use CongruenceRule here because we have to flatten nested lambdas in addition.
 *  
 *  This rule is a special case of Extensionality, but it does not make use of the type.
 */
object LambdaCongruence extends TermBasedEqualityRule(Lambda.path, Lambda.path) {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
      (tm1,tm2) match {
         case (Lambda(x1,a1,t1), Lambda(x2,a2,t2)) =>
            val cont = Continue {
               history += "congruence for lambda"
               val res1 = solver.check(Equality(stack,a1,a2,Some(OMS(Typed.ktype))))(history + "equality of domain types")
               val a = a1
               val (x,s1,s2) =
                  if (x1 == x2)
                     (x1, t1, t2)
                  else {
                     if (! stack.context.isDeclared(x1))
                        (x1, t1, t2 ^ (x2 / OMV(x1)))
                     else {
                        val x = x1 / ""
                        (x1 / "", t1 ^ (x1 / OMV(x)), t2 ^ (x2 / OMV(x)))
                     }
                  }
               val res2 = solver.check(Equality(stack ++ x % a, s1, s2, None))(history + "equality of scopes")
               res1 && res2
            }
            Some(cont)
         case _ => None
      }
   }
}

/** Congruence for Pi
 *  
 *  We cannot use CongruenceRule here because we have to flatten nested Pis and consider -> in addition.
 */
object PiCongruence extends TermBasedEqualityRule(Pi.path, Pi.path) {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
      (tm1,tm2) match {
         case (Pi(x1,a1,t1), Pi(x2,a2,t2)) =>
            val cont = Continue {
               history += "congruence for function types"
               val res1 = solver.check(Equality(stack,a1,a2,Some(OMS(Typed.ktype))))(history + "equality of domain types")
               val a = a1
               val (x,s1,s2) =
                  if (x1 == x2)
                     (x1, t1, t2)
                  else {
                     if (! stack.context.isDeclared(x1))
                        (x1, t1, t2 ^ (x2 / OMV(x1)))
                     else {
                        val x = x1 / ""
                        (x1 / "", t1 ^ (x1 / OMV(x)), t2 ^ (x2 / OMV(x)))
                     }
                  }
               val res2 = solver.check(Equality(stack ++ x % a, s1, s2, None))(history + "equality of scopes")
               res1 && res2
            }
            Some(cont)
         case _ => None
      }
   }
}

/** the beta-reduction rule s : A  --->  (lambda x:A.t) s = t [x/s]
 * If not applicable, the function term is simplified recursively and the rule tried again.
 * This rule also normalizes nested applications so that it implicitly implements the currying rule (f s) t = f(s,t).
 */ 
object Beta extends ComputationRule(Apply.path) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Option[Term] = {
      var reduced = false // remembers if there was a reduction
      // auxiliary recursive function to beta-reduce as often as possible
      // returns Some(reducedTerm) or None if no reduction
      def reduce(f: Term, args: List[Term]): Option[Term] = (f,args) match {
         case (Lambda(x,a,t), s :: rest) => 
            solver.check(Typing(stack, s, a))(history + "argument must have domain type") //TODO what if false?
            reduced = true
            reduce(t ^ (x / s), rest)
         case (f, Nil) =>
            //all arguments were used
            //only possible if there was a reduction, so no need for 'if (reduced)'
            Some(f)
         case _ => 
            /*// simplify f recursively to see if it becomes a Lambda
            val fS = solver.simplify(f)
            if (f != fS) reduce(fS, args)
            else {*/
              //no more reduction possible
              if (reduced)
                Some(ApplySpine(f,args : _*))
              else
                None
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
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) : Option[Term] = tm match {
      case Arrow(a,b) => Some(Pi(OMV.anonymous, a, b))
      case _ => None
   }
}

// experimental (requiring that torso is variable does not combine with other solution rules) 
object SolveMultiple extends SolutionRule(Apply.path) {
   def apply(solver: Solver)(tm1: Term, tm2: Term)(implicit stack: Stack, history: History): Boolean = {
      tm1 match {
         case ApplySpine(OMV(u), args) =>
             // solver.unknowns.isDeclared(u) known by precondition
             // make sure tm1 is of the form u x1 ... xn
             val bvarArgs = args map {
                case OMV(x) => x
                case _ => return false
             }
             // split context into bind = x1, ..., xn and the rest
             val (bind, rest) = stack.context.variables.partition {case vd => bvarArgs contains vd.name}
             // this guarantees that all xi are declared in stack.context and are distinct 
             if (bind.distinct.length != bvarArgs.length) return false
             //TODO check that no variable declaration in rest depends on an xi
             //TODO use rest instead of stack
             val cont = Context(bind:_*)
             // check that Lambda(cont,tm2) is closed
             val tm2Closed = tm2.freeVars forall {x => cont.isDeclared(x)}
             if (! tm2Closed) return false
             solver.solve(u, Lambda(cont, tm2))
         case _ => false
      }
   }
}

/** This rule tries to solve for an unkown by applying lambda-abstraction on both sides and eta-reduction on the left.
 *  Its effect is, for example, that X x = t is reduced to X = lambda x.t where X is a meta- and x an object variable. 
 */
object Solve extends SolutionRule(Apply.path) {
   def apply(solver: Solver)(tm1: Term, tm2: Term)(implicit stack: Stack, history: History): Boolean = {
      tm1 match {
         case Apply(t, OMV(x)) if stack.context.isDeclared(x) =>
             if (t.freeVars contains x)
                return false
             stack.context(x) match {
                case VarDecl(_, Some(a), _, _*) => 
                   //TODO remove x from context
                   solver.check(Equality(stack, t, Lambda(x, a, tm2), None)) // tpOpt map {tp => Pi(x,a,tp)}
                case _ => false
             }
         case _ => false
      }
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