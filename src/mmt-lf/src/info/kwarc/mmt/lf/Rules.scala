package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import objects._
import checking._
import objects.Conversions._
import uom._
import utils.MyList.fromList

object Common {
   /** convenience function for recursively checking the judgement |- a: type */
   def isType(solver: Solver, a: Term)(implicit stack: Stack, history: History) =
      solver.check(Typing(stack, a, OMS(Typed.ktype), Some(OfType.path)))(history + "type of bound variable must be a type")

   /** performs safe simplifications and variable transformation to force the argument to become a Pi
    * @param solver the Solver
    * @param tp the function type
    * @return a type equal to tp that may have Pi shape  
    */
   def makePi(solver: Solver, tp: Term)(implicit stack: Stack, history: History): Term = {
      val tpS = solver.safeSimplifyUntil(tp)(Pi.unapply)._1
      tpS match {
         case Pi(x,a,b) => tpS
         case ApplyGeneral(OMV(m), args) =>
           // check that tp is unknown applied to variables 
           if (! solver.getUnsolvedVariables.isDeclared(m))
              return tpS
           args foreach {
              case OMV(u) => 
              case _ => return tpS
           }
           val mD = OMV(m/"d")
           val mC = OMV(m/"c")
           val mV = OMV(m/"v")
           val mSol = Pi(mV.name, ApplyGeneral(mD, args), ApplyGeneral(mC, args ::: List(mV)))
           // if we have not done the variable transformation before, add the new unknowns
           if (! solver.getPartialSolution.isDeclared(mD.name)) {
              val newVars = Context(VarDecl(mD.name, None, None, None), VarDecl(mC.name, None, None, None))
              solver.addUnknowns(newVars, m)
           }
           // solve m in terms of newVars
           val success = solver.check(Equality(stack, tpS, mSol, Some(OMS(Typed.ktype))))
           if (success) mSol else tpS
         case _ => tpS 
      }
   }
   def pickFresh(solver: Solver, x: LocalName)(implicit stack: Stack) =
      Context.pickFresh(solver.constantContext ++ solver.getPartialSolution ++ stack.context, x)
}

import Common._

/** the type inference rule x:A:type|-B:U  --->  Pi x:A.B : U
 * This rule works for any universe U */
object PiTerm extends InferenceRule(Pi.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
      tm match {
        case Pi(x,a,b) =>
           if (!covered) isType(solver,a)
           val (xn,sub) = Common.pickFresh(solver, x)
           solver.inferType(b ^? sub)(stack ++ xn % a, history) flatMap {bT =>
              if (bT.freeVars contains xn) {
                 // usually an error, but xn may disappear later, especially when unknown in b are not solved yet 
                 //solver.error("type of Pi-scope has been inferred, but contains free variable " + xn + ": " + solver.presentObj(bT))
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
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
      tm match {
        case Lambda(x,a,t) =>
           if (!covered) isType(solver,a)
           val (xn,sub) = Common.pickFresh(solver, x)
           solver.inferType(t ^? sub)(stack ++ xn % a, history) map {b => Pi(xn,a,b)}
        case _ => None // should be impossible
      }
   }
}

/** the type inference rule f : Pi x:A.B  ,  t : A  --->  f t : B [x/t]
 * This rule works for B:U for any universe U */
object ApplyTerm extends InferenceRule(Apply.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = tm match {
     case Apply(f,t) =>
        history += "inferring type of function " + solver.presentObj(f)
        val fTOpt = solver.inferType(f)(stack, history.branch)
        fTOpt match {
           case None =>
              history += "failed"
              None
           case Some(fT) =>
              history += "function type is: " + solver.presentObj(fT)
              val fTPi = Common.makePi(solver, fT)
              if (fTPi != fT)
                 history += "function type is: " + solver.presentObj(fTPi)
              fTPi match {
                 case Pi(x,a,b) =>
                    if (!covered) solver.check(Typing(stack, t, a))(history + "argument must have domain type")
                    Some(b ^? (x / t))
                 case _ =>
                    // definition expansion must also consider unknown variables whose definitions are not known yet
                    None
              }
        }
     case _ => None // should be impossible
   }
}

/** the type checking rule x:A|-f x:B  --->  f : Pi x:A.B */
object PiType extends TypingRule(Pi.path) {
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) : Boolean = {
      (tm,tp) match {
         case (Lambda(x1,a1,t),Pi(x2,a2,b)) =>
            //checking of a1:type necessary because checkEquality does not check typing
            //isType(solver,a1)
            solver.check(Equality(stack,a1,a2,None))(history+"domains must be equal")
            // solver.checkTyping(a2,LF.ktype)(stack) is redundant after the above have succeeded, but checking it anyway might help solve variables
            val (xn,sub1) = Common.pickFresh(solver, x1)
            val sub2 = x2 / OMV(xn)
            solver.check(Typing(stack ++ xn % a2, t ^? sub1, b ^? sub2))(history + "type checking rule for Pi")
         case (tm, Pi(x2, a2, b)) =>
            val (xn,sub) = Common.pickFresh(solver, x2)
            val j = Typing(stack ++ xn % a2,  Apply(tm, xn), b ^? sub)
            solver.check(j)(history + "type checking rule for Pi")
      }
   }
}

/** the extensionality rule (equivalent to Eta) x:A|-f x = g x  --->  f = g  : Pi x:A. B
 * If possible, the name of the new variable x is taken from f, g, or their type; otherwise, a fresh variable is invented. */
object Extensionality extends TypeBasedEqualityRule(Nil, Pi.path) {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Boolean = {
      val Pi(x, a, b) = tp
      // pick fresh variable name, trying to reuse existing name 
      val xBase = (tm1, tm2) match {
         case (Lambda(x1, _, _), Lambda(x2,_,_)) if x1 == x2 => x1
         case _ => x
      }
      val (xn,_) = Context.pickFresh(stack.context, xBase)
      val tm1Eval = Apply(tm1, OMV(xn))
      val tm2Eval = Apply(tm2, OMV(xn))
      val bsub = b ^? (x / OMV(xn))
      solver.check(Equality(stack ++ xn % a, tm1Eval, tm2Eval, Some(bsub)))
   }
}

/** Congruence for Lambda
 *  
 *  We cannot use CongruenceRule here because we have to flatten nested lambdas in addition.
 *  
 *  This rule is a special case of Extensionality, but it does not make use of the type.
 */
object LambdaCongruence extends TermBasedEqualityRule(Nil, Lambda.path, Lambda.path) {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
      (tm1,tm2) match {
         case (Lambda(x1,a1,t1), Lambda(x2,a2,t2)) =>
            val cont = Continue {
               history += "congruence for lambda"
               val res1 = solver.check(Equality(stack,a1,a2,None))(history + "equality of domain types")
               val (xn,_) = Context.pickFresh(stack.context, x1)
               val t1sub = t1 ^? (x1 / OMV(xn))
               val t2sub = t2 ^? (x2 / OMV(xn))
               val res2 = solver.check(Equality(stack ++ xn % a1, t1sub, t2sub, None))(history + "equality of scopes")
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
object PiCongruence extends TermBasedEqualityRule(Nil, Pi.path, Pi.path) {
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
      (tm1,tm2) match {
         case (Pi(x1,a1,t1), Pi(x2,a2,t2)) =>
            val cont = Continue {
               history += "congruence for function types"
               val res1 = solver.check(Equality(stack,a1,a2,None))(history + "equality of domain types")
               val (xn,_) = Context.pickFresh(stack.context, x1)
               val t1sub = t1 ^? (x1 / OMV(xn))
               val t2sub = t2 ^? (x2 / OMV(xn))
               val res2 = solver.check(Equality(stack ++ xn % a1, t1sub, t2sub, None))(history + "equality of scopes")
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
            reduce(t ^? (x / s), rest)
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
      // collects the substitution that will be applied (thus, we only substitute once even if there are multiple nested redexes) 
      var sub = Substitution()
      // repeatedly checks for a redex and returns the reduced terms
      def reduce(f: Term, args: List[Term]): Change = (f,args) match {
         case (Lambda(x,a,t), s :: rest) => 
            sub = sub ++ (x / s)
            reduce(t, rest)
         case (f, Nil) =>
            //all arguments were used (only possible if there was a reduction)
            GlobalChange(f ^? sub)
         case _ => 
            if (sub.isEmpty)
              NoChange
            else
              GlobalChange(ApplySpine(f ^? sub, args : _*))
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
   def applicable(tm1: Term) = tm1 match {
      case ApplySpine(OMV(_),_) => Some(0)
      case _ => None
   }
   def apply(solver: Solver)(tm1: Term, tm2: Term, tpOpt: Option[Term])(implicit stack: Stack, history: History): Boolean = {
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
   def applicable(t: Term) = t match {
      case Apply(_, _) => Some(0)
      case _ => None
   }
   def apply(solver: Solver)(tm1: Term, tm2: Term, tpOpt: Option[Term])(implicit stack: Stack, history: History): Boolean = {
      tm1 match {
         case Apply(t, OMV(x)) =>
             val i = stack.context.lastIndexWhere(_.name == x)
             if (i == -1) return false
             var dropped = List(x) // the variables that we will remove from the context
             var newCon : Context = stack.context.take(i) // the resulting context
             // iterate over the variables vd after x
             stack.context.drop(i+1) foreach {vd =>
                if (vd.freeVars.exists(dropped.contains _)) {
                   // vd depends on x, we use weakening to drop vd as well
                   dropped ::= vd.name
                } else {
                   // append vd to the new context
                   newCon = newCon ++ vd
                }
             }
             // check whether weakening is applicable: dropped variables may not occur in t or Lambda(x,a,tm2)
             if (t.freeVars.exists(dropped.contains _))
                // most important special case: x occurs free in t so that eta is not applicable
                return false
             if (tm2.freeVars.exists(dropped.filterNot(_ == x) contains _))
                return false
             // get the type of x and abstract over it
             stack.context.variables(i) match {
                case VarDecl(_, Some(a), _, _) => 
                   val newStack = stack.copy(context = newCon)
                   solver.solveEquality(t, Lambda(x, a, tm2), tpOpt map {tp => Pi(x,a,tp)})(
                         newStack, history + ("solving by binding " + x))
                case _ => false
             }
         case _ => false
      }
   }
}

/** This rule tries to solve for an unkown by applying lambda-abstraction on both sides and eta-reduction on the left.
 *  Its effect is, for example, that X x = t is reduced to X = lambda x.t where X is a meta- and x an object variable. 
 */
object SolveType extends TypeSolutionRule(Apply.path) {
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History): Boolean = {
      tm match {
         case Apply(t, OMV(x)) =>
             val i = stack.context.lastIndexWhere(_.name == x)
             if (i == -1) return false
             var dropped = List(x) // the variables that we will remove from the context
             var newCon : Context = stack.context.take(i) // the resulting context
             // iterate over the variables vd after x
             stack.context.drop(i+1) foreach {vd =>
                if (vd.freeVars.exists(dropped.contains _)) {
                   // vd depends on x, we use weakening to drop vd as well
                   dropped ::= vd.name
                } else {
                   // append vd to the new context
                   newCon = newCon ++ vd
                }
             }
             // check whether weakening is applicable: dropped variables may not occur in t or Lambda(x,a,tm2)
             if (t.freeVars.exists(dropped.contains _))
                // most important special case: x occurs free in t so that eta is not applicable
                return false
             if (tp.freeVars.exists(dropped.filterNot(_ == x) contains _))
                return false
             // get the type of x and abstract over it
             stack.context.variables(i) match {
                case VarDecl(_, Some(a), _, _) => 
                   val newStack = stack.copy(context = newCon)
                   solver.solveTyping(t, Pi(x, a, tp))(newStack, history + ("solving by binding " + x)) // tpOpt map {tp => Pi(x,a,tp)}
                case _ => false
             }
         case _ => false
      }
   }
}

import proving._

/** the proof step ?:Pi x:A.B ----> lambda x:A.(?:B)
 *
 * This rule works for any universe U
 */
class PiOrArrowIntroRule(op: GlobalName) extends IntroProvingRule(Nil, op) {
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

object PiIntroRule extends PiOrArrowIntroRule(Pi.path) with BackwardInvertible {
   def priority = 5
   def apply(prover: P, goal: Goal) = goal.conc match {
      case Pi(n,a,b) =>
         onApply {
            val n2 = if (n == OMV.anonymous) LocalName("p") else n
            val (x,sub) = Context.pickFresh(goal.fullContext, n2)
            val sg = new Goal(x%a, b ^? sub)
            Alternative(List(sg), () => Lambda(x,a,sg.proof))
         }
      case _ => None
   }
}
object ArrowIntroRule extends PiOrArrowIntroRule(Arrow.path)


object UnnamedArgument {
   def unapply(vd: VarDecl) = vd match {
      case VarDecl(OMV.anonymous, Some(t),_,_) => Some(t)
      case _ => None
   }
}
object SolvedParameter {
   def unapply(vd: VarDecl) = vd match {
      case VarDecl(x, _,Some(d),_) => Some((x,d))
      case _ => None
   }
}
object UnsolvedParameter {
   def unapply(vd: VarDecl) = vd match {
      case VarDecl(x, _,None,_) if x != OMV.anonymous => Some(x)
      case _ => None
   }
}

/** the proof step ?:A ----> e(?,...?)  for e:Pi x1:A1,...,xn:An.A' where A' ^ s = A for some substitution s
 *
 * This rule works for any universe U and the case n=0.
 * This rule replace ?'s in the result with their terms if they can be inferred through unification.
 */
class PiOrArrowElimRule(op: GlobalName) extends ElimProvingRule(Nil, op) {
   /**
    * @param context current context 
    * @param goal term relative to context
    * @param fact closed (function) type
    * @return the argument types of fact such that applying a function of type fact yields a result of type goal 
    */
   protected def makeSubgoals(context: Context, goal: Term, fact: Term): Option[Context] = { 
      // tp must be of the form Pi bindings.scope
      val (bindings, scope) = FunType.unapply(fact).get
      // the free variables of scope (we drop the types because matching does not need them)
      val unknowns: Context = bindings.flatMap {
         case (Some(x),_) => List(VarDecl(x,None,None, None))
         case _ => Nil
      }
      // fact may contain free variables from stack.context, so make sure there are no name clashes
      // sub is a renaming from unknowns to unknownsFresh
      val (unknownsFresh, sub) = Context.makeFresh(unknowns, context.map(_.name))
      val scopeFresh = scope ^ sub
      // match goal against scope, trying to solve for scope's free variables
      // TODO using a first-order matcher is too naive in general - for the general case, we need to use the Solver
      val matcher = new Matcher(unknownsFresh)
      val matchFound = matcher(context, goal, scopeFresh)
      if (!matchFound) return None
      val solution = matcher.getSolution
      // now scope ^ sub ^ solution == goal
      var result: Context = Context()
      bindings foreach {
         // named bound variables that are substituted by solution can be filled in
         // others are holes representing subgoals
         case (Some(x), xtp) =>
            val xFresh = (x ^ sub).asInstanceOf[OMV].name // sub is a renaming
            result ++= VarDecl(xFresh, Some(xtp ^ result.toPartialSubstitution), solution(xFresh), None)
         case (None, anontp) =>
            result ++= VarDecl(OMV.anonymous, Some(anontp ^ result.toPartialSubstitution), None, None)
       }
       Some(result)
   }
   def apply(ev: Term, fact: Term, goal: Term)(implicit stack: Stack) : Option[ApplicableProvingRule] = {
      makeSubgoals(stack.context, goal, fact) map {con =>
         new ApplicableProvingRule {
             def label = ev.toString
             def apply = {
                val args = con map {
                   case VarDecl(_,_,Some(t),_) => t
                   case VarDecl(_, Some(t), None, _) => Hole(t) // t may contain free variable referring to previous holes
                }
                ApplyGeneral(ev, args)
             }
         }
      }
   }
}

object PiElimRule extends PiOrArrowElimRule(Pi.path) with BackwardSearch {
   val priority = 3
   private def makeAlternative(g: Goal, tm: Term, argDecls: Context): Alternative = {
        var sgs : List[Goal] = Nil
        var args: List[() => Term] = Nil
        argDecls foreach {
           case SolvedParameter(_,a) =>
              args ::= {() => a}
           case UnnamedArgument(t) =>
              val sg = new Goal(g.context, t)
              sgs ::= sg
              args ::= {() => sg.proof}
        }
        Alternative(sgs.reverse, () => ApplyGeneral(tm, args.reverseMap(a => a())))
   }
   def apply(prover: P, g: Goal) = {
      (g.varAtoms ::: prover.facts.getSymbolAtoms).flatMap {case (tm,tp) =>
         // match return type of tp against g.conc
         val sgsOpt = makeSubgoals(g.fullContext, g.conc, tp)
         sgsOpt match {
            case None =>
               // no match
               Nil
            case Some(argDecls) =>
              // matched; check if all named arguments of tp were instantiated
              val unsolvedParameters = argDecls.collect {
                 case u @ UnsolvedParameter(x) => u
              }
              if (unsolvedParameters.isEmpty) {
                 // yes: make an alternative using the unnamed arguments of tp as subgoals
                 onApply {makeAlternative(g, tm, argDecls)}.toList
              } else {
                 // no: try to match the first unnamed argument against a known fact to instantiate the remaining ones
                 // find the first mentions all remaining unsolved parameters 
                 val uAOpt = argDecls.find {
                    case UnnamedArgument(t) =>
                       val tvars = t.freeVars
                       unsolvedParameters.forall {p => tvars contains p.name}
                    case _ => false
                 }
                 uAOpt match {
                    case None =>
                       // no progress possible (we might try matching other arguments though)
                       Nil
                    case Some(uA) =>
                       val UnnamedArgument(t) = uA
                       // match t against known facts
                       prover.facts.termsOfTypeAtGoal(g, unsolvedParameters, t) flatMap {case (subs, p) =>
                          // update the argument declarations with the new information 
                          val argDecls2 = argDecls.map {
                             case vd if vd == uA =>
                                // the matched goal is already solved by p
                                VarDecl(OMV.anonymous, None, Some(p), None)
                             case vd @ UnsolvedParameter(x) =>
                                // the remaining parameters can now be solved
                                vd.copy(df = subs(x))
                             case vd @ SolvedParameter(_,_) =>
                                // previously solved parameters remain unchanged
                                vd 
                             case vd @ UnnamedArgument(t) =>
                                // substitute the new solutions in the remaining unnamed arguments 
                                vd.copy(tp = Some(t ^ subs))
                          }
                          onApply {makeAlternative(g, tm, argDecls2)}.toList
                       }
                 }
              }
         }
      }
   }
}

object ArrowElimRule extends PiOrArrowElimRule(Arrow.path) with ForwardSearch {
   def generate(prover: P) {
      // apply all symbols
      prover.facts.getSymbolAtoms foreach {case (tm,tp) => applyAtom(prover.goal, tm, tp, prover.facts)}
      // apply all variables of each goal
      applyVarAtoms(prover.goal, prover.facts) 
   }
   /** recursively applies all variables to create new facts */
   private def applyVarAtoms(g: Goal, facts: FactsDB) {
      g.varAtoms.foreach {case (tm, tp) => applyAtom(g, tm, tp, facts)}
      g.getAlternatives.foreach {_.subgoals.foreach {sg => applyVarAtoms(sg, facts)}}
   }
   /** applies one atom (symbol or variable) to all known facts */
   private def applyAtom(g: Goal, tm: Term, tp: Term, facts: FactsDB) {
      val (bindings, scope) = FunType.unapply(tp).get
      // params: leading named arguments
      val (paramlist, neededArgs) = bindings.span(_._1.isDefined)
      val parameters = FunType.argsAsContext(paramlist) 
      // all the actual logic is implemented separately
      new ArgumentFinder(facts, tm, scope).apply(g, parameters, Nil, neededArgs)
   }
   
   /**
    * @param facts the facts to draw arguments from
    * @param fun the function to apply (once arguments are found)
    * @param scope the return type of fun
    * 
    * the parameter types (leading named arguments) and the remaining input types of fun are supplied in the apply method
    */
   private class ArgumentFinder(facts: FactsDB, fun: Term, scope: Term) {
      /**
       * looks for unnamed arguments and solves parameters along the way
       * 
       * each iteration handles the next unnamed argument type and
       * recurses for each found argument term
       * 
       * @param g the goal closest to the root at which the new fact will be in scope
       * @param parameters the parameters, definitions are added during recursion
       * @param foundArgs the arguments already found (initially empty)
       * @param neededArgs the arguments still needed (initially all arguments other than parameters)
       */
      def apply(g: Goal, parameters: Context, foundArgs: List[(Option[LocalName],Term)],
                                             neededArgs: List[(Option[LocalName],Term)]) {
         // the values of the found named arguments as a substitution
         val foundSubs = foundArgs.collect {case (Some(x),a) => x/a}
         neededArgs match {
            case Nil =>
               parameters.toSubstitution match {
                  case Some(sub) =>
                     // if all parameters were instantiated,
                     // we apply fun to all parameters and found arguments
                     // and add the new fact, whose type is obtained by substituting all parameters and named arguments in scope 
                     val f = Fact(g, ApplyGeneral(fun, sub.map(_.target) ::: foundArgs.map(_._2)), scope ^ (sub ++ foundSubs))
                     facts.add(f)
                  case None =>
                     // otherwise, we cannot create a new fact
               }
            case (nOpt, tp) :: rest =>
               // substitute solved parameters and found named arguments in tp
               // tpS stills contain free variables for the so-far-unsolved parameters 
               val tpS = tp ^? (parameters.toPartialSubstitution ++ foundSubs)
               // get all possible arguments for tpS
               val args = facts.termsOfTypeBelowGoal(g, parameters, tpS)
               args foreach {case (sub, arg, hOpt) =>
                  // sub instantiates additional parameters that still occurred in tpS
                  // we add these solutions to parameters
                  val newParameters = parameters map {vd =>
                     sub(vd.name) match {
                        case None => vd
                        case Some(s) => vd.copy(df = Some(s))
                     }
                  }
                  // arg is the next argument of type tpS
                  // we append it to foundArgs
                  val newFoundArgs = foundArgs ::: List((nOpt,arg))
                  // hOpt may be a goal below g
                  // in that case, we replace g with h
                  val newGoal = hOpt.getOrElse(g)
                  // we recurse to find the remaining arguments
                  apply(newGoal, newParameters, newFoundArgs, rest)
               }
         }
      }
   }
}


object TheoryTypeWithLF extends ComputationRule(ModExp.theorytype) {
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack, history: History) = tm match {
      case TheoryType(params) => if (params.isEmpty) None else Some(Pi(params, TheoryType(Nil)))
   }
}