package info.kwarc.mmt.lf
import info.kwarc.mmt.api._
import objects._
import checking._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.lf
import objects.Conversions._
import uom._

object Common {
   /** checks that a type can be quantified over
    *  to maximize extensibility, we allow a bound variable x:a if a:U:kind for some U
    *
    *  in plain LF, this is only possible if U=type, i.e., if a:type
    *  other frameworks may want to reuse the LF typing rules with more options for U
    */
   // FR: this is too generous: it falsely allows U to be a Pi-kind 
   def isTypeLike(solver: Solver, a: Term)(implicit stack: Stack, history: History) = {
     val h = history + "checking the size of the type of the bound variable"
     val kind = OMS(Typed.kind)
     solver.inferTypeAndThen(a)(stack, h) {aT =>
        solver.check(Typing(stack, aT, kind, Some(OfType.path)))
     }
   }

   /** performs safe simplifications and variable transformation to force the argument to become a Pi
    * @param solver the Solver
    * @param tp the function type
    * @return a type equal to tp that may have Pi shape
    */
   def makePi(solver: Solver, tp: Term)(implicit stack: Stack, history: History): Term = {
      val tpS = solver.safeSimplifyUntil(tp)(Pi.unapply)._1
      tpS match {
         case Pi(x,a,b) => tpS
         case solver.Unknown(m, args) =>
           // check that tp is unknown applied to variables
           if (! solver.getUnsolvedVariables.isDeclared(m)) {
              return tpS
           }
           args foreach {
              case OMV(u) =>
              case _ => return tpS
           }
           val mD = m/"d"
           val mC = m/"c"
           val mV = m/"v"
           val mSol = Pi(mV, solver.Unknown(mD, args), solver.Unknown(mC, args ::: List(OMV(mV))))
           // if we have not done the variable transformation before, add the new unknowns
           if (! solver.getPartialSolution.isDeclared(mD)) {
              val newVars = Context(VarDecl(mD), VarDecl(mC))
              solver.addUnknowns(newVars, Some(m))
           }
           history += ("trying to solve "+m+" as "+solver.presentObj(mSol))
           // solve m in terms of newVars
           val success = solver.check(Equality(stack, tpS, mSol, Some(OMS(Typed.ktype)))) //TODO does this work for polymorphism?
           if (success) mSol else tpS
         case _ => tpS
      }
   }
   def pickFresh(solver: Solver, x: LocalName)(implicit stack: Stack) =
      Context.pickFresh(solver.constantContext ++ solver.getPartialSolution ++ stack.context, x)
}

import Common._

trait PiOrArrowRule {self: CheckingRule =>
  override def alternativeHeads = List(Arrow.path)
}

/** Formation: the type inference rule x:A:type|-B:U  --->  Pi x:A.B : U
  * This rule works for any universe U
  */
object PiTerm extends FormationRule(Pi.path, OfType.path) with PiOrArrowRule {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
      tm match {
        case Pi(x,a,b) =>
           if (!covered) isTypeLike(solver,a)
           val (xn,sub) = Common.pickFresh(solver, x)
           solver.inferType(b ^? sub, covered)(stack ++ xn % a, history) flatMap {bT =>
              if (bT.freeVars contains xn) {
                 // usually an error, but xn may disappear later, especially when unknown in b are not solved yet
                 //solver.error("type of Pi-scope has been inferred, but contains free variable " + xn + ": " + solver.presentObj(bT))
                 None
              } else {
                 // TODO if this check fails, we could recover with a type-coercion 
                 solver.check(Universe(stack,bT))(history + "codomain must be a universe")
                 Some(bT)
              }
           }
        case _ => None // should be impossible
      }
   }
}

/** Introduction: the type inference rule x:A|-t:B  --->  lambda x:A.t : Pi x:A.B
 * This rule works for B:U for any universe U
 */
object LambdaTerm extends IntroductionRule(Lambda.path, OfType.path) {
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
      tm match {
        case Lambda(x,a,t) =>
           if (!covered) isTypeLike(solver,a)
           val (xn,sub) = Common.pickFresh(solver, x)
           solver.inferType(t ^? sub, covered)(stack ++ xn % a, history) map {b => Pi(xn,a,b)}
        case _ => None // should be impossible
      }
   }
}

/** common code for rules regarding the elimination-form: inference and reduction */
abstract class ArgumentChecker {
   /** checks if argument tm can be supplied for expected type tp */
   def apply(solver: CheckingCallback)(tm: Term, tp: Term, covered: Boolean)(implicit stack: Stack, history: History): Boolean
}

/** default implementation: type-check against expected type if not covered; skip if covered */
object StandardArgumentChecker extends ArgumentChecker {
   def apply(solver: CheckingCallback)(tm: Term, tp: Term, covered: Boolean)(implicit stack: Stack, history: History) = {
     history.indented {
      covered || solver.check(Typing(stack, tm, tp))
     }
   }
}


/** Elimination: the type inference rule f : Pi x:A.B  ,  conforms(t,A)  --->  f t : B [x/t]
 *
 * This rule works for B:U for any universe U
 *
 * This rule implements currying and checks all arguments at once
 */
class GenericApplyTerm(conforms: ArgumentChecker) extends InferenceAndTypingRule(Apply.path, OfType.path) {
   def apply(solver: Solver, tm: Term, tpO: Option[Term], covered: Boolean)(implicit stack: Stack, history: History) : (Option[Term], Option[Boolean]) = {
      // calling Beta first could make this rule more reusable because it would avoid inferring the type of a possibly large lambda

      /* inspects the function type to extract the expected types of the arguments
       * @param retType the type after applying to previous arguments
       * @param args the remaining arguments
       * @param argTypes the types of the previous arguments (reverse order)
       * @return the types of all arguments 
       */
      def iterate(retType: Term, args: List[Term], argTypes: List[Term]): Option[(List[Term],Term)] = {
         (retType,args) match {
           case (_, Nil) =>
             Some((argTypes.reverse,retType))
           case (Pi(x,a,b), t::rest) =>
              val bS = b ^? (x/t)
              iterate(bS, rest, a::argTypes)
           /*case ApplyGeneral(OMV(u), _) =>
              history += "does not look like a function type at this point"
              None*/
           case _ =>
              val rTPi = Common.makePi(solver, retType)
              if (rTPi != retType)
                 iterate(rTPi, args, argTypes)
              else {
                val unks = solver.getUnsolvedVariables
                if (rTPi.freeVars.exists(unks.isDeclared)) {
                   // this might be convertible into a function type once the unknown variable is solved
                   history += "does not look like a function type at this point"
                   solver.error("this is not a function type (type level rewriting is not supported)")
                   None
                } else {
                   None
                }
              }
        }
      }
      tm match {
         case ApplySpine(f,args) =>
            val hI = history + ("inferring type of function " + solver.presentObj(f))
            val fTOpt = solver.inferType(f, covered)(stack, hI)
            fTOpt match {
              case Some(fT) =>
                history += "function is `" + solver.presentObj(f) + "` of type `" + solver.presentObj(fT) + "`"
                iterate(fT, args, Nil) match {
                  case Some((argTypes,tmI)) =>
                    // It is not clear whether it is better to type-check the return type or the arguments first.
                    // Conceivably the latter allows solving more unknowns early and localize errors.
                    // But it can also introduce complex terms early, thus slowing down (factor 2 in experiments) checking and
                    // even lead to failures where beta-reductions lead to substitutions to the arguments of unknowns.
                    // Using tryToCheckWithoutDelay instead of check here avoids the latter but not the former.
                    // Therefore, the early check of the type is skipped. Future experiments may find better heuristics.
                    // Currently, aggressive early result checking is used to investigate if problems persist.
                    val resCheckResult = tpO map {tp =>
                       solver.check(Subtyping(stack, tmI, tp))(history + "checking return type against expected type")
                    }
                    // val resCheckResult: Option[Boolean] = None
                    // check the arguments
                    // this does not check later arguments if one argument leads to a definite error
                    val argCheckResult = (args zip argTypes).zipWithIndex forall {case ((t,a),i) =>
                      val aS = solver.substituteSolution(a) // previous checks may have solved some unknowns
                      conforms(solver)(t, aS, covered)(stack, history + ("checking argument " + (i+1)))
                    }
                    // no point in returning a positive check result if this is internally ill-typed
                    val checkResult = resCheckResult map {_ && argCheckResult}
                    // we return the inferred type and (if expected type provided) the type check result
                    val tmIS = solver.substituteSolution(tmI)
                    (Some(tmIS), checkResult)
                  case None =>
                    (None, None)
                }
              case None =>
                history.mergeIn(hI)
                history += "inference of type of function `" + solver.presentObj(f) + "` failed"
                //TODO commented out because it looks redundant, check if it's ever helpful
                //args.foreach {t => solver.inferType(t)(stack, history.branch)} // inference of the argument may solve some variables
                (None,None)
            }
         case _ =>
            (None,None) // should be impossible
      }
   }
}

/** the usual inference rule with conforms(t,A) = t:A */
object ApplyTerm extends GenericApplyTerm(StandardArgumentChecker)

/** type-checking: the type checking rule x:A|-f x:B  --->  f : Pi x:A.B */
// TODO all typing rules of this style are in danger of accepting an expression if all eliminations are well-formed but the term itself is not
//  e.g., [x:Int]sqrt(x) : Nat->Real  would check
object PiType extends TypingRule(Pi.path) with PiOrArrowRule {
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) : Option[Boolean] = {
      (tm,tp) match {
         case (Lambda(x1,a1,t),Pi(x2,a2,b)) =>
            // this case is somewhat redundant, but allows reusing the variable name of the lambda
            // TODO this might check a1 >: a2 instead, but then a1 must be checked separately
            solver.check(Equality(stack,a1,a2,None))(history+"domains must be equal")
            val (xn,sub1) = Common.pickFresh(solver, x1)
            val sub2 = x2 / OMV(xn)
            val aN = solver.substituteSolution(a2) // both a1 and a2 may contain unknowns, which may have been solved when checking a1==a2
            Some(solver.check(Typing(stack ++ xn % aN, t ^? sub1, b ^? sub2)))
         case (tm, Pi(x2, a2, b)) =>
            val (xn,sub) = Common.pickFresh(solver, x2)
            val j = Typing(stack ++ xn % a2,  Apply(tm, xn), b ^? sub)
            Some(solver.check(j))
      }
   }
}

/** equality-checking: the extensionality rule (equivalent to Eta) x:A|-f x = g x : B --->  f = g  : Pi x:A. B
 * If possible, the name of the new variable x is taken from f, g, or their type; otherwise, a fresh variable is invented. */
object Extensionality extends ExtensionalityRule(Nil, Pi.path) with PiOrArrowRule {
   val introForm = Lambda
   
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
      val Pi(x, a, b) = tp
      // pick fresh variable name, trying to reuse existing name
      val xBase = (tm1, tm2) match {
         case (Lambda(x1, _, _), Lambda(x2,_,_)) if x1 == x2 => x1
         case _ => if (x != OMV.anonymous) x else LocalName("x")
      }
      val (xn,_) = Context.pickFresh(stack.context, xBase)
      val tm1Eval = Apply(tm1, OMV(xn))
      val tm2Eval = Apply(tm2, OMV(xn))
      val bsub = b ^? (x / OMV(xn))
      Some(solver.check(Equality(stack ++ xn % a, tm1Eval, tm2Eval, Some(bsub))))
   }
}

/** Congruence for Lambda
 *
 *  We cannot use CongruenceRule here because we have to flatten nested lambdas in addition.
 *
 *  This rule is a special case of Extensionality, but it does not make use of the type.
 */
object LambdaCongruence extends TermHeadBasedEqualityRule(Nil, Lambda.path, Lambda.path) {
   def apply(checker: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
      (tm1,tm2) match {
         case (Lambda(x1,a1,t1), Lambda(x2,a2,t2)) =>
            val cont = Continue {
               history += "congruence for lambda"
               val res1 = checker.check(Equality(stack,a1,a2,None))(history + "equality of domain types")
               val (xn,_) = Context.pickFresh(stack.context, x1)
               val t1sub = t1 ^? (x1 / OMV(xn))
               val t2sub = t2 ^? (x2 / OMV(xn))
               val res2 = checker.check(Equality(stack ++ xn % a1, t1sub, t2sub, None))(history + "equality of scopes")
               res1 && res2
            }
            Some(cont)
         case _ => None
      }
   }
}

/** Congruence for Pi
 *
 *  We cannot use HeadBasedEqualityRule here because we have to flatten nested Pis and consider -> in addition.
 */
object PiCongruence extends TermBasedEqualityRule with PiOrArrowRule {
   val head = Pi.path
   def applicable(tm1: Term, tm2: Term) = heads.contains(tm1.head.orNull) && heads.contains(tm2.head.orNull)
   def apply(checker: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
      (tm1,tm2) match {
         case (Pi(x1,a1,t1), Pi(x2,a2,t2)) =>
            val cont = Continue {
               history += "congruence for function types"
               val res1 = checker.check(Equality(stack,a1,a2,None))(history + "equality of domain types")
               val (xn,_) = Context.pickFresh(stack.context, x1)
               val t1sub = t1 ^? (x1 / OMV(xn))
               val t2sub = t2 ^? (x2 / OMV(xn))
               val res2 = checker.check(Equality(stack ++ xn % a1, t1sub, t2sub, None))(history + "equality of scopes")
               res1 && res2
            }
            Some(cont)
         case _ => None
      }
   }
}

/** equality rule for apply that only considers currying
 */
object NormalizeCurrying extends TermBasedEqualityRule {
   val head = Apply.path
   def applicable(tm1: Term, tm2: Term) = heads.contains(tm1.head.orNull) && heads.contains(tm2.head.orNull)
   def apply(checker: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
      (tm1,tm2) match {
         // CR: Deleted if f1 == f2, as this special case is not sufficient
         // FR: the guard should probably be put back in for optimization; any resulting limitations should probably be fixed elsewhere
         case (ApplySpine(f1,args1), ApplySpine(f2,args2)) =>
            // normalize nesting of applications
            val tm1N = ApplySpine(f1,args1:_*)
            val tm2N = ApplySpine(f2,args2:_*)
            //println("tm1: "+tm1.toStr(true)+", tm2: "+tm2.toStr(true)+", tm1': "+tm1N.toStr(true)+", tm2': "+tm2N.toStr(true))
            if (tm1N != tm1 || tm2N != tm2) {
              val cont = Continue {
                 //println("Normalizing the currying of the terms to check term based equality. ")
                 history += "normalize currying"
                 checker.check(Equality(stack, tm1N, tm2N, tp))
              }
              Some(cont)
            } else
              None
         case _ => None
      }
   }
}

/** computation rule for apply that normalizes currying by rewriting terms into uncurried form
 */
object FlattenCurrying extends ComputationRule(Apply.path) {
  def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = {
    tm match {
      case ApplySpine(c,args) =>
        val tmS = ApplySpine(c,args :_*) // this flattens currying because ApplySpine.unapply uncurries
        if (tm != tmS)
          Simplify(tmS)
        else
          RecurseOnly(List(1))
      case _ => RecurseOnly(List(1))
    }
  }
}


/**
 * the beta-reduction rule reducible(s,A)  --->  (lambda x:A.t) s = t [x/s]
 *
 * the reducibility judgment is kept abstract, usually it is the typing judgment s:A
 *
 * This rule also normalizes nested applications so that it implicitly implements the currying rule (f s) t = f(s,t).
 */
class GenericBeta(conforms: ArgumentChecker) extends ComputationRule(Apply.path) {

   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {
      var reduced = false // remembers if there was a reduction
      // auxiliary recursive function to beta-reduce as often as possible
      // returns Some(reducedTerm) or None if no reduction
      def reduce(f: Term, args: List[Term]): Simplifiability = (f,args) match {
         case (Lambda(x,a,t), s :: rest) =>
            if (conforms(solver)(s,a,covered)(stack, history+"checking argument to see if beta-reduction applicable")) {
              reduced = true
              reduce(t ^? (x / s), rest)
            } else {
              history += "beta-reduction is syntactically possible, but the argument does not conform to expectations"
              //RecurseOnly(1 :: Nil)
              Simplifiability.NoRecurse
            }
         case (f, Nil) =>
           //all arguments were used, recurse in case f is again a redex
           //otherwise, return f (only possible if there was a reduction, so no need for 'if (reduced)')
           apply(solver)(f, covered) match {
             case s: Simplify => s
             case _: CannotSimplify => Simplify(f)
           }
           /*
         case(OMS(p),ls) =>
           val dfO = solver.lookup.getConstant(p).df
           dfO match {
             case Some(df) => // TODO EXPERIMENTAL! What to do if defined, but definition is not a lambda?
               val (dfS,opt) = solver.safeSimplifyUntil(df)(Lambda.unapply)
               if (opt.isDefined) reduce(dfS,args)
               else if (reduced) Simplify(ApplySpine(f,args :_*))
               else RecurseOnly(1 :: Nil)
             case None =>
               if (reduced)
                 Simplify(ApplySpine(f,args : _*))
               else
                 Simplifiability.NoRecurse
           }
           */
         case _ =>
            /*// simplify f recursively to see if it becomes a Lambda
            val fS = solver.simplify(f)
            if (f != fS) reduce(fS, args)
            else {*/
              //no more reduction possible
              if (reduced)
                Simplify(ApplySpine(f,args : _*))
              else
                RecurseOnly(List(1))
      }
      tm match {
         //using ApplySpine here also normalizes curried application by merging them into a single one
         case ApplySpine(f, args) => reduce(f, args)
         case _ => Simplifiability.NoRecurse// Recurse // only possible after recursing
      }
   }
}

/**
 * the usual beta-reduction rule s : A  --->  (lambda x:A.t) s = t [x/s]
 */
object Beta extends GenericBeta(StandardArgumentChecker) {
  /**
    * Fully beta-reduces a given term using [[Controller.simplifier]].
    *
    * Effectively applies the simplifier to the term using only one rule, namely `this`.
    */
  def reduce(t: Term)(implicit ctrl: Controller): Term = {
    val su = SimplificationUnit(Context.empty, expandDefinitions = false, fullRecursion = true)
    ctrl.simplifier(t, su, RuleSet(lf.Beta))
  }
}

/*
/**
 *  should be redundant, but maybe more efficient
 */
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
*/



class Injectivity(val head: GlobalName) extends TermBasedEqualityRule {
   def applicable(tm1: Term, tm2: Term) = (tm1,tm2) match {
      case (ApplySpine(OMS(this.head),args1),ApplySpine(OMS(this.head),args2)) => args1.length == args2.length
      case _ => false
   }
   def apply(check: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
      val (ApplySpine(_,args1),ApplySpine(_,args2)) = (tm1,tm2)
      //if check.getDef(head) == None && check.getTp(head) returns type
      None
   }
}

/** |- t:A  --->  |- (t:A) : A */
object TypeAttributionTerm extends InferenceRule(OfType.path, OfType.path) {
  def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) : Option[Term] = {
    val OfType(t,a) = tm
    if (!covered) {
      solver.inferTypeAndThen(a)(stack, history + "checking the attributed type") {aI =>
        // nothing to do, we just have to make sure that the type is well-formed before checking the term against it 
        true
      } 
      solver.check(Typing(stack, t, a))(history + "checking against attributed type")
    }
    Some(a)
  }
}

/** |- (t:A) = t */
object DropTypeAttribution extends ComputationRule(OfType.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = {
    val OfType(t,a) = tm
    if (!covered) {
      // this will call TypeAttributionTerm once, which recursively triggers the necessary checks of the type before we throw it away  
      solver.inferType(tm, covered)(stack, history + "checking the attributed type")
    }
    Simplify(t)
  }
}


// experimental (requiring that torso is variable does not combine with other solution rules)
object SolveMultiple extends ValueSolutionRule(Apply.path) {
   def applicable(tm1: Term) = tm1 match {
      case ApplySpine(OMV(_),_) => Some(0)
      case _ => None
   }
   def apply(j: Equality): Option[(Equality,String)] = {
      j.tm1 match {
         case ApplySpine(OMV(u), args) =>
             // solver.unknowns.isDeclared(u) known by precondition
             // make sure tm1 is of the form u x1 ... xn
             val bvarArgs = args map {
                case OMV(x) => x
                case _ => return None
             }
             // split context into bind = x1, ..., xn and the rest
             val (bind, rest) = j.stack.context.variables.partition {case vd => bvarArgs contains vd.name}
             // this guarantees that all xi are declared in stack.context and are distinct
             if (bind.distinct.length != bvarArgs.length) return None
             //TODO check that no variable declaration in rest depends on an xi
             //TODO use rest instead of stack
             val cont = Context(bind:_*)
             // check that Lambda(cont,tm2) is closed
             val tm2Closed = j.tm2.freeVars forall {x => cont.isDeclared(x)}
             if (! tm2Closed) return None
             Some((Equality(j.stack, OMV(u), Lambda(cont, j.tm2), None), "binding variables"))
         case _ => None
      }
   }
}

trait ApplySolutionRule extends SolutionRule {
   def applicable(t: Term) = t match {
      case Apply(_, _) => Some(0)  // do not match for OMV(_) here: unknowns in function position belong to this rule even if the rule can't do anything
      case _ => None
   }
   
   protected case class Isolation(newContext: Context, bound: LocalName, boundTp: Term, rest: Term)

   /** returns Isolation(c, x, a, r) such that c, x:a |- tm = r x  and x not occurring in c, r */
   protected def bind(context: Context, tm: Term, other: Term): Option[Isolation] = {
      tm match {
         case Apply(t, OMV(x)) =>
             val i = context.lastIndexWhere(_.name == x)
             if (i == -1) return None
             // dropped contains x and all variable declarations that depend on it
             var dropped = List(x) 
             var newCon : Context = context.take(i) // the resulting context
             // iterate over the variables vd after x
             context.drop(i+1) foreach {vd =>
                if (vd.freeVars.exists(dropped.contains)) {
                   // vd depends on x, we use weakening to drop vd as well
                   dropped ::= vd.name
                } else {
                   // append vd to the new context
                   newCon = newCon ++ vd
                }
             }
             // check whether weakening is applicable: dropped variables may not occur in t or Lambda(x,a,tm2)
             if (t.freeVars.exists(dropped.contains))
                // most important special case: x occurs free in t so that eta is not applicable
                return None
             if (other.freeVars.exists(dropped.filterNot(_ == x) contains _))
                return None
             // get the type of x and abstract over it
             context.variables(i).tp match {
                case Some(a) =>
                   Some(Isolation(newCon, x, a, t))
                case _ => None
             }
         case _ => None
      }
   }
}

/** solution: This rule tries to solve for an unknown by applying lambda-abstraction on both sides and eta-reduction on the left.
 *  Its effect is, for example, that X x = t is reduced to X = lambda x.t where X is a meta- and x an object variable.
 */
object Solve extends ValueSolutionRule(Apply.path) with ApplySolutionRule {
   def apply(j: Equality) = {
      bind(j.stack.context, j.tm1, j.tm2) map {case Isolation(newCon, x, a, rest) =>
        val newStack = j.stack.copy(context = newCon)
        (Equality(newStack, rest, Lambda(x, a, j.tm2), j.tpOpt map {tp => Pi(x,a,tp)}), "binding x")
      }
   }
}

/** This rule tries to solve for an unknown by applying lambda-abstraction on both sides and eta-reduction on the left.
 *  Its effect is, for example, that X x = t is reduced to X = lambda x.t where X is a meta- and x an object variable.
 */
object SolveType extends TypeSolutionRule(Apply.path) with ApplySolutionRule {
   def apply(j: Typing) = {
      bind(j.stack.context, j.tm, j.tp) map {case Isolation(newCon, x, a, rest) =>
        val newStack = j.stack.copy(context = newCon)
        (Typing(newStack, rest, Pi(x, a, j.tp)), "binding x")
      }
   }
}

object TheoryTypeWithLF extends ComputationRule(ModExp.theorytype) {
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = tm match {
      case TheoryType(params) =>
         if (params.isEmpty) Recurse else Simplify(Pi(params, TheoryType(Nil)))
      case _ => Simplifiability.NoRecurse
   }
}

/** A simplification rule that implements A -> B = Pi x:A.B  for fresh x.
  * LocalName.Anon is used for x */
// not used anymore because Pi rules now also apply to arrow
/*
object ExpandArrow extends ComputationRule(Arrow.path) {
   def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History) = tm match {
      case Arrow(a,b) => Simplify(Pi(OMV.anonymous, a, b))
      case _ => Simplifiability.NoRecurse
   }
}
*/

/**
  * inverse of ExpandArrow - removes unnecessary occurrences of [[Pi]]s.
  * E.g., over the signature `{A: type, B: type}`, the [[Pi]] in `Πx: A. B`
  * is unnecessary and can be replaced by an [[Arrow]] `A -> B`.
  */
object RemoveUnusedPi extends SimplificationRule(Pi.path) {
  override def apply(context: Context, t: Term): Simplifiability = t match {
    case Pi(name, tp, body) =>
      if (body.freeVars.contains(name)) Simplifiability.NoRecurse
      else Simplify(Arrow(tp, body))

    case _ => Simplifiability.NoRecurse
  }
}
