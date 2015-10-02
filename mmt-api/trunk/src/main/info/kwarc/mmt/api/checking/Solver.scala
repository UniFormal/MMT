package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._
import libraries._
import modules._
import symbols._
import frontend._
import proving._
import objects.Conversions._
import scala.collection.mutable.{HashSet,HashMap}

/* ideas
 * inferType should guarantee well-formedness (what about LambdaTerm?)
 *   but what if there are unknowns whose type cannot be inferred? Is that even possible?
 * limitedSimplify must include computation, definition expansion, but can stop on GlobalChange; but safety is usually needed
 * constants have equality rule: injectivity and implicit arguments used to obtain necessary/sufficient condition (not preserved by morphism); congruence if no rule applicable
 * false should not be returned without generating an error
 */

object InferredType extends TermProperty[Term](utils.mmt.baseURI / "clientProperties" / "solver" / "inferred")

/** returned by [[Solver.dryRun]] as the result of a side-effect-free check */
sealed trait DryRunResult
case object MightFail extends java.lang.Throwable with DryRunResult {
   override def toString = "might fail"
}
object WouldFail extends java.lang.Throwable with DryRunResult {
   override def toString = "will fail"
}
case class Success[A](result: A) extends DryRunResult {
   override def toString = "will succeed"
}


/**
 * A Solver is used to solve a system of constraints about Term's given as judgments.
 *
 * The judgments may contain unknown variables (also called meta-variables or logic variables);
 * variables may represent any MMT term, i.e., object language terms, types, etc.;
 * the solution is a [[Substitution]] that provides a closed Term for every unknown variable.
 * (Higher-order abstract syntax should be used to represent unknown terms with free variables as closed function terms.)
 *
 * The Solver decomposes the judgments individually by applying typing rules, collecting (partial) solutions along the way and possibly delaying unsolvable judgments.
 * If the unknown variables are untyped and rules require a certain type, the Solver adds the type.
 *
 * Unsolvable constraints are delayed and reactivated if later solving of unknowns provides further information.
 *
 * @param controller an MMT controller that is used to look up Rule's and Constant's. No changes are made to the controller.
 * @param context the constant context
 * @param initUnknowns the unknown context
 *   unknown variables may occur in the types of later unknowns.
 *
 * See [[CheckingUnit]] for the semantics of the contexts.
 *
 * Use: Create a new instance for every problem, call apply on all constraints, then call getSolution.
 */
class Solver(val controller: Controller, val constantContext: Context, initUnknowns: Context, val rules: RuleSet)
      extends CheckingCallback with Logger {

   /**
    * to have better control over state changes, all stateful variables are encapsulated a second time
    */
   private object state {
      // stateful fields

      /** tracks the solution, initially equal to unknowns, then a definiens is added for every solved variable */
      private var _solution : Context = initUnknowns
      /** the unknowns that were solved since the last call of activate (used to determine which constraints are activatable) */
      private var _newsolutions : List[LocalName] = Nil
      /** tracks the delayed constraints, in any order */
      private var _delayed : List[DelayedConstraint] = Nil
      /** tracks the errors in reverse order of encountering */
      private var _errors: List[History] = Nil
      /** tracks the dependencies in reverse order of encountering */
      private var _dependencies : List[CPath] = Nil

      // accessor methods for the above
      def solution = _solution
      def newsolutions = _newsolutions
      def delayed = _delayed
      def errors = _errors
      def dependencies = _dependencies

      // adder methods for the stateful lists

      /** registers a constraint */
      def addNewSolution(n: LocalName) {
         if (mutable) _newsolutions ::= n
      }
      /** registers a constraint */
      def addConstraint(d: DelayedConstraint) {
         if (mutable) _delayed ::= d
         else {
            throw MightFail
         }
      }
      /** registers an error */
      def addError(e: History) {
         if (mutable) _errors ::= e
         else throw WouldFail
      }
      /** registers a dependency */
      def addDependency(p: CPath) {
         if (mutable) _dependencies ::= p
      }

      // setter methods for the above

      def removeConstraint(dc: DelayedConstraint) {
         if (mutable) _delayed = _delayed filterNot (_ == dc)
         else throw MightFail
      }
      def clearNewSolutions {
         if (mutable) _newsolutions = Nil
         else throw MightFail
      }
      def solution_=(newSol: Context) {
         _solution = newSol
      }

      // instead of full backtracking, we allow exploratory runs that do not have side effects

      /** if false, all mutator methods have no effect on the state
       *  they may throw a [[DryRunResult]]
       */
      private def mutable = pushedStates.isEmpty
      /** the state that is stored here for backtracking */
      private type StateData = Context
      /** a stack of states for backtracking */
      private var pushedStates: List[StateData] = Nil

      /** evaluates its arguments without applying its side effects on the state
       *  @return if Some(v), then normal evaluation is guaranteed to return v without errors
       */
      def immutably[A](a: => A): DryRunResult = {
         pushedStates ::= solution
         try {Success(a)}
         catch {
            case e: DryRunResult => e
         }
         finally {
            _solution = pushedStates.head
            pushedStates = pushedStates.tail
         }
      }
   }
   import state._

   /** true if unresolved constraints are left */
   def hasUnresolvedConstraints : Boolean = ! delayed.isEmpty
   /** true if unsolved variables are left */
   def hasUnsolvedVariables : Boolean = solution.toSubstitution.isEmpty
   /** true if all judgments solved so far succeeded (all variables solved, no delayed constraints, no errors) */
   def checkSucceeded = ! hasUnresolvedConstraints && ! hasUnsolvedVariables && errors.isEmpty
   /** the solution to the constraint problem
    * @return None if there are unresolved constraints or unsolved variables; Some(solution) otherwise
    */
   def getSolution : Option[Substitution] = if (delayed.isEmpty) solution.toSubstitution else None
   /**
    * @return the current partial solution to the constraint problem
    * This solution may contain unsolved variables, and there may be unresolved constraints.
    */
   def getPartialSolution : Context = solution
   /**
    * @return the context containing only the unsolved variables
    */
   def getUnsolvedVariables : Context = solution.filter(_.df.isEmpty)

   /** @return the current list of unresolved constraints */
   def getConstraints : List[DelayedConstraint] = delayed
   /** @return the current list of errors and their history */
   def getErrors : List[History] = errors
   /** @return the current list of dependencies */
   def getDependencies : List[CPath] = dependencies

   /** for Logger */
   val report = controller.report
   /** prefix used when logging (can be changed flexibly by object-checker to filter the log better) */
   var logPrefix = "solver"
   /** the SubstitutionApplier to be used throughout */
   private implicit val sa = new MemoizedSubstitutionApplier
   /** a DefinitionExpander to be used throughout */
   private val defExp = new uom.DefinitionExpander(controller)
   /** used for rendering objects, should be used by rules if they want to log */
   implicit val presentObj : Obj => String = o => controller.presenter.asString(o)

   /** the context that is not part of judgements */
   private def outerContext = constantContext ++ solution

   /**
    * logs a string representation of the current state
    * @param prefix the log prefix to use (the normal one by default)
    * (occasionally it's useful to use a different prefix, e.g., "error" or when the normal prefix is not logged but the result should be)
    */
   def logState(prefix: String = logPrefix) {
      def logHistory(h: History) {
         logGroup {
            h.getSteps.reverse.foreach(s => report(prefix, s.present))
         }
      }
      logGroup {
         report(prefix, "unknowns: " + initUnknowns.toString)
         report(prefix, "solution: " + solution.toString)
         val unsolved = getUnsolvedVariables.map(_.name)
         if (! unsolved.isEmpty)
            report(prefix, "unsolved: " + unsolved.mkString(", "))
         else
            report(prefix, "all variables solved")
         if (! errors.isEmpty) {
            report(prefix, "errors:")
            logGroup {
               errors.foreach {e =>
                  report(prefix, "error: " + e.getSteps.head.present)
                  logHistory(e)
               }
            }
         } else
            report(prefix, "no errors")
         if (! delayed.isEmpty) {
            report(prefix, "constraints:")
            logGroup {
               delayed.foreach {
                  case d: DelayedJudgement =>
                     report(prefix, d.constraint.present)
                     logGroup {
                        report(prefix, d.constraint.presentAntecedent(_.toString))
                        report(prefix, d.constraint.presentSucceedent(_.toString))
                        if (errors.isEmpty)
                           // if there are no errors, see the history of the constraints
                           logHistory(d.history)
                     }
                  case d: DelayedInference =>
                     report(prefix, "continuation after delayed inference of  " + presentObj(d.tm))
                     if (errors.isEmpty)
                        logHistory(d.history)
               }
            }
         } else
            report(prefix, "no remaining constraints")
      }
   }

   /* TODO get* methods must pass context; only checked constants may be looked up */

   /** retrieves the type type of a constant and registers the dependency
    *
    * returns nothing if the type could not be reconstructed
    */
   def getType(p: GlobalName): Option[Term] = {
      val c = controller.globalLookup.getConstant(p)
      if (c.tpC.analyzed.isDefined) addDependency(p $ TypeComponent)
      c.tpC.analyzed
   }
   /** retrieves the definiens of a constant and registers the dependency
    *
    * returns nothing if the type could not be reconstructed
    */
   def getDef(p: GlobalName) : Option[Term] = {
      val c = controller.globalLookup.getConstant(p)
      if (c.dfC.analyzed.isDefined) addDependency(p $ DefComponent)
      c.dfC.analyzed
   }
   def getModule(p: MPath) : Option[Module] = {
      controller.globalLookup.getO(p) match {
         case Some(m: Module) => Some(m)
         case Some(_) => None
         case None => None
      }
   }
   /**
    * looks up a variable in the appropriate context
    * @return the variable declaration for name
    */
   def getVar(name: LocalName)(implicit stack: Stack) = (constantContext ++ solution ++ stack.context)(name)

   /** registers the solution for an unknown variable
    *
    * If a solution exists already, their equality is checked.
    * @param name the solved variable
    * @param value the solution; must not contain object variables, but may contain meta-variables that are declared before the solved variable
    * @return true unless the solution differs from an existing one
    * precondition: value is well-typed if the overall check succeeds
    */
   def solve(name: LocalName, value: Term)(implicit history: History) : Boolean = {
      log("solving " + name + " as " + value)
      history += ("solving " + name + " as " + presentObj(value))
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.df.isDefined) {
         check(Equality(Stack.empty, value, solved.df.get, solved.tp))(history + "solution must be equal to previously found solution") //TODO
      } else {
         val valueS = simplify(value ^^ left.toPartialSubstitution)(Stack.empty,history) // substitute solutions of earlier variables that have been found already
         parser.SourceRef.delete(valueS) // source-references from looked-up types may sneak in here
         val rightS = right ^^ (OMV(name) / valueS) // substitute in solutions of later variables that have been found already
         val vd = solved.copy(df = Some(valueS))
         solution = left ::: vd :: rightS
         addNewSolution(name)
         typeCheckSolution(vd)
      }
   }
   /** registers the solved type for a variable
    *
    * If a type exists already, their equality is checked.
    * @param name the variable
    * @param value the type; must not contain object variables, but may contain meta-variables that are declared before the solved variable
    * @return true unless the type differs from an existing one
    * precondition: value is well-typed if the the overall check succeeds
    */
   private def solveType(name: LocalName, value: Term)(implicit history: History) : Boolean = {
      log("solving type of " + name + " as " + value)
      val valueS = simplify(value)(Stack.empty, history)
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.tp.isDefined)
         check(Equality(Stack.empty, valueS, solved.tp.get, None))(history + "solution for type must be equal to previously found solution") //TODO
      else {
         val vd = solved.copy(tp = Some(valueS))
         solution = left ::: vd :: right
         //no need to register this in newsolutions (TODO: or is there?)
         typeCheckSolution(vd)
      }
   }
   /** if the type and the definiens of an unknown are solved independently, this type-checks them */
   private def typeCheckSolution(vd: VarDecl)(implicit history: History): Boolean = {
      (vd.tp, vd.df) match {
         case (Some(tp), Some(df)) =>
            check(Typing(Stack.empty, df, tp))(history + "checking solution of metavariable against solved type")
         case _ => true
      }
   }

   /**
    * @param newVars new unknowns; creating new unknowns during checking permits variable transformations
    * @param before the variable before which to insert the new ones
    */
   def addUnknowns(newVars: Context, before: LocalName): Boolean = {
      val (left, right) = solution.span(_.name != before)
      solution = left ::: newVars ::: right
      true
   }

   /** registers an error
    *  @return false
    */
   def error(message: => String)(implicit history: History): Boolean = {
      log("error: " + message)
      addError(history + message)
      // maybe return true so that more errors are found
      false
   }

   /** applies this Solver to one Judgement
    *  This method can be called multiple times to solve a system of constraints.
    *  @param j the Judgement
    *  @return if false, j is disproved; if true, j holds relative to all delayed judgements
    *
    *  Note that this may return true even if can be disproved, namely if the delayed judgements are disproved later.
    *
    *  If this returns false, an error must have been registered.
    */

   def apply(j: Judgement): Boolean = {
      val h = new History(Nil)
      addConstraint(new DelayedJudgement(j, h, true))
      activate
   }

   /** delays a constraint for future processing
    * @param j the Judgement to be delayed
    * @param activatable true if the judgment can be activated immediately
    * @return true (i.e., delayed Judgment's always return success)
    */
   private def delay(j: Judgement, incomplete: Boolean = false)(implicit history: History): Boolean = {
      // testing if the same judgement has been delayed already
      if (delayed.exists {
         case d: DelayedJudgement => d.constraint hasheq j
         case _ => false
      }) {
         log("delaying (exists already): " + j.present)
      } else {
         log("delaying: " + j.present)
         history += "(delayed)"
         val dc = new DelayedJudgement(j, history, incomplete)
         addConstraint(dc)
      }
      true
   }

   /** true if there is an activatable constraint that is not incomplete */
   private def existsActivatable : Boolean = delayed.exists {d => d.isActivatable && !d.incomplete}

   /**
    * processes the next activatable constraint until none are left
    *
    * if there is no activatable constraint, we try an incomplete constraint as a last resort
    */
   @scala.annotation.tailrec
   private def activate : Boolean = {
     val subs = solution.toPartialSubstitution
     def prepareS(s: Stack) =
        Stack(controller.simplifier(s.context ^^ subs, constantContext ++ solution, rules))
     // look for an activatable constraint
     delayed foreach {_.solved(newsolutions)}
     clearNewSolutions
     val dcOpt = delayed.find {d => d.isActivatable && !d.incomplete} orElse {
         log("first invocation or no activatable constraint left, trying other constraint")
         delayed.find {_.incomplete}
     }
     dcOpt match {
        case None =>
           // no activatable constraint
           if (delayed.isEmpty && errors.isEmpty)
              noActivatableConstraint
           else
              false
        case Some(dc) =>
           // activate a constraint
           removeConstraint(dc)
           // mayhold is the result of checking the activated constraint
           val mayhold = dc match {
              case dj: DelayedJudgement =>
                 val j = dj.constraint
                 implicit val history = dj.history
                 implicit val stack = j.stack
                 def prepare(t: Term, covered: Boolean = false) =
                    if (covered) simplify(t ^^ subs) else (t ^^ subs)
                 //logState() // noticably slows down type-checking, only use for debugging
                 log("activating: " + j.present)
                 j match {
                    case Typing(stack, tm, tp, typS) =>
                       check(Typing(prepareS(stack), prepare(tm), prepare(tp, true), typS))
                    case Subtyping(stack, tp1, tp2) =>
                       check(Subtyping(prepareS(stack), prepare(tp1), prepare(tp2)))
                    case Equality(stack, tm1, tm2, tp) =>
                       check(Equality(prepareS(stack), prepare(tm1, true), prepare(tm2, true), tp.map {x => prepare(x, true)}))
                    case Universe(stack, tm) =>
                       check(Universe(prepareS(stack), prepare(tm)))
                    case Inhabitable(stack, tm) =>
                       check(Inhabitable(prepareS(stack), prepare(tm)))
                 }
              case di: DelayedInference =>
                  implicit val stackP = prepareS(di.stack)
                  implicit val history = di.history
                  inferTypeAndThen(safeSimplify(di.tm ^^ subs))(stackP, history)(di.cont)
           }
           if (mayhold) {
              //recurse to activate the next constraint
              activate
           } else
              // the judgment was disproved
              false
           }
   }

   /**
    * @return true if unsolved variables can be filled in by prover
    */
   private def noActivatableConstraint: Boolean = {
      solution.declsInContext.forall {
         case (cont, VarDecl(x, Some(tp), None,_)) =>
            implicit val history = new History(Nil)
            history += "proving open goals"
            prove(constantContext++cont,tp)(history) match {
               case Some(p) =>
                  solve(x, p)
               case None =>
                  error("unproved")
            }
         case _ =>
            true
      }
   }

   /**
    * tries to evaluate an expression without any effect on the state
    * @return Some(v) if evaluation successful
    */
   override def dryRun[A](a: => A): DryRunResult = immutably(a)

   /**
    * performs a type inference and calls a continuation function on the inferred type
    *
    * If type inference is not successful, this is delayed.
    */
   def inferTypeAndThen(tm: Term)(stack: Stack, history: History)(cont: Term => Boolean): Boolean = {
      implicit val (s,h) = (stack, history)
      inferType(tm) match {
         case Some(tp) =>
            cont(tp)
         case None =>
            addConstraint(new DelayedInference(stack, history + "(inference delayed)", tm, cont))
            true
      }
   }

   /** unsafe: via UOM */
   def simplify(t : Term)(implicit stack: Stack, history: History) = {
      val tS = controller.simplifier(t, constantContext ++ solution ++ stack.context, rules)
      if (tS != t)
         history += ("simplified: " + presentObj(t) + " ~~> " + presentObj(tS))
      tS
   }

   /** simplifies safelfy one step along each branches, well-formedness is preserved+reflected */
   //TODO merge with limitedSimplify; offer simplification strategies
   private def safeSimplify(tm: Term)(implicit stack: Stack, history: History): Term = tm match {
      case _:OMID | _:OMV | _:OMLITTrait => tm
      case ComplexTerm(op, subs, cont, args) =>
         val rOpt = rules.getFirst(classOf[ComputationRule], op)
         rOpt flatMap {rule => rule(this)(tm, false)} match {
            case Some(tmS) =>
               log("simplified: " + tm + " ~~> " + tmS)
               history += "simplified: " + presentObj(tm) + " ~~> " + presentObj(tmS)
               tmS.from(tm)
            case None =>
               // no rule or rule not applicable, recurse
               val argsS = args map {a => safeSimplify(a)(stack ++ cont, history + "simplifying argument")}
               val contS = cont mapTerms {case (c,t) => safeSimplify(t)(stack ++ c, history + "simplifying component of bound variable")}
               val subsS = subs mapTerms {a => safeSimplify(a)(stack, history + "simplifying named argument")}
               val tmS = ComplexTerm(op, subsS, contS, argsS).from(tm)
               tmS
         }
   }

   /** simplifies one step overall */
   private def safeSimplifyOne(tm: Term)(implicit stack: Stack, history: History): Term = {
      def expandDefinition: Term = {
         val tmE = defExp(tm,outerContext++stack.context)
         if (tmE hashneq tm)
            history += ("definition expansion yields: " + presentObj(tm) + " ~~> " + presentObj(tmE))
         tmE
      }
      tm.head match {
         case Some(h) =>
            val rOpt = rules.getByHead(classOf[ComputationRule], h)
            // use first applicable rule
            rOpt foreach {rule =>
               rule(this)(tm, false) match {
                  case Some(tmS) =>
                     history += ("simplified: " + presentObj(tm) + " ~~> " + presentObj(tmS))
                     return tmS
                  case None =>
               }
            }
            // no applicable rule, expand a definition
            expandDefinition
         case None => expandDefinition
      }
   }

   /** special case of the version below where we simplify until an applicable rule is found
    *  @param tm the term to simplify
    *  @param rm the RuleMap from which an applicable rule is needed
    *  @param stack the context of tm
    *  @return (tmS, Some(r)) where tmS = tm and r from rm is applicable to tmS; (tmS, None) if tm = tmS and no further simplification rules are applicable
    */
   private def limitedSimplify[R <: Rule](tm: Term, hs: HashSet[R])(implicit stack: Stack, history: History): (Term,Option[R]) =
      safeSimplifyUntil[R](tm)(t => t.head flatMap {h => hs.find(_.head == h)})

   /** applies [[ComputationRule]]s expands definitions until a condition is satisfied;
    *  A typical case is transformation into weak head normal form.
    *  @param tm the term to simplify (It may be simple already.)
    *  @param simple a term is considered simple if this function returns a non-None result
    *  @param stack the context of tm
    *  @return (tmS, Some(a)) if tmS is simple and simple(tm)=tmS; (tmS, None) if tmS is not simple but no further simplification rules are applicable
    */
   def safeSimplifyUntil[A](tm: Term)(simple: Term => Option[A])(implicit stack: Stack, history: History): (Term,Option[A]) = {
      simple(tm) match {
         case Some(a) => (tm,Some(a))
         case None =>
            val tmS = safeSimplifyOne(tm)
            if (tmS hashneq tm)
               safeSimplifyUntil(tmS)(simple)
            else
               (tm, None)
      }
   }
   /** like the other method but simplifies two terms in parallel */
   @scala.annotation.tailrec
   final def safeSimplifyUntil[A](tm1: Term, tm2: Term)(simple: (Term,Term) => Option[A])
                           (implicit stack: Stack, history: History): (Term,Term,Option[A]) = {
      simple(tm1,tm2) match {
         case Some(a) => (tm1,tm2,Some(a))
         case None =>
            val tm1S = safeSimplifyOne(tm1)
            val tm2S = safeSimplifyOne(tm2)
            if ((tm1S hashneq tm2S) || (tm2S hashneq tm2))
                  safeSimplifyUntil(tm1S, tm2S)(simple)
               else
                  (tm1S,tm2S,None)
      }
   }

   /**
    * checks a judgement
    *
    * This is the method that should be used to recurse into a hypothesis.
    * It handles logging and error reporting and delegates to specific methods based on the judgement.
    *
    * The apply method is similar but additionally simplifies the judgment.
    *
    * @param j the judgement
    * @return like apply
    */
   def check(j: Judgement)(implicit history: History): Boolean = {
      history += j
      log(j.presentSucceedent)
      logGroup {
         log(j.presentAntecedent)
         j match {
            case j: Typing   => checkTyping(j)
            case j: Subtyping   => checkSubtyping(j)
            case j: Equality => checkEquality(j)
            case j: Universe => checkUniverse(j)
            case j: Inhabitable => checkInhabitable(j)
            case j: Inhabited => checkInhabited(j)
         }
      }
   }

   /** proves a Typing Judgement by bidirectional type checking
    *
    * In particular, uses TypingRule's to branch and InferenceRule's followed by Equality check as fallback.
    *
    * pre: context and type are covered
    *
    * post: typing judgment is covered
    */
   private def checkTyping(j: Typing)(implicit history: History) : Boolean = {
     val tm = j.tm
     val tp = j.tp
     implicit val stack = j.stack
     // try to solve the type of an unknown
     val solved = solveTyping(tm, tp)
     if (solved) return true
     def checkByInference(tpS: Term): Boolean = {
         val hisbranch = history.branch
         inferType(tm)(stack, hisbranch) match {
            case Some(itp) =>
               check(Subtyping(stack, itp, tpS))(history + ("inferred type must conform to expected type; the term is: " + presentObj(tm)))
            case None =>
               delay(Typing(stack, tm, tpS, j.tpSymb))(hisbranch + "type inference failed")
         }
     }
     tm match {
       // the foundation-independent cases
       case OMV(x) => getVar(x).tp match {
         case None =>
            if (solution.isDeclared(x))
              solveType(x, tp) //TODO: check for occurrences of bound variables?
            else
              error("untyped variable type-checks against nothing: " + x)
         case Some(t) => check(Subtyping(stack, t, tp))
       }
       case OMS(p) =>
         getType(p) match {
           case None => getDef(p) match {
             case None =>
                checkByInference(tp)
                //error("untyped, undefined constant type-checks against nothing: " + p.toString)
             case Some(d) => check(Typing(stack, d, tp, j.tpSymb)) // expand defined constant
           }
           case Some(t) => check(Subtyping(stack, t, tp))
         }
       case l: OMLIT => check(Subtyping(stack, OMS(l.rt.synType), tp))
       // the foundation-dependent cases
       // bidirectional type checking: first try to apply a typing rule (i.e., use the type early on), if that fails, infer the type and check equality
       case tm =>
         limitedSimplify(tp,rules.get(classOf[TypingRule])) match {
           case (tpS, Some(rule)) =>
             try {
                rule(this)(tm, tpS)
             } catch {case TypingRule.SwitchToInference =>
                checkByInference(tpS)
             }
           case (tpS, None) =>
              // either this is an atomic type, or no typing rule is known
              checkByInference(tpS)
         }
     }
   }

   /** infers the type of a term by applying InferenceRule's
    * @param tm the term
    * @param stack its Context
    * @return the inferred type, if inference succeeded
    *
    * This method should not be called by users (instead, use object Solver.check).
    * It is only public because it serves as a callback for Rule's.
    *
    * pre: context is covered
    *
    * post: if result, typing judgment is covered
    */
   override def inferType(tm: Term, covered: Boolean = false)(implicit stack: Stack, history: History): Option[Term] = {
     log("inference: " + presentObj(tm) + " : ?")
     history += "inferring type of " + presentObj(tm)
     // return previously inferred type, if any (previously unsolved variables are substituted)
     InferredType.get(tm) match {
        case s @ Some(_) =>
           return s.map(_ ^^ solution.toPartialSubstitution)
        case _ =>
     }
     val res = logGroup {
        log("in context: " + presentObj(stack.context))
        val resFoundInd = tm match {
          //foundation-independent cases
          case OMV(x) =>
             history += "lookup in context"
             getVar(x).tp
          case OMS(p) =>
             history += "lookup in theory"
             getType(p) orElse {
               getDef(p) match {
                 case None => None
                 case Some(d) => inferType(d) // expand defined constant
               }
             }
          case l: OMLIT =>
             history += "lookup in literal"
             // structurally well-formed literals carry their type
             return Some(OMS(l.rt.synType)) // no need to use InferredType.put on literals
          case OMMOD(p) =>
             // types of theories and views are formed using meta-level operators
             history += "lookup in library"
             getModule(p) match {
                case Some(t: Theory) => Some(TheoryType(t.parameters))
                case Some(v: View)   => Some(MorphType(v.from,v.to))
                case _ =>
                   error("not found")
                   None
             }
          case _ =>
             None
        }
        //foundation-dependent cases if necessary
        //syntax-driven type inference
        resFoundInd orElse {
            val (tmS, ruleOpt) = limitedSimplify(tm,rules.get(classOf[InferenceRule]))
            ruleOpt match {
              case Some(rule) =>
                 history += ("applying rule for " + rule.head.name.toString)
                 rule(this)(tmS, covered)
              case None =>
                 history += "no applicable rule"
                 None
            }
        }
     }
     log("inferred: " + res.getOrElse("failed"))
     // remember inferred type
     //TODO commented out for now because it clashes with forgetting solutions during a dryRun
     //if (res.isDefined) InferredType.put(tm, res.get)
     res
   }

   /** proves a Subtyping Judgement
    *
    * MMT does not natively implement any subtyping and relegates to equality checking by default.
    * However, this behavior can customized by providing [[SubtypingRule]]s.
    *
    * pre: context and both types are covered
    *
    * post: subtyping judgment is covered
    */
   private def checkSubtyping(j: Subtyping)(implicit history: History) : Boolean = {
      val stRules = rules.get(classOf[SubtypingRule])
      // optimization: if there are no rules, we can skip immediately to equality checking
      if (!stRules.isEmpty) {
         implicit val stack = j.stack
         val (tp1S, tp2S, rOpt) = safeSimplifyUntil(j.tp1, j.tp2) { case (a1,a2) =>
            stRules.find(_.applicable(a1,a2))
         }
         rOpt flatMap {r => r(this)(tp1S, tp2S)} match {
            case Some(r) =>
               return r
            case None =>
               if (existsActivatable)
                  // maybe other branches solve unknowns that make a rule applicable
                  return delay(Subtyping(stack, tp1S, tp2S), true)
         }
      }
      // otherwise, we default to checking equality
      // in the absence of subtyping rules, this is the needed behavior anyway
      check(Equality(j.stack, j.tp1, j.tp2, None))
   }

   /** proves a Universe Judgment
    * @param j the judgment
    * @return true if succeeded or delayed
    *
    * pre: context is covered
    *
    * post: j is covered
    */
   private def checkUniverse(j : Universe)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     limitedSimplify(j.univ, rules.get(classOf[UniverseRule])) match {
        case (uS, Some(rule)) => rule(this)(uS)
        case (uS, None) => delay(Universe(stack, uS))
     }
   }

   /** proves an Inhabitable Judgment
    * @param j the judgment
    * @return true if succeeded or delayed
    *
    * pre: context is covered
    *
    * post: j is covered
    */
   private def checkInhabitable(j : Inhabitable)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     limitedSimplify(j.wfo, rules.get(classOf[InhabitableRule])) match {
        case (uS, Some(rule)) => rule(this)(uS)
        case (uS, None) =>
           inferType(j.wfo)(stack, history + "inferring universe") match {
             case None =>
                delay(Inhabitable(stack, uS))
             case Some(univ) =>
                check(Universe(stack, univ))
          }
     }
   }

   /** proves an Equality Judgment by recursively applying in particular EqualityRule's.
    * @param j the judgement
    * @return false if the Judgment is definitely not provable; true if it has been proved or delayed
    *
    * pre: context, terms, and (if given) type are covered; if type is given, both typing judgments are covered
    *
    * post: equality is covered
    */
   private def checkEquality(j: Equality)(implicit history: History): Boolean = {
      val tm1 = j.tm1
      val tm2 = j.tm2
      val tpOpt = j.tpOpt
      implicit val stack = j.stack
      val tm1S = simplify(tm1)
      val tm2S = simplify(tm2)
      // 1) base cases, e.g., identical terms, solving unknowns
      // identical terms
      if (tm1S hasheq tm2S) return true
      // different literals are always non-equal
      (tm1S, tm2S) match {
         case (l1: OMLIT, l2: OMLIT) => if (l1.value != l2.value) return error(s"$l1 and $l2 are inequal literals")
         case _ =>
      }
      // solve an unknown
      val solved = solveEquality(j) || solveEquality(j.swap)
      if (solved) return true

      // 2) find a TermBasedEqualityRule (based on the heads of the terms)
      rules.get(classOf[TermBasedEqualityRule]).filter(r => r.applicable(tm1,tm2)) foreach {rule =>
         // we apply the first applicable rule
         val contOpt = rule(this)(tm1,tm2,tpOpt)
         if (contOpt.isDefined) {
            return contOpt.get.apply
         }
      }

      // 3) find a TypeBasedEqualityRule based on the head of the type
      // first infer the type if it has not been given in tpOpt
      val tp = tpOpt match {
        case Some(tp) => tp
        case None =>
           // TODO switching to unsafe type inference should only make things faster, but actually causes failures
           val itp = inferType(tm1S)(stack, history + "inferring omitted type") orElse
                     inferType(tm2S)(stack, history + "inferring omitted type")
           itp.getOrElse(return delay(Equality(stack, tm1S, tm2S, None)))
      }
      // try to simplify the type until an equality rule is applicable
      val tbEqRules = rules.get(classOf[TypeBasedEqualityRule])
      safeSimplifyUntil(tp)(t => tbEqRules.find(_.applicable(t))) match {
         case (tpS, Some(rule)) => 
            rule(this)(tm1S, tm2S, tpS) match {
               case Some(b) => b
               case None =>
                  checkEqualityTermBased(List(tm1S), List(tm2S), false)(stack, history, tp)
            }
         case (tpS, None) =>
            // this is either a base type or an equality rule is missing
            checkEqualityTermBased(List(tm1S), List(tm2S), false)(stack, history, tp)
      }
   }

   /* ********************** auxiliary methods of checkEquality ***************************/

   /**
    * finds a TermBasedEqualityRule that applies to t1 and any of the terms in t2
    * first found rule is returned
    */
   private def findEqRule(t1: Term, others: List[Term])(implicit stack : Stack, history: History, tp: Term) : Option[Continue[Boolean]] = {
      val eqRules = rules.get(classOf[TermBasedEqualityRule])
      others foreach {t2 =>
         eqRules foreach {r =>
            if (r.applicable(t1,t2)) {
               val contOpt = r(this)(t1, t2, Some(tp))
               if (contOpt.isDefined) return contOpt
            }
         }
      }
      None
   }

   /**
    * checks equality t1 = t2 at an atomic type by simplification
    *
    * Both t1 and t2 are remembered as the chain of terms resulting from repeated definition expansion
    * @param terms1 the chain of terms that led to t1 (must be non-empty)
    * @param terms2 the chain of terms that led to t2 (must be non-empty)
    * @param t2Final if true, no simplification possible anymore
    * @param flipped if true, t1 and t2 have been flipped during recursive calls (false by default)
    *   This parameter is not semantically necessary but carried around to undo flipping when calling other functions.
    * @return true if equal
    */
   private def checkEqualityTermBased(terms1: List[Term], terms2: List[Term], t2Final: Boolean, flipped: Boolean = false)(implicit stack : Stack, history: History, tp: Term) : Boolean = {
       log("equality (trying rewriting): " + terms1.head + " = " + terms2.head)
       val t1 = terms1.head
       // see if we can expand a definition in t1
       val t1E = defExp(t1, outerContext++stack.context)
       val t1EL = safeSimplifyOne(t1E)
       val t1ES = simplify(t1EL)
       val changed = t1ES hashneq t1
       if (changed) {
          log("left term rewritten to " + t1ES)
          // check if it is identical to one of the terms known to be equal to t2
          if (terms2.exists(_ hasheq t1ES)) {
             log("success by identity")
             return true
          }
          // check if there is a TermBasedEqualityRule that applies to the new torso of t1 and the torso of a term equal to t2
          val contOpt = findEqRule(t1ES, terms2)
          contOpt foreach {cont => return cont.apply()}
       }
       // if we failed to prove equality, we have multiple options:
       (changed, t2Final) match {
          // t1 expanded, t2 cannot be expanded anymore --> keep expanding t1
          case (true, true)      => checkEqualityTermBased(t1ES::terms1, terms2, true, flipped)
          // t1 expanded, t2 can still be expanded anymore --> continue expanding t2
          case (true, false)     => checkEqualityTermBased(terms2, t1ES::terms2, false, ! flipped)
          // t1 cannot be expanded anymore but t2 can ---> continue expanding t2
          case (false, false)    => checkEqualityTermBased(terms2, terms1, true, ! flipped)
          // neither term can be expanded --> try congruence rule as last resort
          case (false, true)     =>
             // if necessary, we undo the flipping of t1 and t2
             val s1 = if (flipped) terms2.head else t1ES
             val s2 = if (flipped) t1ES else terms2.head
             if (existsActivatable)
                // if there is some other way to proceed, do it
                delay(Equality(stack, s1, s2, Some(tp)), true)
             else
                // last resort try congruence
                checkEqualityCongruence(TorsoForm.fromHeadForm(s1), TorsoForm.fromHeadForm(s2))
       }
   }

   /**
    * tries to prove equality using basic congruence rule
    * torso and heads must be identical, checkEquality is called in remaining cases
    */
   private def checkEqualityCongruence(t1: TorsoForm, t2: TorsoForm)(implicit stack : Stack, history: History, tp: Term): Boolean = {
      lazy val j = Equality(stack, t1.toHeadForm, t2.toHeadForm, Some(tp))
      lazy val noFreeVars = j.freeVars.isEmpty
      /* The torso form focuses on sequences of elimination operators to the same torso.
       * If the heads of the terms are introduction operators h, the congruence rule can usually be applied greedily
       * because introduction is usually injective.
       * For that purpose, a TermBasedEqualityRule(h,h) should be provided.
       * Therefore, a congruence rule for binders is currently not necessary.
       */
      val similar = t1.torso == t2.torso && t1.apps.length == t2.apps.length
      if (similar) {
         (t1.apps zip t2.apps).forall {case (Appendage(head1,args1),Appendage(head2,args2)) =>
             val similarApp = head1 == head2 && args1.length == args2.length
             if (! similarApp) {
                if (noFreeVars)
                   error("terms have different shape, thus they cannot be equal")
                else
                   delay(j)
             } else {
                log("equality (trying congruence)")
                history += "applying congruence"
                similarApp && (args1 zip args2).forall {case (a1, a2) =>
                   check(Equality(stack, a1, a2, None))(history + "comparing argument")
                }
             }
         }
      } else {
         if (noFreeVars)
            error("terms have different shape, thus they cannot be equal")
         else
            //TODO can we fail if there are free variables but only in the context?
            delay(j)
      }
   }

   /**
    * @param m an unknown variable
    * @return if m is an unknown, the list of free variables of tm that preclude solving m
    *   namely the bound variables and the unknowns declared after m
    */
   private def notAllowedInSolution(m: LocalName, tm: Term)(implicit stack: Stack, history: History): List[LocalName] = {
      val mIndex = solution.index(m).get
      val fvs = tm.freeVars
      fvs.filter {v =>
         if (stack.context.isDeclared(v))
            true // v declared in variable context
         else solution.index(v) match {
            case None => false // v declared in constantContext
            case Some(vIndex) => // v declared in solution
               if (vIndex > mIndex)
                  false // but before m
               // TODO switching to the commented-out code might help solving them cases
               else true // v == m || solution(v).df.isDefined // after m but not solved yet
         }
      }
  }

   /** tries to solve an unknown occurring in tm1 in terms of tm2. */
   private def solveEquality(j: Equality)(implicit stack: Stack, history: History): Boolean = {
      j.tm1 match {
         //foundation-independent case: direct solution of an unknown variable
         case OMV(m) =>
            if (!solution.isDeclared(m))
               return false
            val remainingFreeVars = notAllowedInSolution(m, j.tm2)
            if (remainingFreeVars.isEmpty) {
               // we can solve m already, the remaining unknowns in tm2 can be filled in later
               val res = solve(m, j.tm2)
               j.tpOpt foreach {case tp => solveType(m, tp)}
               res
            } else {
               history += ("free variables remain: " + remainingFreeVars.mkString(", "))
               delay(j)
               // meta-variable solution has free variable --> type error unless free variable disappears later
               // better: we can rearrange the unknown variables (which ultimately may not depend on each other anyway)
               // note: tm2 should be simplified - that may make a free variable disappear
            }
         //apply a foundation-dependent solving rule selected by the head of tm1
         case _ =>
            Solver.findSolvableVariable(rules.get(classOf[SolutionRule]), solution, j.tm1) match {
            case Some((rs, m)) =>
               rs.head(j) match {
                  case Some((j2, msg)) => solveEquality(j2)(j2.stack, history + msg)
                  case None => false
               }
            case _ => false
         }
         /*case TorsoNormalForm(OMV(m), Appendage(h,_) :: _) if solution.isDeclared(m) && ! tm2.freeVars.contains(m) => //TODO what about occurrences of m in tm1?
            rules.getFirst(classOf[SolutionRule], h) match {
               case None => false
               case Some(rule) => rule(this)(tm1, tm2)
            }*/
      }
   }
   /* ********************** end of auxiliary methods of checkEquality ***************************/

   def solveTyping(tm: Term, tp: Term)(implicit stack: Stack, history: History): Boolean = {
      tm match {
         //foundation-independent case: direct solution of an unknown variable
         case OMV(m) =>
            if (!solution.isDeclared(m))
               return false
            val remainingFreeVars = notAllowedInSolution(m, tp)
            if (remainingFreeVars.isEmpty) {
               // we can solve m already, the remaining unknowns in tm2 can be filled in later
               val res = solveType(m, tp)
               res
            } else {
               history += ("free variables remain: " + remainingFreeVars.mkString(", "))
               delay(Typing(stack,tm,tp))
               // meta-variable solution has free variable --> type error unless free variable disappears later
               // better: we can rearrange the unknown variables (which ultimately may not depend on each other anyway)
               // note: tm2 should be simplified - that may make a free variable disappear
            }
         //apply a foundation-dependent solving rule selected by the head of tm1
         case TorsoNormalForm(OMV(m), Appendage(h,_) :: _) if solution.isDeclared(m) && ! tp.freeVars.contains(m) => //TODO what about occurrences of m in tm1?
            rules.getFirst(classOf[TypeSolutionRule], h) match {
               case None => false
               case Some(rule) => rule(this)(tm, tp)
            }
         case _ => false
      }
   }

   /**
    * proves an inhabitation judgment by theorem proving
    *
    * pre: context and type are covered
    *
    * post: inhabitation judgment is covered
    */
   private def checkInhabited(j : Inhabited)(implicit history: History): Boolean = {
      val res = prove(j.context, j.tp)
      if (res.isDefined)
         true
      else
         delay(j)
   }

   private def prove(context: Context, conc: Term)(implicit history: History): Option[Term] = {
      val msg = "proving " + presentObj(context) + " |- _ : " + presentObj(conc)
      log(msg)
      history += msg
      val pu = ProvingUnit(context, conc, logPrefix)
      controller.extman.get(classOf[Prover]) foreach {prover =>
         val (found, proof) = prover.apply(pu, rules, 5)
         if (found) {
            val p = proof.get
            history += "proof: " + presentObj(p)
            log("proof: " + presentObj(p))
            return Some(p)
         } else {
            log("no proof found with prover " + prover.toString) //goal.present(0)(presentObj, None, None))
         }
      }
      log("giving up")
      None
   }

   /**
    *  tries to prove a goal
    *  @return the proof term if successful
    */
   def prove(conc: Term)(implicit stack: Stack, history: History): Option[Term] = {
      prove(constantContext ++ solution ++ stack.context, conc)
   }

   /** applies all ForwardSolutionRules of the given priority
    * @param priority exactly the rules with this Priority are applied */
   //TODO call this method at appropriate times
   private def forwardRules(priority: ForwardSolutionRule.Priority)(implicit stack: Stack, history: History): Boolean = {
      val results = solution.zipWithIndex map {
         case (vd @ VarDecl(x, Some(tp), None, _), i) =>
            implicit val con : Context = solution.take(i)
            limitedSimplify(tp, rules.get(classOf[ForwardSolutionRule])) match {
               case (tpS, Some(rule)) if rule.priority == priority => rule(this)(vd)
               case _ => false
            }
         case _ => false
      }
      results.exists(_ == true)
   }
}

object Solver {
  /** reconstructs a single term and returns the reconstructed term and its type */
  def check(controller: Controller, stack: Stack, tm: Term): Option[(Term,Term)] = {
      val (unknowns,tmU) = parser.ObjectParser.splitOffUnknowns(tm)
      val etp = LocalName("expected_type")
      val rules = RuleSet.collectRules(controller, stack.context)
      val solver = new Solver(controller, stack.context, unknowns ++ VarDecl(etp, None, None, None), rules)
      val j = Typing(stack, tmU, OMV(etp), None)
      solver(j)
      if (solver.checkSucceeded) solver.getSolution.map {sub =>
          val tmR = tmU ^ sub
          (tmR, sub("expected_type").get) // must be defined if there is a solution
      } else
         None
  }
  /** infers the type of a term that is known to be well-formed */
  def infer(controller: Controller, context: Context, tm: Term, rulesOpt: Option[RuleSet]): Option[Term] = {
      val rules = rulesOpt.getOrElse {
         RuleSet.collectRules(controller, context)
      }
      implicit val stack = Stack(Context())
      implicit val history = new History(Nil)
      val solver = new Solver(controller, context, Context(), rules)
      val tpOpt = solver.inferType(tm, true)
      tpOpt map {tp => solver.simplify(tp)}
  }

  /**
    * tests a term for the an occurrence of an unknown variables that can be isolated by applying solution rules
    * @param rules the solution rules to test with
    * @param unknowns the list of variables to try to isolate
    * @param t the term in which to test
    * @return a pair (rs, n) such that applying rs to t (head first) has a chance of isolating n
    */
   def findSolvableVariable(rules: Iterable[SolutionRule], unknowns: Context, t: Term): Option[(List[SolutionRule],LocalName)] = t match {
      case OMA(OMS(_), args) =>
         rules.foreach {r =>
            r.applicable(t) foreach {i =>
               findSolvableVariable(rules, unknowns, args(i)) foreach {case (rs,m) =>
                  val otherArgs = args.take(i) ::: args.drop(i+1)
                  if (otherArgs.forall(a => ! a.freeVars.contains(m)))
                     return Some((rs:::List(r), m))
               }
            }
         }
         return None
      case OMV(m) if unknowns.isDeclared(m) => Some((Nil, m))
      case _ => None
   }


}
