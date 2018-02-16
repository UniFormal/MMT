package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import frontend._
import info.kwarc.mmt.api.symbols.{Constant, PlainInclude}
import info.kwarc.mmt.api.utils.Killable
import modules._
import objects.Conversions._
import objects._
import proving._
import parser.ParseResult

import scala.collection.mutable.HashSet
import scala.util.Try

/* ideas
 * inferType should guarantee well-formedness (what about LambdaTerm?)
 *   but what if there are unknowns whose type cannot be inferred? Is that even possible?
 *
 * limitedSimplify must include computation, definition expansion, but can stop on GlobalChange; but safety is usually needed
 *
 * constants have equality rule: injectivity and implicit arguments used to obtain necessary/sufficient condition (not preserved by morphism); congruence if no rule applicable
 *
 * false should not be returned without generating an error
 *
 * injectivity rules must smartly handle situations like op(t1)=op(t2)
 * currently all definitions in t1 and t2 are expanded even though op is often injective, especially if op is a type operator
 */

object InferredType extends TermProperty[(Branchpoint,Term)](utils.mmt.baseURI / "clientProperties" / "solver" / "inferred")

/** returned by [[Solver.dryRun]] as the result of a side-effect-free check */
sealed trait DryRunResult {
  def get:Option[_] = None
}
sealed trait ThrowableDryRunResult extends java.lang.Throwable with DryRunResult
case class MightFail(history: History) extends ThrowableDryRunResult {
   override def toString = "might fail"
}
case object WouldFail extends ThrowableDryRunResult {
   override def toString = "will fail"
}
case class Success[A](result: A) extends DryRunResult {
   override def get = Some(result)
   override def toString = "will succeed"
}

class Branchpoint(val parent: Option[Branchpoint], val delayed: List[DelayedConstraint], val depLength: Int, val solution: Context) {
  def isRoot = parent.isEmpty
  /** @return true if this branch contains anc */
  def descendsFrom(anc: Branchpoint): Boolean = parent.contains(anc) || (parent match {
    case None => false
    case Some(p) => p.descendsFrom(anc)
  })
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
class Solver(val controller: Controller, checkingUnit: CheckingUnit, val rules: RuleSet)
      extends CheckingCallback with Logger {

   val constantContext = checkingUnit.context
   val initUnknowns = checkingUnit.unknowns

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

      /** registers a solution */
      def addNewSolution(n: LocalName) {
         _newsolutions ::= n
      }
      /** registers a constraint */
      def addConstraint(d: DelayedConstraint)(implicit history: History) {
        if (!mutable && !pushedStates.head.allowDelay) {
          throw MightFail(history)
        } else {
          _delayed ::= d
          if (!mutable) {
            pushedStates.head.delayedInThisRun ::= d
          }
        }
      }
      /** registers an error */
      def addError(e: History) {
         if (mutable) _errors ::= e
         else {
           throw WouldFail
         }
      }
      /** registers a dependency */
      def addDependency(p: CPath) {
         _dependencies ::= p
      }

      // more complex mutator methods for the stateful lists

      def removeConstraint(dc: DelayedConstraint) {
         _delayed = _delayed filterNot (_ == dc)
         if (!mutable) {
           val state = pushedStates.head
           state.delayedInThisRun = state.delayedInThisRun.filterNot(_ == dc)
         }
      }
      def clearNewSolutions {
         _newsolutions = Nil
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
      private case class StateData(solutions: Context, newsolutions: List[LocalName], dependencies: List[CPath], delayed: List[DelayedConstraint], allowDelay: Boolean) {
         var delayedInThisRun: List[DelayedConstraint] = Nil
      }
      /** a stack of states for dry runs */
      private var pushedStates: List[StateData] = Nil

      /** true if we are currently in a dry run */
      def isDryRun = !mutable

      /**
       * evaluates its arguments without generating new constraints
       *
       * all state changes are rolled back unless evaluation is successful and commitOnSuccess is true
       */
      def immutably[A](allowDelay: Boolean, commitOnSuccess: A => Boolean)(a: => A): DryRunResult = {
         val tempState = StateData(solution, newsolutions, dependencies, _delayed, allowDelay)
         pushedStates ::= tempState
         def rollback {
            val oldState = pushedStates.head
            pushedStates = pushedStates.tail
            _solution = oldState.solutions
            _newsolutions = oldState.newsolutions
            _dependencies = oldState.dependencies
            _delayed = oldState.delayed
         }
         try {
           val aR = a
           if (tempState.delayedInThisRun.nonEmpty) {
              activateRepeatedly // DM commented this out to fix an unspecified Heisenbug, reinstated by FR as DM did not remember details
              tempState.delayedInThisRun.headOption.foreach {h =>
                 throw MightFail(h.history)
              }
           }
           if (commitOnSuccess(aR)) {
             pushedStates = pushedStates.tail
           } else {
             rollback
           }
           Success(aR)
         } catch {
            case e: ThrowableDryRunResult =>
              rollback
              e
         }
      }

      /* first attempt at full backtracking; the code below is already partially used but has no effect yet
       * - branchpoints build the tree and save the current state
       * - backtracking restores current state
       * - cached inferred types store the branchpoint, results are only valid if on branch
       *
       * - new solutions are still collected in the state but not used anymore (minor de-optimization) to make constraints stateless
       *
       * problems
       * - how to avoid reproving constraints that are unaffected by backtracking?
       * - how to backtrack when an error stems from a delayed constraint and the call to backtrackable has already terminated?
       */
      private var currentBranch: Branchpoint = makeBranchpoint(None)
      def getCurrentBranch = currentBranch
      def setCurrentBranch(bp: Branchpoint) {
        currentBranch = bp
      }
      /** restore the state from immediately before bp was created */
      private def backtrack(bp: Branchpoint) {
        // restore constraints
        _delayed = bp.delayed
        // remove new dependencies
        _dependencies = _dependencies.drop(_dependencies.length - bp.depLength)
        // restore old solution
        _solution = bp.solution
        // no need to restore errors - any error should result in backtracking when !currentBranch.isRoot
      }
      private def makeBranchpoint(parent: Option[Branchpoint] = Some(currentBranch)) = {
        new Branchpoint(parent, delayed, dependencies.length, solution)
      }
      class Backtrack extends Throwable
      /** set a backtracking point and run code
       *  @return result of code or None if error occurred
       *
       *  even if this succeeds, new constraints may have been added that will fail in the future
       */
      def backtrackable[A](code: => A): Option[A] = {
        val bp = makeBranchpoint()
        currentBranch = bp
        try {
          Some(code)
        } catch {
          case b: Backtrack =>
            backtrack(bp)
            None
        } finally {
          // even if we do not backtrack, we must update the current branch
          currentBranch = currentBranch.parent.get // is defined because it was set above
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
    *
    * @return None if there are unresolved constraints or unsolved variables; Some(solution) otherwise
    */
   def getSolution : Option[Substitution] = if (delayed.isEmpty) solution.toSubstitution else None
   /**
    * @return the current partial solution to the constraint problem
    * This solution may contain unsolved variables, and there may be unresolved constraints.
    */
   def getPartialSolution : Context = solution
   /** @return the context containing only the unsolved variables */
   def getUnsolvedVariables : Context = solution.filter(_.df.isEmpty)
   /** @return the the unsolved variables */
   def getSolvedVariables : List[LocalName] = solution.filter(_.df.isDefined).map(_.name)

   /** @return the current list of unresolved constraints */
   def getConstraints : List[DelayedConstraint] = delayed
   /** @return the current list of errors and their history */
   def getErrors : List[History] = errors
   /** @return the current list of dependencies */
   def getDependencies : List[CPath] = dependencies

   /** for Logger */
   val report = controller.report
   /** the component URI if provided, solver otherwise */
   val logPrefix = checkingUnit.component.map(_.toString).getOrElse("solver")
   /** the SubstitutionApplier to be used throughout */
   private implicit val sa = new MemoizedSubstitutionApplier
   /** a DefinitionExpander to be used throughout */
   private val defExp = new uom.DefinitionExpander(controller)
   /** used for rendering objects, should be used by rules if they want to log */
   implicit val presentObj : Obj => String = o => controller.presenter.asString(o)

   /** the context that is not part of judgements */
   def outerContext = constantContext ++ solution

   /** precomputes relevant rule sets, ordered by priority */
   private lazy val computationRules = rules.getOrdered(classOf[ComputationRule])
   private lazy val inferenceRules = rules.getOrdered(classOf[InferenceRule])
   private lazy val subtypingRules = rules.getOrdered(classOf[SubtypingRule])
   //TODO why are these not ordered?
   private lazy val typebasedsolutionRules = rules.get(classOf[TypeBasedSolutionRule])
   private lazy val typingRules = rules.get(classOf[TypingRule])
   private lazy val universeRules = rules.get(classOf[UniverseRule])
   private lazy val inhabitableRules = rules.get(classOf[InhabitableRule])
   private lazy val termBasedEqualityRules = rules.get(classOf[TermBasedEqualityRule])
   private lazy val termHeadBasedEqualityRules = rules.get(classOf[TermHeadBasedEqualityRule])
   private lazy val typeBasedEqualityRules = rules.get(classOf[TypeBasedEqualityRule])
   private lazy val solutionRules = rules.get(classOf[SolutionRule])
   private lazy val forwardSolutionRules = rules.get(classOf[ForwardSolutionRule])
   /* convenience function for going to the next rule after one has been tried */
   private def dropTill[A](l: List[A], a: A) = l.dropWhile(_ != a).tail
   private def dropJust[A](l: List[A], a:A) = l.filter(_ != a)

   /** a [[CongruenceClosure]] that stops comparing where a variable could be solved */
   private def makeCongClos = new CongruenceClosure(eq =>
      if (List(eq.tm1,eq.tm2).exists(t => Solver.findSolvableVariable(solutionRules, solution, t).isDefined))
        Some(false)
      else
        None
   )

   /**
    * logs a string representation of the current state
    *
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
         report(prefix, "unknowns: " + initUnknowns.toStr(shortURIs = true))
         report(prefix, "solution: " + solution.toStr(shortURIs = true))
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

   private def logAndHistoryGroup[A](body: => A)(implicit history: History) = {
     logGroup {
       history.indented {
         body
       }
     }
   }

   /* TODO get* methods must pass context */

   /** retrieves the type type of a constant and registers the dependency
    *
    * returns nothing if the type could not be reconstructed
    */
   def getType(p: GlobalName): Option[Term] = {
      val c = getConstant(p)
      val t = c.tpC.getAnalyzedIfFullyChecked
      if (t.isDefined)
        addDependency(p $ TypeComponent)
      t
   }

  private def getConstant(p : GlobalName) : Constant =
    controller.library.get(ComplexTheory(constantContext), LocalName(p.module) / p.name, s => throw GetError(s)) match {
      case c: Constant => c
      case d => throw GetError("Not a constant: " + d)
    }

   /** retrieves the definiens of a constant and registers the dependency
    *
    * returns nothing if the type could not be reconstructed
    */
   def getDef(p: GlobalName) : Option[Term] = {
      val c = getConstant(p)
      val t = c.dfC.getAnalyzedIfFullyChecked
      if (t.isDefined)
        addDependency(p $ DefComponent)
      t
   }
   def getModule(p: MPath) : Option[Module] = {
      controller.globalLookup.getO(p) match {
         case Some(m: Module) => Some(m)
         case Some(_) => None
         case None => None
      }
   }

  // TODO this should track lookups for dependency management
  def lookup = controller.globalLookup
  @deprecated("Used in LFX, but could probably be done better","")
  def materialize(cont : Context, tm : Term, expandDefs : Boolean, parent : Option[MPath]) = controller.simplifier.materialize(cont,tm,expandDefs,parent)

   /**
    * looks up a variable in the appropriate context
    *
    * @return the variable declaration for name
    */
   def getVar(name: LocalName)(implicit stack: Stack) = (constantContext ++ solution ++ stack.context)(name)

   /** registers the solution for an unknown variable
    *
    * If a solution exists already, their equality is checked.
 *
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

   /** moves declarations in solution to the right so that 'name' occurs as far to the right as allowed by dependencies */
   private def moveToRight(name: LocalName) {
     val (before, it::rest) = solution.span(_.name != name)
     var toLeft = Context.empty
     var toEnd = Context(it)
     var toEndNames = List(name)
     rest.foreach {vd =>
       if (utils.disjoint(vd.freeVars, toEndNames)) {
         // no dependency: move over vd
         toLeft = toLeft ++ vd
       } else {
         // dependency: also move vd to the end
         toEnd = toEnd ++ vd
         toEndNames ::= vd.name
       }
     }
     solution = before ++ toLeft ++ toEnd
     log("moved " + name + " to the right, new solution: " + presentObj(solution))
   }

   /** registers the solved type for a variable
    *
    * If a type exists already, their equality is checked.
 *
    * @param name the variable
    * @param value the type; must not contain object variables, but may contain meta-variables that are declared before the solved variable
    * @return true unless the type differs from an existing one
    * precondition: value is well-typed if the the overall check succeeds
    */
   private def solveType(name: LocalName, value: Term)(implicit history: History) : Boolean = {
      log("solving type of " + name + " as " + value)
     history += "solving type of " + name + " as " + value
      val valueS = simplify(value)(Stack.empty, history)
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.tp.isDefined) {
        /* TODO this is a first attempt at what roughly should happen. Since WithoutDelay doesn't solve variables itself
           TODO I'm calling checkEquality if subtyping fails. */
        var checksOut = tryToCheckWithoutDelay(Subtyping(Stack.empty, valueS, solved.tp.get))
        if (checksOut.contains(true)) true
        else { // TODO in one of two cases the previous solution should probably be updated (probably to the supertype?)
          checksOut = tryToCheckWithoutDelay(Subtyping(Stack.empty,solved.tp.get,valueS))
            if (checksOut contains true) true else
            check(Equality(Stack.empty, valueS, solved.tp.get, None))(history + "solution for type must be equal to previously found solution")
        } /* TODO Alternatively one could keep track of all solutions and check which one (or combination) checks out, but that seems
             TODO expensive. */
      }
      else {
         val vd = solved.copy(tp = Some(valueS))
         solution = left ::: vd :: right
         log("new solution: " + solution)
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
    * @param before the variable before which to insert the new ones, otherwise insert at end
    */
   def addUnknowns(newVars: Context, before: Option[LocalName]): Boolean = {
      val (left, right) = before match {
        case Some(b) => solution.span(_.name != b)
        case None => (solution.toList,Nil)
      }
      solution = left ::: newVars ::: right
      true
   }

   /**
    * define x:tp as the unique value that satisfies the constraint
    *
    * this is implemented by adding a fresh unknown, and running the constraint
    * this can be used to compute a value in logic programming style, where the computation is given by a functional predicate
    */
   def defineByConstraint(x: LocalName, tp: Term)(constraint: Term => Boolean) = {
     addUnknowns(x%tp, None)
     constraint(OMV(x))
   }

   /** registers an error
 *
    *  @return false
    */
   override def error(message: => String)(implicit history: History): Boolean = {
      log("error: " + message)
      history += message
      addError(history)
      // maybe return true so that more errors are found
      false
   }

   /**
    * main entry method: runs the solver on the judgment in the checking unit
    */
   def applyMain = apply(checkingUnit.judgement)

   /** applies this Solver to one Judgement
    *  This method can be called multiple times to solve a system of constraints.
    *
    *  @param j the Judgement
    *  @return if false, j is disproved; if true, j holds relative to all delayed judgements
    *
    *  Note that this may return true even if j can be disproved, namely if the delayed judgements are disproved later.
    *
    *  If this returns false, an error must have been registered.
    */
   def apply(j: Judgement): Boolean = {
      val h = new History(Nil)
      val bi = new BranchInfo(h, getCurrentBranch)
      addConstraint(new DelayedJudgement(j, bi, true, None))(h)
      activateRepeatedly
      if (errors.nonEmpty) {
        // definitely disproved
        false
      } else {
        if (delayed.nonEmpty) {
          // definitely inconclusive
          true
        } else
          // all good except maybe some unknowns unsolved
          solveRemainingUnknowns
      }
   }

   /** delays a constraint for future processing
 *
    * @param j the Judgement to be delayed
    * @return true (i.e., delayed Judgment's always return success)
    */
   private def delay(j: Judgement, onActivation: Option[() => Boolean] = None)(implicit history: History): Boolean = {
      // testing if the same judgement has been delayed already
      if (delayed.exists {
         case d: DelayedJudgement => d.constraint hasheq j
         case _ => false
      }) {
         log("delaying (exists already): " + j.present)
      } else {
         log("delaying: " + j.present)
         history += "(delayed)"
         val bi = new BranchInfo(history, getCurrentBranch)
         val dc = new DelayedJudgement(j, bi, onActivation.isDefined, onActivation)
         addConstraint(dc)
    }
      true
   }

   /** true if there is an activatable constraint that is not incomplete */
   private def existsActivatable(allowIncomplete: Boolean) : Boolean = {
     val solved = getSolvedVariables
     delayed.exists {d => d.isActivatable(solved) && (!d.incomplete || allowIncomplete)}
   }

   /**
    * processes the next activatable constraint until none are left
    *
    * if there is no activatable constraint, we try an incomplete constraint as a last resort
    *
    * @return false if disproved (if true returned, constraints and unknowns may be left)
    */
   @scala.annotation.tailrec
   private def activateRepeatedly: Boolean = {
     val subs = solution.toPartialSubstitution
     def prepareS(s: Stack)(implicit h: History) = {
        //  ^^ subs might be redundant because simplifier expands defined variables
        // but there may be subtleties because terms may already be marked as simple
        Stack(controller.simplifier(s.context ^^ subs, constantContext ++ solution, rules))
     }
     // look for an activatable constraint
     val solved = getSolvedVariables
     //delayed foreach {_.solved(newsolutions)} -- removed optimization for backtracking
     clearNewSolutions // can be removed
     val dcOpt = delayed.find {d => d.isActivatable(solved) && !d.incomplete} orElse {
         log("first invocation or no activatable constraint left, trying other constraint")
         delayed.find {_.incomplete}
     }
     dcOpt match {
        case None =>
          // there might still be unsolved unknowns and constraints left
          errors.isEmpty
        case Some(dc) =>
           // activate a constraint
           removeConstraint(dc)
           setCurrentBranch(dc.branch)
           // mayhold is the result of checking the activated constraint
           val mayhold = dc match {
              case dj: DelayedJudgement =>
                 val j = dj.constraint
                 implicit val history = dj.history
                 implicit val stack = j.stack
                 def prepare(t: Obj, covered: Boolean = false): t.ThisType = {
                    substituteSolved(t, covered)
                 }
                 //logState() // noticeably slows down type-checking, only use for debugging
                 log("activating: " + j.present)
                 dj.onActivation match {
                   case Some(f) if ! dc.isActivatable(solved) => f()
                   case _ => j match {
                      case Typing(stack, tm, tp, typS) =>
                         check(Typing(prepareS(stack), prepare(tm), prepare(tp, true), typS))
                      case Subtyping(stack, tp1, tp2) =>
                         check(Subtyping(prepareS(stack), prepare(tp1), prepare(tp2)))
                      case Equality(stack, tm1, tm2, tp) =>
                         check(Equality(prepareS(stack), prepare(tm1, true), prepare(tm2, true), tp.map {x => prepare(x, true)}))
                      case Universe(stack, tm) =>
                         check(Universe(prepareS(stack), prepare(tm)))
                      case Inhabitable(stack, tp) =>
                         check(Inhabitable(prepareS(stack), prepare(tp)))
                      case Inhabited(stack, tp) =>
                         check(Inhabited(prepareS(stack), prepare(tp, true)))
                      case IsContext(stack, cont) =>
                         check(IsContext(prepareS(stack), prepare(cont)))
                      case EqualityContext(stack, c1, c2, a) =>
                         check(EqualityContext(prepareS(stack), prepare(c1, true), prepare(c2, true), a))
                   }
                 }
              case di: DelayedInference =>
                  implicit val history = di.history
                  implicit val stackP = prepareS(di.stack)
                  inferTypeAndThen(safeSimplify(di.tm ^^ subs))(stackP, history)(di.cont)
           }
           if (mayhold) {
              //recurse to activate the next constraint
              activateRepeatedly
           } else
              // the judgment was disproved
              false
           }
   }

   /**
    * @return true if unsolved variables can be filled in by prover
    */
   private def solveRemainingUnknowns: Boolean = {
      solution.declsInContext.forall {
         case (_, vd) if vd.df.isDefined =>
           // solved
           true
         case (cont, vd) if vd.df.isEmpty =>
            // unsolved, maybe use prover based on needed type
            implicit val history = new History(Nil)
            vd.tp match {
              case None =>
                error("unsolved (untyped) unknown: " + vd.name)
              case Some(tp) =>
                def tryAHole = if (vd.name.startsWith(ParseResult.VariablePrefixes.explicitUnknown)) {
                  solve(vd.name, Hole(tp))
                }
                val rO = typebasedsolutionRules.find(r => r.applicable(tp))
                rO match {
                  case Some(rule) =>
                    history += "calling type-based solution rule " + rule.getClass + " on " + vd.name + ": " + presentObj(tp)
                    implicit val stack = Stack(cont)
                    rule.solve(this)(tp) match {
                      case Some(p) =>
                        solve(vd.name, p)
                      case None =>
                        tryAHole
                        error("no solution found")
                    }
                  case _ =>
                    tryAHole
                    error("unsolved (typed) unknown: " + vd.name)
                }
            }
      }
   }

   /**
    * tries to evaluate an expression without any generating new constraints
    *
    * @param a the expression
    * @param commitOnSuccess do not roll back state changes if successful
    */
   override def dryRun[A](allowDelay: Boolean, commitOnSuccess: A => Boolean)(a: => A): DryRunResult = immutably(allowDelay, commitOnSuccess)(a)

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
            val bi = new BranchInfo(history + "(inference delayed)", getCurrentBranch)
            addConstraint(new DelayedInference(stack, bi, tm, cont))
            true
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
      if (checkingUnit.isKilled) {
        checkingUnit.killact
        return error("checking was cancelled by external signal")
      }
      JudgementStore.getOrElseUpdate(j) {
        history += j
        log("checking: " + j.presentSucceedent)
        logAndHistoryGroup {
          log("in context: " + j.presentAntecedent)
          j match {
            case j: Typing   => checkTyping(j)
            case j: Subtyping => checkSubtyping(j)
            case j: Equality => checkEquality(j)
            case j: Universe => checkUniverse(j)
            case j: Inhabitable => checkInhabitable(j)
            case j: Inhabited => checkInhabited(j)
            case j: IsContext => checkContext(j)
            case j: EqualityContext => checkEqualityContext(j)
          }
        }
     }
   }

   /** caches the results of judgements to avoid duplicating work */
   // judgements are not cached if we are in a dry run to make sure they are run again later to solve unknowns
   private object JudgementStore {
     private val store = new scala.collection.mutable.HashMap[Judgement,Boolean]
     /** lookup up result for j; if not known, run f to define it */
     def getOrElseUpdate(j : Judgement)(f: => Boolean): Boolean = {
       store.find {case (k,_) => k implies j} match {
         case Some((_,r)) =>
           r
         case None =>
           val r = f
           if (!isDryRun) {
             store(j) = r
           }
           r
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
       //TODO if the type has a typing rule and the term is OMV/OMS or if Infered.get is defined, we have two conflicting strategies
       //it's unclear which one works better

       // the foundation-independent cases
       case OMV(x) =>
         val vd = getVar(x)
         vd.tp match {
           case None =>
              if (solution.isDeclared(x))
                solveType(x, tp) //TODO: check for occurrences of bound variables?
              else {
                vd.df match {
                  case None =>
                    error("untyped, undefined variable type-checks against nothing: " + x)
                  case Some(d) =>
                    check(Typing(stack, d, tp))
                }
              }
           case Some(t) =>
               check(Subtyping(stack, t, tp))
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
       // note that OMLs do not have a generally-defined type
       case l: OMLIT => check(Subtyping(stack, l.rt.synType, tp))
       // the foundation-dependent cases
       // bidirectional type checking: first try to apply a typing rule (i.e., use the type early on), if that fails, infer the type and check equality
       case tm =>
         limitedSimplify(tp,typingRules) match {
           case (tpS, Some(rule)) =>
             try {
                history += "Applying TypingRule " + rule.toString
                rule(this)(tm, tpS)
             } catch {
               case TypingRule.SwitchToInference =>
                 checkByInference(tpS)
               case rule.DelayJudgment(msg) =>
                 delay(Typing(stack, tm, tpS, j.tpSymb))(history + msg)
             }
           case (tpS, None) =>
              // either this is an atomic type, or no typing rule is known
              checkByInference(tpS)
         }

     }
   }

   /** infers the type of a term by applying InferenceRule's
 *
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
        case Some((bp,tmI)) if getCurrentBranch.descendsFrom(bp) =>
           return Some(tmI ^^ solution.toPartialSubstitution)
        case _ =>
     }
     val res = logAndHistoryGroup {
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
          // note that OML's do not have a generally-defined type
          case l: OMLIT =>
             history += "lookup in literal"
             // structurally well-formed literals carry their type
             Some(l.rt.synType) // no need to use InferredType.put on literals
          case l : UnknownOMLIT => Some(l.synType)
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
        //rules may trigger backtracking, in which case the next rule is tried until one yields a result
        resFoundInd orElse {
           var activerules = inferenceRules
           var tmS = tm
           var ret: Option[Term] = null
           while (ret == null) {
              val (tmp, ruleOpt) = limitedSimplify(tmS, activerules)
              tmS = tmp
              ruleOpt match {
                 case Some(rule) =>
                    history += ("applying inference rule " + rule.toString)
                    try {
                      ret = rule(this)(tmS, covered)
                    } catch {
                       case t : MaytriggerBacktrack#Backtrack =>
                          history += t.getMessage
                          activerules = /*dropTill(activerules,rule)*/ dropJust(activerules, rule)
                    }
                 case None =>
                    history += "no applicable rule"
                    ret = None
              }
           }
           ret
        }
     }
     log("inferred: " + presentObj(tm) + " : " + res.map(presentObj).getOrElse("failed"))
     history += "inferred: " + presentObj(tm) + " : " + res.map(presentObj).getOrElse("failed")
     //remember inferred type
     if (!isDryRun) {
       res foreach {r => InferredType.put(tm, (getCurrentBranch,r))}
     }
     res map {r => substituteSolved(r, true)} // inference result must be well-typed, can be simplified
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
   private def checkSubtyping(j: Subtyping)(implicit history: History): Boolean = {
      // TODO this fully expands definitions if no rule is applicable
      // better: use an expansion algorithm that stops expanding if it is know that no rule will become applicable

     // first see if equality can be established
     val CC = makeCongClos
     val obviouslyEqual = tryToCheckWithoutDelay(CC(Equality(j.stack,j.tp1,j.tp2,None)) :_*) contains true
     if (obviouslyEqual) {
       return true
     }
     history += "not obviously equal, trying subtyping"
     def innerCheck(tp1 : Term, tp2 : Term) : Boolean = if (subtypingRules.nonEmpty) {
       implicit val stack = j.stack
       var activerules = subtypingRules
       var done = false
       var tp1S = tp1
       var tp2S = tp2
       while (!done) {
         val (tmp1, tmp2, rOpt) = safeSimplifyUntil(tp1S, tp2S) { case (a1, a2) =>
           activerules.find(_.applicable(a1, a2))
         }
         tp1S = tmp1
         tp2S = tmp2
         rOpt match {
           case Some(rule) =>
             history += ("applying subtyping rule " + rule.toString + " on " + presentObj(tp1S) + " <: " + presentObj(tp2S))
             try {
               val b = rule(this)(tp1S, tp2S).getOrElse {
                 throw rule.Backtrack("")
               }
               return b
             } catch {
               case t: MaytriggerBacktrack#Backtrack =>
                 history += t.getMessage
                 activerules = dropJust(activerules, rule)
             }
           case None =>
             // try unsafe (since the alternative is equality-checking, which does this anyway, this shouldn't break anything)
             val tp1N = simplify(tp1S)
             val tp2N = simplify(tp2S)
             if (tp1S.hashneq(tp1N) || tp2S.hashneq(tp2N)) {
               history += "Trying unsafe"
               return innerCheck(tp1N, tp2N)
             }

             history += "No rules left; final terms are: " + presentObj(tp1S) + " and " + presentObj(tp2S)
             done = true
           // if (existsActivatable) return delay(Subtyping(stack, tp1S, tp2S), true)
         }
       }
       false
     } else false
      if (innerCheck(j.tp1,j.tp2)) true else {
        // otherwise, we default to checking equality
        // in the absence of subtyping rules, this is the needed behavior anyway
        history += "can't establish subtype relation, falling back to checking equality"
        check(Equality(j.stack, j.tp1, j.tp2, None))
      }
   }

   /** proves a Universe Judgment
 *
    * @param j the judgment
    * @return true if succeeded or delayed
    *
    * pre: context is covered
    *
    * post: j is covered
    */
   // TODO this should be removed; instead LF should use low-priority InhabitableRules that apply to any term
   private def checkUniverse(j : Universe)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     limitedSimplify(j.univ, universeRules) match {
        case (uS, Some(rule)) =>
          history += "Applying UniverseRule " + rule.toString
          rule(this)(uS)
        case (uS, None) => delay(Universe(stack, uS))
     }
   }

   /** proves an Inhabitable Judgment
 *
    * @param j the judgment
    * @return true if succeeded or delayed
    *
    * pre: context is covered
    *
    * post: j is covered
    */
   private def checkInhabitable(j : Inhabitable)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     limitedSimplify(j.wfo, inhabitableRules) match {
        case (uS, Some(rule)) =>
          history += "Applying InhabitableRule " + rule.toString
          rule(this)(uS)
        case (uS, None) =>
           history += "inferring universe"
           inferType(j.wfo)(stack, history) match {
             case None =>
                delay(Inhabitable(stack, uS))
             case Some(univ) =>
                check(Universe(stack, univ))
          }
     }
   }

   /** proves an Equality Judgment by recursively applying in particular EqualityRule's.
    *
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
      // 1) foundation-independent cases, e.g., identical terms, solving unknowns
      (tm1S, tm2S) match {
         case (l1: OMLIT, l2: OMLIT) =>
           if (l1.value != l2.value)
             return error(s"$l1 and $l2 are inequal literals")
           else {
             // TODO return true if a common value is a literal of the two distinct syntactic types
           }
         case (OML(n1, tp1, df1, _, f1), OML(n2, tp2, df2, _, f2)) =>
            if (n1 != n2)
              return error("OML's have different names, so cannot be equal")
            if (f1 != f2)
              return error("OML's have different features, so cannot be equal")
            return List((tp1,tp2),(df1,df2)) forall {
              case (None,None) => true
              case (Some(x1),Some(x2)) => checkEquality(Equality(j.stack, x1, x2, None))(history + "checking component of OML")
              case _ => error("OML's have different structure, so cannot be equal")
            }
         case _ =>
           if (tm1S hasheq tm2S) return true
           // there is not much else that is definitely inequal: e.g., even two distinct variables could be equal if their type is term-irrelevant
      }
      // solve an unknown
      val jS = j.copy(tm1 = tm1S, tm2 = tm2S)
      val solved = solveEquality(jS) || solveEquality(jS.swap)
      if (solved) return true

      // 2) find a TermBasedEqualityRule
      termBasedEqualityRules.filter(r => r.applicable(tm1S,tm2S)) foreach {rule =>
         // we apply the first applicable rule
         history += "trying term-based equality rule " + rule.toString
         val contOpt = rule(this)(tm1S,tm2S,tpOpt)
         contOpt match {
           case Some(cont) =>
             return cont.apply
           case None =>
             history += "rule not applicable"
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
      val tbEqRules = typeBasedEqualityRules
      safeSimplifyUntil(tp)(t => tbEqRules.find(_.applicable(t))) match {
         case (tpS, Some(rule)) =>
            history += "applying type-based equality rule " + rule.toString
            val res = try {
              rule(this)(tm1S, tm2S, tpS)
            } catch {
              case rule.DelayJudgment(msg) =>
                return delay(Equality(stack, tm1S, tm2S, Some(tpS)))(history + msg)
            }
            res match {
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
      val eqRules = termBasedEqualityRules
      others foreach {t2 =>
         eqRules foreach {r =>
            if (r.applicable(t1,t2)) {
               history += "Applying TermBasedEqualityRule " + r.toString
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
 *
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
          log("left term rewritten to " + t1ES.toStr(true))
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
          // t1 expanded, t2 can still be expanded --> continue expanding t2
          case (true, false)     => checkEqualityTermBased(terms2, t1ES::terms2, false, ! flipped)
          // t1 cannot be expanded anymore but t2 can ---> continue expanding t2
          case (false, false)    => checkEqualityTermBased(terms2, terms1, true, ! flipped)
          // neither term can be expanded --> try congruence rule as last resort
          case (false, true)     =>
             // if necessary, we undo the flipping of t1 and t2
             val s1 = if (flipped) terms2.head else t1ES
             val s2 = if (flipped) t1ES else terms2.head
             val s12Vars = s1.freeVars ::: s2.freeVars
             val minCont = stack.context.minimalSubContext(s12Vars)
             val j = Equality(Stack(minCont), s1, s2, Some(tp))
             val unknownsLeft = j.freeVars.toList intersect solution.map(_.name)

             // only incomplete congruence reasoning is left, we delay it if there is any hope to avoid it
             val nextStep = () => checkEqualityCongruence(TorsoForm.fromHeadForm(s1,unknownsLeft), TorsoForm.fromHeadForm(s2,unknownsLeft), unknownsLeft.nonEmpty)
             if (unknownsLeft.nonEmpty) {
               delay(j, Some(nextStep))
             } else {
               nextStep()
             }
             /* old code, experimentally replaced by above
             /* delay if there are unknowns left in this judgment that
              * - are already solved, or
              * - might be solved by activating other judgments
              */
             val delayThis = (unknownsLeft.nonEmpty && existsActivatable(allowIncomplete = false)) ||
                unknownsLeft.exists(n => solution(n).df.isDefined)
             if (delayThis) {
                // if there is some other way to proceed, do it
                delay(j, true)
             } else {
                // last resort: try congruence
                checkEqualityCongruence(TorsoForm.fromHeadForm(s1), TorsoForm.fromHeadForm(s2), unknownsLeft.nonEmpty)
             }*/
       }
   }

   /**
    * tries to prove equality using basic congruence rule
    * torso and heads must be identical, checkEquality is called in remaining cases
    */
   private def checkEqualityCongruence(t1: TorsoForm, t2: TorsoForm, unknownsLeft: Boolean)(implicit stack : Stack, history: History, tp: Term): Boolean = {
     lazy val j = Equality(stack, t1.toHeadForm, t2.toHeadForm, Some(tp))

     /* The torso form focuses on sequences of elimination operators to the same torso.
       * If the heads of the terms are introduction operators h, the congruence rule can usually be applied greedily
       * because introduction is usually injective.
       * For that purpose, a TermBasedEqualityRule(h,h) should be provided.
       * Therefore, a congruence rule for binders is currently not necessary.
       */
     def notSimilar = {

       val unknownsL = j.freeVars.toList intersect solution.map(_.name)
       def hasUnknowns(tm : Term) = (tm.freeVars intersect unknownsL).nonEmpty
       // j is delayed without a special treatment; if anything else is left to try, delay; else throw error

       if (unknownsLeft && existsActivatable(allowIncomplete = true))
         delay(j)
       else if // it *could* be that a torso is an application of an unknown, in which case equality should be checked?
       (t1.apps.length == t2.apps.length &&
         (hasUnknowns(t1.torso) || hasUnknowns(t2.torso)) &&
         (t1.apps.nonEmpty || t2.apps.nonEmpty)) // this condition is rather hacky, but necessary to avoid infinite loops
         checkEquality(Equality(stack, t1.torso, t2.torso, None))
       else {
         error("terms have different shape, thus they cannot be equal")
       }
     }

     val similar = t1.torso == t2.torso && t1.apps.length == t2.apps.length // && checkEquality(Equality(stack,t1.torso,t2.torso,None))
     if (similar) {
       (t1.apps zip t2.apps).forall { case (Appendage(head1, args1), Appendage(head2, args2)) =>
         val similarApp = head1 == head2 && args1.length == args2.length
         if (!similarApp) {
           notSimilar
         } else {
           log("equality (trying congruence)")
           history += "applying congruence"
           similarApp && (args1 zip args2).forall { case (a1, a2) =>
             check(Equality(stack, a1, a2, None))(history + "comparing argument")
           }
         }
       }
     } else {
       notSimilar
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

   /** tries to solve an unknown occurring in tm1 in terms of tm2
    *
    *  returns true if the unknowns were solved and the equality proved
    *  otherwise, returns false without state change (returning false here does not signal that the equality is disproved)
    */
   private def solveEquality(j: Equality)(implicit stack: Stack, history: History): Boolean = {
      j.tm1 match {
         //foundation-independent case: direct solution of an unknown variable
         case OMV(m) =>
            if (!solution.isDeclared(m))
               return false
            moveToRight(m)
            val remainingFreeVars = notAllowedInSolution(m, j.tm2)
            if (remainingFreeVars.isEmpty) {
               // we can solve m already, the remaining unknowns in tm2 can be filled in later
               val res = solve(m, j.tm2)
               j.tpOpt foreach {case tp => solveType(m, tp)}
               res
            } else {
               history += ("free variables remain: " + remainingFreeVars.mkString(", "))
               history += presentObj(solution)
               delay(j)
               // meta-variable solution has free variable --> type error unless free variable disappears later
               // better: we can rearrange the unknown variables (which ultimately may not depend on each other anyway)
               // note: tm2 should be simplified - that may make a free variable disappear
            }
         //apply a foundation-dependent solving rule selected by the head of tm1
         case _ => Solver.findSolvableVariable(solutionRules, solution, j.tm1) match {
            case Some((rs, m)) =>
              rs.foreach {sr =>
                sr(j) match {
                  case Some((j2,msg)) =>
                    history += "Using solution rule " + rs.head.toString
                    return solveEquality(j2)(j2.stack, (history + msg).branch)
                  case _ =>
                }
              }
              false
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

   /* ********************** simplification ******************************************************/

   /**
    * unsafe via ObjectSimplifier
    * this subsumes substituting for solved unknowns before simplifier expands defined variables
    */
   def simplify(t : Obj)(implicit stack: Stack, history: History): t.ThisType = {
      val tS = controller.simplifier(t, constantContext ++ solution ++ stack.context, rules)
      if (tS != t)
         history += ("simplified: " + presentObj(t) + " ~~> " + presentObj(tS))
      tS
   }

   /** substitutes solved unknowns, then possibly calls simplification
    *  (simplification alone does not necessary substitute solved unknowns because simpleness is cached)
    */
   def substituteSolved(t: Obj, covered: Boolean)(implicit stack: Stack, history: History): t.ThisType = {
      val subs = solution.toPartialSubstitution
      val tS = t ^^ subs
      val tSS = if (covered) simplify(tS) else tS
      tSS.asInstanceOf[t.ThisType] // always succeeds but Scala doesn't know that
   }

   /** simplifies safely one step along each branches, well-formedness is preserved+reflected */
   //TODO merge with limitedSimplify; offer simplification strategies
   private def safeSimplify(tm: Term)(implicit stack: Stack, history: History): Term = tm match {
      case _:OMID | _:OMLITTrait => tm
      case OMV(n) => (stack.context.getO(n) orElse solution.getO(n) orElse constantContext.getO(n)) match {
        case Some(vd) if vd.tp.isDefined =>
          //TODO variable definitions that contain shadowed variables may not be expanded
          safeSimplify(vd.tp.get)
        case _ => tm
      }
      case o: OML => o
      case ComplexTerm(op, subs, cont, args) =>
         computationRules foreach {rule =>
            if (rule.head == op) {
              rule(this)(tm, false) match {
                case Some(tmS) =>
                  history += "applying computation rule " + rule.toString
                  log("simplified: " + tm + " ~~> " + tmS)
                  history += "simplified: " + presentObj(tm) + " ~~> " + presentObj(tmS)
                  return tmS.from(tm)
                case _ =>
              }
            }
          }
        // no rule or rule not applicable, recurse
        val subsS = subs mapTerms {a => safeSimplify(a)(stack, history + "simplifying argument before context")}
        val contS = cont mapTerms {case (c,t) => safeSimplify(t)(stack ++ c, history + "simplifying component of bound variable")}
        val argsS = args map {a => safeSimplify(a)(stack ++ cont, history + "simplifying argument")}
        val tmS = ComplexTerm(op, subsS, contS, argsS).from(tm)
        tmS
   }

   /** simplifies one step overall */
   private def safeSimplifyOne(tm: Term)(implicit stack: Stack, history: History): Term = {
     def expandDefinition: Term = {
       val tmE = defExp(tm, outerContext++stack.context)
       if (tmE hashneq tm)
         history += ("definition expansion yields: " + presentObj(tm) + " ~~> " + presentObj(tmE))
       tmE
     }
     if (checkingUnit.isKilled) {
       return tm
     } /*
     tm.head match {
       case Some(h) =>
         // use first applicable rule
         computationRules foreach {rule =>
           if (rule.head == h) {
             val ret = rule(this)(tm, false)
             ret foreach {tmS =>
               history += "applying computation rule " + rule.toString
               history += ("simplified: " + presentObj(tm) + " ~~> " + presentObj(tmS))
               return tmS
             }
           }
         }
         // no applicable rule, expand a definition
         expandDefinition
       case None => expandDefinition
     } */
     val traverser = new SimplifyTraverser(stack,history)
     val (ret,done) = traverser.run(tm)
     if (done) {
       history += ("simplified: " + presentObj(tm) + " ~~> " + presentObj(ret))
       ret
     } else expandDefinition
   }

  private val thisSolver = this
  /* a very naive simplifier that simplifies anywhere in a term */
  // TODO this does not scale; it is unclear what heuristic to use for simplification
  private class SimplifyTraverser(stack: Stack, history: History) extends StatelessTraverser {
    private var done = false
    def run(t : Term): (Term,Boolean) = {
      val ret = apply(t, stack.context)
      (ret,done)
    }
    def traverse(t: Term)(implicit con: Context, state: State): Term = {
      if (done) return t
      t match {
        case ComplexTerm(op,_,_,_) =>
            // use first applicable rule
            computationRules foreach {rule =>
              if (rule.head == op) {
                val ret = rule(thisSolver)(t, false)(Stack(con),history)
                ret foreach {tmS =>
                  history += "applying computation rule " + rule.toString
                  done = true
                  return tmS
                }
              }
            }
            // no applicable rule, traverse
            Traverser(this,t)
       /* case OMS(op) => //FR commenting this out, awaiting answer from DM about whether it's needed; it currently causes match errors
          // use first applicable rule
          computationRules foreach {rule =>
            if (rule.head == op) {
              val ret = rule(thisSolver)(t, false)(Stack(con),history)
              ret foreach {tmS =>
                history += "applying computation rule " + rule.toString
                done = true
                return tmS
              }
            }
          }
          // no applicable rule, traverse
          Traverser(this,t) */
        case _ =>
            Traverser(this,t)
      }
    }
  }

   /** special case of the version below where we simplify until an applicable rule is found
 *
    *  @param tm the term to simplify
    *  @param hs the RuleMap from which an applicable rule is needed
    *  @param stack the context of tm
    *  @return (tmS, Some(r)) where tmS = tm and r from rm is applicable to tmS; (tmS, None) if tm = tmS and no further simplification rules are applicable
    */
   private def limitedSimplify[R <: CheckingRule](tm: Term, hs: Iterable[R])(implicit stack: Stack, history: History): (Term,Option[R]) =
      safeSimplifyUntil[R](tm)(t => t.head flatMap {h => hs.find(_.heads contains h)})

   /** applies [[ComputationRule]]s expands definitions until a condition is satisfied;
    *  A typical case is transformation into weak head normal form.
 *
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
            if ((tm1S hashneq tm1) || (tm2S hashneq tm2))
                  safeSimplifyUntil(tm1S, tm2S)(simple)
               else
                  (tm1S,tm2S,None)
      }
   }

   /* ********************** methods for contexts ***************************/

  /** checks contexts */
   private def checkContext(j: IsContext)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     val con = simplify(j.wfo)
     con.declsInContext.forall {case (pre,vd) =>
       val newCon = stack ++ pre
       (vd.tp,vd.df) match {
         case (None,None) => true
         case (Some(tp),Some(df)) => check(Typing(newCon, df, tp, None))(history + "defined declaration must be well-typed")
         case (Some(tp), None) => check(Inhabitable(newCon, tp))(history + "type in contexts must be inhabitable")
         case (None, Some(df)) => inferTypeAndThen(df)(newCon, history + "definiens in context must be well-formed") {_ => true}
       }
     }
   }

  /** checks equality of contexts (up to either alpha-renaming or reordering) */
   private def checkEqualityContext(j: EqualityContext)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     if (j.context1.length != j.context2.length) {
       return error("contexts do not have the same length")
     }
     val c1 = simplify(j.context1)
     val c2 = simplify(j.context2)
     var sub = Substitution.empty
     def checkOptTerm(c: Context, tO1: Option[Term], tO2: Option[Term]) = (tO1,tO2) match {
       case (None,None) =>
         true
       case (Some(t1),Some(t2)) =>
         check(Equality(j.stack ++ c, t1, t2 ^? sub, None))(history + "component-wise equality of contexts")
       case _ =>
         error("contexts do not have the same shape")
     }
     val c2R = if (j.uptoAlpha) {
       c2
     } else {
       // c2New is the reordering of c2 such that corresponding variables have the same name
       // invariant: c2New ++ c2Rest == c2  up to reordering
       var c2Rest = c2
       var c2New = Context.empty
       c1.foreach {vd1 =>
         val (skip, vd2rest) = c2Rest.span(_.name != vd1.name)
         if (vd2rest.isEmpty) {
           return error("contexts do not declare the same names")
         }
         val vd2 :: rest = vd2rest
         // c2Rest == skip ++ vd2 ++ rest  and  vd1.name == vd2.name
         // TODO vd2 may refer to skipped variables if they have a definition, in which case we should substitute the definition
         if (! utils.disjoint(skip.map(_.name), vd2.freeVars)) {
           return error("cannot reorder contexts to make them declare the same variables names equal")
         }
         c2Rest = skip ::: rest
         c2New = c2New ++ vd2
       }
       c2New
     }

     (c1.declsInContext.toList zip c2.zipWithIndex) forall {case ((c, vd1), (vd2,i)) =>
       if (j.uptoAlpha) {
         sub = sub ++ (vd2.name -> OMV(vd1.name))
       } else {
         //if (vd1.name != vd2.name) return error("contexts do not declare the same variables") else // redundant now
       }
       checkOptTerm(c, vd1.tp, vd2.tp) && checkOptTerm(c, vd1.df, vd2.df)
     }
   }

   /* ************************************************************ */


   def solveTyping(tm: Term, tp: Term)(implicit stack: Stack, history: History): Boolean = {
     val unknownsLeft = tm.freeVars intersect solution.map(_.name)
     val tnf = TorsoNormalForm(Nil)
      tm match {
         //foundation-independent case: direct solution of an unknown variable
         case OMV(m) =>
            if (!solution.isDeclared(m))
               return false
            moveToRight(m)
            val remainingFreeVars = notAllowedInSolution(m, tp)
            if (remainingFreeVars.isEmpty) {
               // we can solve m already, the remaining unknowns in tm2 can be filled in later
               val res = solveType(m, tp)
               res
            } else {
               history += ("bound variables or unknowns remain in potential solution: " + remainingFreeVars.mkString(", "))
               delay(Typing(stack,tm,tp))
               // meta-variable solution has free variable --> type error unless free variable disappears later
               // better: we can rearrange the unknown variables (which ultimately may not depend on each other anyway)
               // note: tm2 should be simplified - that may make a free variable disappear
            }
         //apply a foundation-dependent solving rule selected by the head of tm1
         case tnf(OMV(m), Appendage(h,_) :: _) if solution.isDeclared(m) && ! tp.freeVars.contains(m) => //TODO what about occurrences of m in tm1?
           val allrules = rules.getByHead(classOf[TypeSolutionRule], h)
           allrules.foreach{rule =>
             history += "Applying TypeSolutionRule " + rule.toString
             return rule(this)(tm, tp)
           }
           false
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
      val res = prove(j.tp)(j.stack, history)
      if (res.isDefined)
         true
      else
         delay(j)
   }

   /**
    *  tries to prove a goal
    *
    *  @return the proof term if successful
    */
   def prove(conc: Term)(implicit stack: Stack, history: History): Option[Term] = {
      prove(constantContext ++ solution ++ stack.context, conc)
   }

  def proveWithoutSolution(conc: Term)(implicit stack: Stack, history: History): Option[Term] = {
    prove(constantContext ++ stack.context, conc)
  }

   private def prove(context: Context, conc: Term)(implicit history: History): Option[Term] = {
      val msg = "proving " + presentObj(context) + " |- _ : " + presentObj(conc)
      log(msg)
      history += msg
      val pu = ProvingUnit(checkingUnit.component, simplify(context)(Stack(context),history), conc, logPrefix).diesWith(checkingUnit)
      controller.extman.get(classOf[Prover]) foreach {prover =>
         val (found, proof) = prover.apply(pu, rules, 3) //Set the timeout on the prover
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
    * applies all ForwardSolutionRules of the given priority
    *
    * @param priority exactly the rules with this Priority are applied
    */
   //TODO call this method at appropriate times
   private def forwardRules(priority: ForwardSolutionRule.Priority)(implicit stack: Stack, history: History): Boolean = {
      val results = solution.zipWithIndex map {
         case (vd, i) => vd.tp match {
           case Some(tp) =>
              implicit val con : Context = solution.take(i)
              limitedSimplify(tp, forwardSolutionRules) match {
                 case (tpS, Some(rule)) if rule.priority == priority =>
                   history += "Applying ForwardSolutionRule " + rule.toString
                   rule(this)(vd)
                 case _ => false
              }
           case None => false
         }
         case _ => false
      }
      results.contains(true)
   }
}

object Solver {
  /** reconstructs a single term and returns the reconstructed term and its type */
  def check(controller: Controller, stack: Stack, tm: Term): Either[(Term,Term),Solver] = {
      val ParseResult(unknowns,free,tmU) = ParseResult.fromTerm(tm)
      val etp = LocalName("expected_type")
      val j = Typing(stack, tmU, OMV(etp), None)
      val cu = CheckingUnit(None, stack.context, unknowns ++ VarDecl(etp), j)
      val rules = RuleSet.collectRules(controller, stack.context)
      val solver = new Solver(controller, cu, rules)
      solver.applyMain
      if (solver.checkSucceeded) solver.getSolution match {
         case Some(sub) =>
             val tmR = tmU ^ sub
             Left(tmR, sub("expected_type").get) // must be defined if there is a solution
         case None => Right(solver)
      } else
         Right(solver)
  }
  /** infers the type of a term that is known to be well-formed */
  def infer(controller: Controller, context: Context, tm: Term, rulesOpt: Option[RuleSet]): Option[Term] = {
      val rules = rulesOpt.getOrElse {
         RuleSet.collectRules(controller, context)
      }
      implicit val stack = Stack(Context())
      implicit val history = new History(Nil)
      val cu = CheckingUnit(None, context, Context.empty, null) // awkward but works because we do not call applyMain
      val solver = new Solver(controller, cu, rules)
      val tpOpt = solver.inferType(tm, true)
      tpOpt map {tp => solver.simplify(tp)}
  }

  /**
    * tests a term for the occurrence of an unknown variables that can be isolated by applying solution rules
    *
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
