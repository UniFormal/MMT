package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import frontend._
import jdk.vm.ci.hotspot.HotSpotCompressedNullConstant
import symbols._
import uom._
import utils._
import modules._
import objects.Conversions._
import objects._
import proving._
import parser.ParseResult

import scala.collection.mutable.{HashSet, ListMap}
import scala.collection.immutable.{ListMap => IListMap}
import scala.collection.mutable
import scala.runtime.NonLocalReturnControl

/* ideas
 * inferType should guarantee well-formedness (what about LambdaTerm?)
 *   but what if there are unknowns whose type cannot be inferred? Is that even possible?
 *
 * safeSimplifyUntilRuleApplicable must include computation, definition expansion, but can stop on GlobalChange; but safety is usually needed
 *
 * constants have equality rule: injectivity and implicit arguments used to obtain necessary/sufficient condition (not preserved by morphism); congruence if no rule applicable
 *
 * false should not be returned without generating an error
 *
 * injectivity rules must smartly handle situations like op(t1)=op(t2)
 * currently all definitions in t1 and t2 are expanded even though op is often injective, especially if op is a type operator
 */

/** caches the inferred type of a term with that term */

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
 * @param checkingUnit the judgment to check
 *
 * See [[CheckingUnit]] for the semantics of the contexts.
 *
 * Use: Create a new instance for every problem, call apply on all constraints, then call getSolution.
 */
class Solver(val controller: Controller, val checkingUnit: CheckingUnit, val rules: RuleSet, val initstate : Option[SolverState]  = None)
      extends CheckingCallback with SolverAlgorithms with Logger {

   /** for Logger */
   val report = controller.report
   /** the component URI if provided, solver otherwise */
   val logPrefix = checkingUnit.component.map(_.toString).getOrElse("solver")
   /** the SubstitutionApplier to be used throughout */
   private implicit val sa = new MemoizedSubstitutionApplier
   /** used for rendering objects, should be used by rules if they want to log */
   implicit val presentObj : Obj => String = o => controller.presenter.asString(o)
   /** for marking objects as stable, created fresh for every run because stability depends on the context and we do not want to have to remove the marker */
   // There is a memory leak here because we never remove Stability markers. But it's unclear when and how it's best to remove them.
   val stability = Stability.make

   /**
    * to have better control over state changes, all stateful variables are encapsulated a second time
    */
   protected object state {

     var currentState: SolverState = if(initstate.nonEmpty) {initstate.get} else{ new SolverState(_solution = checkingUnit.unknowns) }




     // accessor and setter methods for [currentState], used to simulate mutable state
     def solution = currentState._solution
     def solution_= (s : Context) {currentState = currentState.copy(_solution = s)}
     def delayed = currentState._delayed
     def delayed_= (d : List[DelayedConstraint]) {currentState = currentState.copy(_delayed = d)}
     def errors =  currentState._errors
     def errors_= (e : List[SolverError]) {currentState = currentState.copy(_errors = e)}
     def dependencies = currentState._dependencies
     def dependencies_= (c : List[CPath]) {currentState = currentState.copy(_dependencies = c)}

     /**
       *
       * @param n
       * @return the bounds for n
       */
     def getbounds(n: LocalName) =  currentState._bounds.getOrElse(n ,Nil)

      // adder methods for the "stateful" lists

      /** registers a constraint */
      def addConstraint(d: DelayedConstraint)(implicit history: History) {
        if (!mutable && !pushedStates.head.allowDelay) {
          throw MightFail(history)
        } else {
          delayed ::= d
          if (!mutable) {
            pushedStates.head.delayedInThisRun ::= d
          }
        }
      }
      /** registers an error */
      def addError(e: SolverError) {
         if (mutable) errors ::= e
         else {
           throw WouldFail
         }
      }

      /** registers a dependency */
      def addDependency(p: CPath) {
         dependencies ::= p
      }

      // more complex mutator methods for the stateful lists

      def removeConstraint(dc: DelayedConstraint) {
         delayed = currentState._delayed filterNot (_ == dc)
         if (!mutable) {
           val state = pushedStates.head
           state.delayedInThisRun = state.delayedInThisRun.filterNot(_ == dc)
         }
      }
      def setNewSolution(newSol: Context) {
         if (!mutable && !pushedStates.head.allowSolving) {
           throw MightFail(NoHistory)
         }
         solution = newSol
      }
      // special case of setNewSolution that does not count as a side effect
      def reorderSolution(newSol: Context) {
         solution = newSol
      }

      def setNewBounds(n: LocalName, bs: List[TypeBound]) {
         if (!mutable && !pushedStates.head.allowSolving) {
           throw MightFail(NoHistory)
         }

        // since _bounds in [[currentState]] is immutable , we first have to create a new bounds list
        val newbounds =  currentState._bounds.updated(n , bs)
        currentState = currentState.copy(_bounds = newbounds)
      }

      // instead of full backtracking, we allow exploratory runs that do not have side effects

      /** if false, all mutator methods have no effect on the state
       *  they may throw a [[DryRunResult]]
       */
      private def mutable = pushedStates.isEmpty
      /** the state that is stored here for backtracking */
      private case class StateData(solutions: Context, bounds: ListMap[LocalName,List[TypeBound]],
                                   dependencies: List[CPath], delayed: List[DelayedConstraint],
                                   allowDelay: Boolean, allowSolving: Boolean) {
         var delayedInThisRun: List[DelayedConstraint] = Nil
      }
      /** a stack of states for dry runs */
      private var pushedStates: List[StateData] = Nil

      /** true if we are currently in a dry run */
      def isDryRun = !mutable

     //TODO: make StateData also use the immutable version of ListMap
      /**
       * evaluates its arguments without generating new constraints
       *
       * all state changes are rolled back unless evaluation is successful and commitOnSuccess is true
        *
       */
      def immutably[A](allowDelay: Boolean, allowSolving: Boolean, commitOnSuccess: A => Boolean)(a: => A): DryRunResult = {
         val tempState = StateData(solution, new ListMap[LocalName, List[TypeBound]]().++=(currentState._bounds), dependencies, currentState._delayed, allowDelay, allowSolving)
         pushedStates ::= tempState
         def rollback {
            val oldState = pushedStates.head
            pushedStates = pushedStates.tail
            solution = oldState.solutions
            // since oldState uses a mutable ListMap to save the bounds it has to be converted to an immutable list first (using IListMap.from)
            currentState = currentState.copy(_bounds = IListMap.from(oldState.bounds.toList))
            dependencies = oldState.dependencies
            delayed = oldState.delayed
         }
         try {
           val aR = a
           if (tempState.delayedInThisRun.nonEmpty) {
              activateRepeatedly
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
        delayed = bp.delayed
        // remove new dependencies
        dependencies = currentState._dependencies.drop(currentState._dependencies.length - bp.depLength)
        // restore old solution
        solution = bp.solution
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

  def saveCurrentState() = currentState = currentState.pushState()
  def undoCurrentState() = currentState = currentState.last
  def getCurrentState() = currentState

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
   def getErrors : List[SolverError] = errors
   /** @return the current list of dependencies */
   def getDependencies : List[CPath] = dependencies

  /** caches the results of judgements to avoid duplicating work */
  // judgements are not cached if we are in a dry run to make sure they are run again later to solve unknowns
  protected object JudgementStore {
    private val store = new scala.collection.mutable.HashMap[Judgement,Boolean]
    /** if a judgment is tried again, we may need to uncache it (e.g., when reactivating a delayed judgment) */
    def delete(j: Judgement) {
      store.remove(j)
    }
    /** lookup up result for j; if not known, run f to define it */
    def getOrElseUpdate(j : Judgement)(f: => Boolean): Boolean = {
      store.find {case (k,r) => r && (k implies j) || !r && (j implies k)} match {
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
         if (unsolved.nonEmpty) {
            report(prefix, "unsolved: " + unsolved.mkString(", "))
            unsolved foreach {u =>
              val (upper,lower) = getbounds(u).partition(_.upper)
              if (upper.nonEmpty)
                report(prefix, "upper bounds of " + u + ": " + upper.map(b => presentObj(b.bound)).mkString(", "))
              if (lower.nonEmpty)
                report(prefix, "lower bounds of " + u + ": " + lower.map(b => presentObj(b.bound)).mkString(", "))
            }
         } else {
            report(prefix, "all variables solved")
         }
         if (errors.nonEmpty) {
            report(prefix, "errors:")
            logGroup {
               errors.foreach {case SolverError(_,e,_) =>
                  report(prefix, "error: " + e.getSteps.head.present)
                  logHistory(e)
               }
            }
         } else
            report(prefix, "no errors")
         if (delayed.nonEmpty) {
            report(prefix, "constraints:")
            logGroup {
               delayed.foreach {
                  case d: DelayedJudgement =>
                     report(prefix, d.constraint.present)
                     // can be useful for debugging but is overkill in general
                     logGroup {
                          report(prefix, d.constraint.presentAntecedent(_.toString))
                          report(prefix, d.constraint.presentSuccedent(_.toString))
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

   protected def logAndHistoryGroup[A](body: => A)(implicit history: History) = {
     logGroup {
       history.indented {
         body
       }
     }
   }

   /**
    * tries to evaluate an expression without any generating new constraints
    *
    * @param a the expression
    * @param commitOnSuccess do not roll back state changes if successful
    */
   override def dryRun[A](allowDelay: Boolean, commitOnSuccess: A => Boolean)(a: => A): DryRunResult = immutably(allowDelay, true, commitOnSuccess)(a)

   /**
    * tries to derive a set of judgments without any side effect (no solving of variables, no delay)
    */
   def tryToCheckWithoutEffect(js:Judgement*): Option[Boolean] = {
     val dr = immutably(false, false, (x:Boolean) => false) {
       js forall {j => check(j)(NoHistory)}
     }
     dr match {
      case Success(s:Boolean) => Some(s)
      case Success(_) => throw ImplementationError("illegal success value")
      case WouldFail => Some(false)
      case _:MightFail => None
     }
   }

  // ******************* representation of unknowns

  /**
   * an unknown meta-variable
   * see also Solver.makeUnknown
   */
  object Unknown {
    def apply(name: LocalName, args: List[Term]) = if (args.isEmpty) OMV(name) else OMA(OMV(name), args)
    def unapply(t: Term): Option[(LocalName,List[Term])] = t match {
      case OMAorAny(OMV(n), args) =>
        if (solution isDeclared n)
          Some((n,args))
        else
          None
      case _ => None
    }
    def is(t: Term) = unapply(t).isDefined
  }

  /** substitutes some unknowns with solutions
   *  pre: solutions are of the form Free(xs,s) where xs.length is the number of arguments of the unknown
   *  getSolution returns such a substitution
   *
   */
  // TODO this breaks structure sharing
  protected class SubstituteUnknowns(sub: Substitution) extends StatelessTraverser {
    private val unknowns = sub.map(_.name)
    // con is ignored
    def traverse(t: Term)(implicit con : Context, state : State) = t match {
      case Unknown(n, args) =>
        val argsT = args map {a => traverse(a)}
        val tS = sub(n) match {
          case Some(t) =>
            val FreeOrAny(xs, s) = t
            s ^? (xs /! argsT)
          case None =>
            Unknown(n, argsT)
        }
        tS.from(t) // preserve meta-data when this is called at the end, in particular source reference of _
      case _ =>
        if (t.freeVars exists {x => unknowns contains x})
          Traverser(this, t)
        else
          t
    }
  }
  /** replaces all solved unknowns with their solution */
  def substituteSolution(o: Obj): o.ThisType = {
    val su = new SubstituteUnknowns(getPartialSolution.toPartialSubstitution)
    val oS = su.traverseObject(o)(Context.empty, ())
    oS.asInstanceOf[o.ThisType] // always succeeds but Scala doesn't know that
  }

  // *************************** context and lookup

   val constantContext = checkingUnit.context
   val initUnknowns = checkingUnit.unknowns
   /** the context that is not part of judgements */
   def outerContext = constantContext ++ solution

  // TODO this should track lookups for dependency management
  def lookup = controller.globalLookup

  private def getConstant(p : GlobalName)(implicit h: History): Option[Constant] = {
    lookup.getO(ComplexTheory(constantContext), p.toLocalName) match {
      case Some(c: Constant) => Some(c)
      case Some(_) =>
        error("not a constant: " + p)
        None
      case None =>
        error("constant not found: " + p)
        None
    }
  }

   /** retrieves the type type of a constant and registers the dependency
    *
    * returns nothing if the type could not be reconstructed
    */
   def getType(p: GlobalName)(implicit h: History): Option[Term] = {
      val c = getConstant(p).getOrElse {
        h += "constant not found"
        return None
      }
      if (!c.tpC.isDefined) {
        h += "constant has no type"
        return None
      }
      val t = c.tpC.getAnalyzedIfFullyChecked
      if (t.isDefined) {
        addDependency(p $ TypeComponent)
      } else {
        h += "type of constant not fully checked"
      }
     t
   }

   /** retrieves the definiens of a constant and registers the dependency
    *
    * returns nothing if the type could not be reconstructed
    */
   def getDef(p: GlobalName)(implicit h: History) : Option[Term] = {
      val c = getConstant(p).getOrElse {return None}
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

  /**
   * looks up a variable in the appropriate context
   *
   * @return the variable declaration for name
   */
  def getVar(name: LocalName)(implicit stack: Stack) = (constantContext ++ solution ++ stack.context)(name)

   // ******************************** methods for solving solving unknowns ***********************************

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
      log("solving " + name + " as " + presentObj(value))
      history += ("solving " + name + " as " + presentObj(value))
      val valueS = simplify(substituteSolution(value))(Stack.empty, history)
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.df.isDefined) {
         check(Equality(Stack.empty, valueS, solved.df.get, solved.tp))(history + "solution must be equal to previously found solution")
      } else {
         parser.SourceRef.delete(valueS) // source-references from looked-up types may sneak in here
         val vd = solved.copy(df = Some(valueS))
         // substitute in solutions of other variables
         // this usd to do only val rightS = right ^^ (OMV(name) / valueS), but that is bad because solutions are not immediately reduced
         // the code below is a bit redundant as it substitutes all solutions, even though normally only the new one should occur free
         val newSolution = left ::: vd :: right
         setNewSolution(newSolution)
         val newSolutionS = substituteSolution(newSolution)
         setNewSolution(newSolutionS)
         val r = typeCheckSolution(vd)
         if (!r) return false
         getbounds(name) forall {case TypeBound(bound, below) =>
           val j = subOrSuper(below)(Stack.empty, value, bound)
           check(j)(history + "solution must conform to existing bound")
         }
      }
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
   protected def solveType(name: LocalName, value: Term)(implicit history: History) : Boolean = {
      lazy val msg = "solving type of " + name + " as " + presentObj(value)
      log(msg)
      history += msg
      val newSolution = simplify(substituteSolution(value))(Stack.empty, history)
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.tp.isDefined) {
         val existingSolution = solved.tp.get
         // check(Equality(Stack.empty, newSolution, existingSolution, None))(history + "solution for type must be equal to previously found solution")
         if (tryToCheckWithoutDelay(Subtyping(Stack.empty,newSolution,existingSolution)).contains(true)) {
           setNewSolution(left ::: solved.copy(tp = Some(newSolution)) :: right)
           true
         }
         else check(Subtyping(Stack.empty,existingSolution,newSolution))
      } else {
         val vd = solved.copy(tp = Some(newSolution))
         setNewSolution(left ::: vd :: right)
         val r = typeCheckSolution(vd)
         if (!r) return false
         getbounds(name).forall {case TypeBound(bound, _) =>
            check(Typing(Stack.empty, bound, newSolution))(history + "solution to type must be compatible with existing bound")
         }
      }
   }

   protected def subOrSuper(sub: Boolean)(stack: Stack, t1: Term, t2: Term) = {
     if (sub) Subtyping(stack, t1, t2) else Subtyping(stack, t2, t1)
   }

   /**
    * solves a type bound for an unknown
    * @param below if true, name <: value, else name :> value
    */
   protected def solveBound(name: LocalName, value: Term, below: Boolean)(implicit history: History) : Boolean = {
      val kind = if (below) "upper" else "lower"
      log(s"solving $kind bound of $name as " + value)
      history += s"solving $kind bound of $name as " + presentObj(value)
      val newBound = simplify(substituteSolution(value))(Stack.empty, history)
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.df.isDefined) {
        val existingSolution = solved.df.get
        val j = subOrSuper(below)(Stack.empty, existingSolution, newBound)
        check(j)(history + "existing solution must conform to bound")
      } else {
        if (solved.tp.isDefined) {
          val existingType = solved.tp.get
          val r = check(Typing(Stack.empty, newBound, existingType))(history + "bound of unknown must conform to its type")
          if (!r) return false
        }
        val bnd = TypeBound(newBound, below)
        // remove all bounds subsumed by the new one
        val newBounds = getbounds(name) filter {case TypeBound(oldBound, oldBelow) =>
          if (below != oldBelow)
            true
          else {
            val j = subOrSuper(below)(Stack.empty, newBound, oldBound)
            val subsumed = tryToCheckWithoutEffect(j)
            !(subsumed contains true)
          }
        }
        setNewBounds(name, bnd :: newBounds)
        true
      }
   }

   /** if the type and the definiens of an unknown are solved independently, this type-checks them */
   protected def typeCheckSolution(vd: VarDecl)(implicit history: History): Boolean = {
      (vd.tp, vd.df) match {
         case (Some(FreeOrAny(tpCon,tp)), Some(FreeOrAny(dfCon,df))) =>
           val eqCon = check(EqualityContext(Stack.empty, tpCon, dfCon, true))(history + "checking equality of contexts of solution of of metavariable and type")
           if (!eqCon) return false
           val subs = (tpCon alpha dfCon).getOrElse(return false)
           check(Typing(Stack(tpCon), df ^? subs, tp))(history + "checking solution of metavariable against solved type")
         case _ => true
      }
   }

   /** moves declarations in solution to the right so that 'name' occurs as far to the right as allowed by dependencies */
   protected def moveToRight(name: LocalName) {
     val (before, it::rest) = solution.span(_.name != name)
     var toLeft = Context.empty
     var toEnd = Context(it)
     var toEndNames = List(name)
     rest.foreach {vd =>
       val vdVars = vd.freeVars ::: state.getbounds(vd.name).flatMap(_.bound.freeVars)
       if (utils.disjoint(vdVars, toEndNames)) {
         // no dependency: move over vd
         toLeft = toLeft ++ vd
       } else {
         // dependency: also move vd to the end
         toEnd = toEnd ++ vd
         toEndNames ::= vd.name
       }
     }
     reorderSolution(before ++ toLeft ++ toEnd)
     log("moved " + name + " to the right, new solution: " + presentObj(solution))
   }


   /**
    * @param m an unknown variable
    * @return if m is an unknown, the list of free variables of tm that preclude solving type/definition of m as tm
    *   namely the bound variables and the unknowns declared after m
    */
   protected def notAllowedInSolution(m: LocalName, tm: Term)(implicit stack: Stack, history: History): List[LocalName] = {
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
               // TODO switching to the commented-out code might help solving some cases
               else true // v == m || solution(v).df.isDefined // after m but not solved yet
         }
      }
  }

  // check that xs is a list of distinct bound variables and return their declarations (needed for solving unknowns)
  protected def isDistinctVarList(xs: List[Term])(implicit stack: Stack): Option[Context] = {
    val vds = xs map {
      case OMV(x) => stack.context.getO(x) match {
        case Some(vd) => vd
        case None => return None
      }
      case _ => return None
    }
    if (vds.distinct != vds)
      None
    else
      Some(vds)
   }

   private var freshUnkownCounter = -1
   /** generates a fresh names for an unknown */
   def freshUnknown() = {
     freshUnkownCounter += 1
     LocalName("") / "O" / freshUnkownCounter.toString
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
      setNewSolution(left ::: newVars ::: right)
      true
   }

   /**
    * define x:tp as the unique value that satisfies the constraint
    *
    * this is implemented by adding a fresh unknown, and running the constraint
    * this can be used to compute a value in logic programming style, where the computation is given by a functional predicate
    * @param x a fresh unknown
    * @param tp the type of x
    * @param constraint a program that calls checks that suffice to solve the value of x; this should return false if the solution failed
    */
   def defineByConstraint(x: LocalName, tp: Term)(constraint: Term => Boolean) = {
     addUnknowns(x%tp, None)
     constraint(OMV(x))
   }

   // ******************************** error reporting

  /** registers an error, returns false */
  override def error(message: => String, exc: Option[Level.Excuse] = None)(implicit history: History): Boolean = {
      log("error: " + message)
      history += message
      val level = Level.Error
      val e = SolverError(exc, history)
      addError(e)
      false
  }

  // ************************************* the main algorithm

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
      addConstraint(new DelayedJudgement(j, bi, notTriedYet = true))(h)
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
   protected def delay(j: Judgement, suffices: Option[List[Equality]] = None)(implicit history: History): Boolean = {
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
         val dc = new DelayedJudgement(j, bi, suffices)
         addConstraint(dc)
      }
      true
   }

   /**
    * processes the next activatable constraint until none are left
    *
    * if there is no activatable constraint, we try an incomplete constraint as a last resort
    *
    * @return false if disproved (if true returned, constraints and unknowns may be left)
    */
   private def activateRepeatedly: Boolean = {
     // activates a constraint
     def activate(dc: DelayedConstraint) = {
       removeConstraint(dc)
       setCurrentBranch(dc.branch)
       dc match {
         case dj: DelayedJudgement =>
           val j = dj.constraint
           JudgementStore.delete(j) // previous delay may have cached true
           val jP = prepareJ(j)
           log("activating: " + jP.present)
           check(jP)(dj.history)
         case di: DelayedInference =>
           val tmP = prepare(di.tm)
           val stackP = prepareS(di.stack)
           val history = di.history + "reactivating inference of type of " + presentObj(tmP)
           inferTypeAndThen(tmP)(stackP, history)(di.cont)
       }
     }
     // infinite loop, we break out with 'return' when disproved or no more progress possible
     while (true) {
       // look for an activatable constraint
       val solved = getSolvedVariables
       val dcOpt = delayed.find {d => d.isActivatable(solved)}
       if (errors.nonEmpty) return false // speed up checking files with errors
       dcOpt match {
         case Some(dc) =>
           // normal case
           val mayhold = activate(dc)
           if (!mayhold) return false
          case None =>
            // find an unsolved but bounded unknown that is not used in any constraint
            val solvableOpt = solution.find {vd =>
              vd.df.isEmpty && getbounds(vd.name).nonEmpty && {
                delayed forall {
                  case d: DelayedJudgement => true // !(d.freeVars contains vd.name)
                  case d: DelayedInference => true // omitted types of bound variables typically lead to delayed inferences of the kind of the unknown type for which a bound has been found already
                }
              }
            }
            solvableOpt match {
              case Some(vd) =>
                // solve the unknown by equating it to all its bounds
                val h = new History(Nil)
                h += s"solving ${vd.name}, for which no constraints are left, by equating it to its bounds"
                val hd::tl = getbounds(vd.name)
                val r = solve(vd.name, hd.bound)(h + "registering solution")
                if (!r) return false
                val bdR = tl forall {bd =>
                  check(Equality(Stack.empty, hd.bound, bd.bound, vd.tp))(h + "equating to other bounds")
                }
                if (!bdR) return false
              case None =>
                // find a constraint that can be discharged by non-uniquely solving an unknown
                val nonUniqueTryable = delayed.filter {dc => dc.suffices.isDefined}
                val discharged = nonUniqueTryable.find {dc =>
                  val hist = dc.history
                  hist += "non-uniquely solving unknowns as sufficient condition to discharge constraint"
                  val allSolved = dc.suffices.get.forall {e =>
                    check(e)(hist.branch)
                  }
                  val solvedHere = getSolvedVariables diff solved
                  if (allSolved && solvedHere.nonEmpty) {
                    removeConstraint(dc)
                    // add the information to the history of other constraints in case those lead to errors later
                    val solvedHereS = "(unknowns " + solvedHere.mkString(", ") + " were solved non-uniquely on a separate branch)"
                    delayed.foreach {dc =>
                      if (dc.freeVars.exists(solvedHere.contains))
                        dc.history += solvedHereS
                    }
                    // skip the find-iteration and try other constraints normally again
                    true
                  } else {
                    false
                  }
                }
                if (discharged.isEmpty) {
                  // nothing left to try - give up
                  // even if there are no errors, there might still be unsolved unknowns and constraints left
                  return errors.isEmpty
                }
            }
       }
     }
     true // impossible but needed for Scala
   }
   private def prepareJ(j: Judgement) = j match {
      case Typing(stack, tm, tp, typS) =>
         Typing(prepareS(stack), prepare(tm), prepare(tp, true), typS)
      case Subtyping(stack, tp1, tp2) =>
         Subtyping(prepareS(stack), prepare(tp1), prepare(tp2))
      case Equality(stack, tm1, tm2, tp) =>
         Equality(prepareS(stack), prepare(tm1, true), prepare(tm2, true), tp.map {x => prepare(x, true)})
      case Universe(stack, tm) =>
         Universe(prepareS(stack), prepare(tm))
      case Inhabitable(stack, tp) =>
         Inhabitable(prepareS(stack), prepare(tp))
      case Inhabited(stack, tp) =>
         Inhabited(prepareS(stack), prepare(tp, true))
      case IsContext(stack, cont) =>
         IsContext(prepareS(stack), prepare(cont))
      case EqualityContext(stack, c1, c2, a) =>
         EqualityContext(prepareS(stack), prepare(c1, true), prepare(c2, true), a)
   }
   private def prepareS(s: Stack) = {
      Stack(s.context map {t => prepare(t)}) // this used to fully simplify (without definition expansion)
   }
   private def prepare(o: Obj, covered: Boolean = false): o.ThisType = {
      substituteSolution(o) // no need to simplify anymore because no beta-redexes are introduced
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
              case Some(FreeOrAny(tpCon,tp)) =>
                if (tp.freeVars.exists(solution.isDeclared)) {
                  // if the type still has unknowns, there's no point in trying to solve the unknown here
                  error("unsolved unknown of unknown type: " + vd.name)
                } else {
                  findUniqueInhabitant(tp)(Stack(tpCon), history) match {
                    case Some(p) =>
                      solve(vd.name, FreeOrAny(tpCon,p))
                    case None =>
                      if (vd.name.startsWith(ParseResult.VariablePrefixes.explicitUnknown)) {
                        // the user explicitly asked for this term to be filled in
                        solve(vd.name, FreeOrAny(tpCon, Hole(tp)))
                        error("unsolved hole", Some(Level.Gap))
                      } else {
                        error("unsolved (typed) unknown: " + vd.name)
                      }
                  }
                }
            }
      }
   }
}

//TODO bounds should be saved as a part of the context (that may require an artificial symbol)
/**
  * The SolverState represents the state of the solver. The solver state is basically implemented as a stack where
  * the top element is the current state of the [[Solver]] and everything below the top elements are saved, former
  * states of the solver
  *
  * @param _solution tracks the solution, initially equal to unknowns, then a definiens is added for every solved variable
  * @param _bounds contains the bounds used for subtyping
  * @param _dependencies tracks the dependencies in reverse order of encountering
  * @param _delayed tracks the delayed constraints, in any order
  * @param _errors tracks the errors in reverse order of encountering
  * @param parent the parent SolverState, usually this is a former, saved state of the [[Solver]]
  */
case class SolverState( _solution: Context = Context.empty,  _bounds: IListMap[LocalName,List[TypeBound]] = new IListMap[LocalName,List[TypeBound]](),
                        _dependencies: List[CPath] = Nil,  _delayed: List[DelayedConstraint] = Nil,  _errors : List[SolverError] = Nil, parent : Option[SolverState] = None ) {


  /**
    *
    * @return the latest saved [[SolverState]], if there is none, then this function returns "this"
    *         this behaviour is useful for undo actions in the interactive prover since
    */
  def last = parent.getOrElse(this)

  /**
    * adds a new state in front of the saved states, "this" becomes a saved state and the new state becomes the descendant of "this
    *
    * @param _solution tracks the solution, initially equal to unknowns, then a definiens is added for every solved variable
    * @param _bounds contains the bounds used for subtyping
    * @param _dependencies tracks the dependencies in reverse order of encountering
    * @param _delayed tracks the delayed constraints, in any order
    * @param _errors tracks the errors in reverse order of encountering
    * @return
    */
  def pushState( _solution: Context = this._solution,  _bounds: IListMap[LocalName,List[TypeBound]] = this._bounds,
                 _dependencies: List[CPath] = this._dependencies,  _delayed: List[DelayedConstraint] = this._delayed,    _errors : List[SolverError] = this._errors) = {
    val tmp = this.copy(_solution , _bounds , _dependencies, _delayed, _errors,parent = Some(this))
    tmp
  }

}


/** auxiliary methods and high-level type reconstruction API */
object Solver {
  /** counter for debugging */
  var checkId = 0
  def breakAfter(id: Int) {
    if (checkId >= id)
      ()
  }
  /** base for client property keys */
  val propertyURI = utils.mmt.baseURI / "clientProperties" / "solver"
  
  /** type reconstruction: checks a single term against a partially known type
   *
   *  @param tm the term (as a [[ParseResult]])
   *  @param expectedType the expected type and the unknowns that additionally occur in it; if omitted a fresh unknown is generated for the type
   *  @param rulesOpt the typing rules if known (to avoid computing them again)
   *  @return Left(term, type) if successful,  Right(solver) otherwise; in the latter case, use e.g. solver.logState to print the typing errors
   */
  def check(controller: Controller, stack: Stack, tm: Term, expectedType: Option[(Context,Term)] = None, rulesOpt: Option[RuleSet] = None): Union[(Term,Term),Solver] = {
      val ParseResult(unknowns,free,tmU) = ParseResult.fromTerm(tm)
      val (etpUnknowns,etp) = expectedType.getOrElse {
        val v = LocalName("expected_type")
        (Context(VarDecl(v)), OMV(v))
      }
      val j = Typing(stack, tmU, etp, None)
      val cu = CheckingUnit(None, stack.context, unknowns ++ etpUnknowns, j)
      val rules = rulesOpt.getOrElse(RuleSet.collectRules(controller, stack.context))
      val solver = new Solver(controller, cu, rules)
      solver.applyMain
      if (solver.checkSucceeded) solver.getSolution match {
         case Some(_) =>
           val tmR = solver.substituteSolution(tmU)
           val tmI = solver.substituteSolution(etp)
           Left((tmR, tmI))
         case None => Right(solver)
      } else
         Right(solver)
  }
  /** infers the type of a term that is known to be well-formed */
  def infer(controller: Controller, context: Context, tm: Term, rulesOpt: Option[RuleSet],unknowns:Context = Context.empty): Option[Term] = {
      val rules = rulesOpt.getOrElse {
         RuleSet.collectRules(controller, context)
      }
      implicit val stack = Stack(Context.empty)
      implicit val history = new History(Nil)
      val cu = CheckingUnit(None, context, unknowns, null) // awkward but works because we do not call applyMain
      val solver = new Solver(controller, cu, rules)
      val tpOpt = solver.inferType(tm, true)
      val tpSOpt = tpOpt map {t => solver.simplify(t)}
      tpSOpt//.map(t => solver.substituteSolution(t)) // map {tp => solver.simplify(tp)}
  }

  /** checks a term without unknowns against a type, returns the solver if not successful */
  def checkType(controller: Controller, context: Context, tm: Term, tp: Term): Option[Solver] = {
      val j = Typing(Stack(Context.empty), tm, tp, None)
      val cu = CheckingUnit(None, context, Context.empty, j)
      val rules = RuleSet.collectRules(controller, context)
      val solver = new Solver(controller, cu, rules)
      solver.applyMain
      if (solver.checkSucceeded) None else Some(solver)
  }

  /** create an unknown whose solution may contain certain variables */
  def makeUnknown(name: LocalName, args: List[Term]) = OMAorAny(OMV(name), args)
  
  /**
    * tests a term for the occurrence of an unknown variables that can be isolated by applying solution rules
    *
    * @param rules the solution rules to test with
    * @param unknowns the list of variables to try to isolate
    * @param t the term in which to test
    * @return a pair (rs, n) such that applying rs to t (head first) has a chance of isolating n
    */
   def findSolvableVariable[A<:SolutionRule](rules: Iterable[A], unknowns: Context, t: Term): Option[(List[A],LocalName)] = t match {
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
        None
      case OMAorAny(OMV(m), _) if unknowns.isDeclared(m) => Some((Nil, m))
      case _ => None
   }
}

/** used by [[Solver]] to store inferred types with terms */
object InferredType extends TermProperty[(Branchpoint,Term)](Solver.propertyURI / "inferred")

/** used by [[Solver]] to mark a term as head-normal: no simplification rule can change the head symbol */
class Stability(id: Int) extends BooleanTermProperty(Solver.propertyURI / "stability" / id.toString)

object Stability {
  private var id = -1
  def make = {id += 1; new Stability(id)}
}

/** stores a type bound for an unknown as used in the [[Solver]] */
case class TypeBound(bound: Term, upper: Boolean)

/** error/warning produced by [[Solver]] */
case class SolverError(excuse: Option[Level.Excuse], history: History,msgO : Option[(Obj => String) => String]= None) {
  def msg(implicit cont : Obj => String) = msgO.map(_.apply(cont)).getOrElse(history.steps.head.present(cont))
}

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

// TODO variable bounds are missing
/** experimental backtracking support in [[Solver]] */
class Branchpoint(val parent: Option[Branchpoint], val delayed: List[DelayedConstraint], val depLength: Int, val solution: Context) {
  def isRoot = parent.isEmpty
  /** @return true if this branch contains anc */
  def descendsFrom(anc: Branchpoint): Boolean = parent.contains(anc) || (parent match {
    case None => false
    case Some(p) => p.descendsFrom(anc)
  })
}
