package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._

//import info.kwarc.mmt.api.objects._
import libraries._
import modules._
import symbols._
import frontend._
import objects.Conversions._
import scala.collection.mutable.{HashSet,HashMap}

/* ideas
 * inferType guarantees well-formedness (not done yet by LambdaTerm)
 *   but what if there are unknowns whose type cannot be inferred? Is that even possible?
 * limitedSimplify must include computation, definition expansion, but can stop on GlobalChange; but safety is usually needed
 * constants have equality rule: injectivity and implicit arguments used to obtain necessary/sufficient condition (not preserved by morphism); congruence if no rule applicable
 */

/** A wrapper around a Judgement to maintain meta-information while a constraint is delayed */
class DelayedConstraint(val constraint: Judgement, private var currentLevel: Int, until: Int, val history: History) {
  private val freeVars = constraint.freeVars
  private var activatable = false
  /** This must be called whenever a variable that may occur free in this constraint has been solved */
  def solved(names: List[LocalName]) {
     if (! activatable && (names exists {name => freeVars contains name})) activatable = true
  }
  /** @return true iff, since delaying, a variable has been solved that occurs free in this Constraint */
  def isActivatable(level: Int): Boolean = {
     val newLevel = level > currentLevel
     currentLevel = level // we assume the level increases monotonously
     (level >= until) && (newLevel || activatable)
  }
  override def toString = constraint.toString
}

/** wrapper for classes that can occur in the [[History]] */
trait HistoryEntry {
   /** for user-facing rendering */
   def present(implicit cont: Obj => String): String
}

/** a HistoryEntry that consists of a string, meant as a log or error message */
case class Comment(text: Unit => String) extends HistoryEntry {
   override def toString = text()
   def present(implicit cont: Obj => String) = text()
}

/**
 * The History is a branch in the tree of decisions, messages, and judgements that occurred during type-checking
 * 
 * The most import History's are those ending in an error message.
 * See [[Solver.getErrors]]
 * 
 * @param the nodes of the branch, from leaf to root
 */
class History(private var steps: List[HistoryEntry]) {
   /** creates and returns a new branch with a child appended to the leaf */
   def +(e: HistoryEntry) : History = new History(e::steps)
   /** shortcut for adding a Comment leaf */
   def +(s: => String) : History = this + new Comment(_ => s)
   /** appends a child to the leaf */
   def +=(e: HistoryEntry) {steps ::= e}
   /** appends a child to the leaf */
   def +=(s: => String) {this += Comment(_ => s)}
   /** creates a copy of the history that can be passed when branching */
   def branch = new History(steps)
   /** get the steps */
   def getSteps = steps
   /**
    * A History produced by the ObjectChecker starts with the ValidationUnit, but the error is only encountered along the way.
    * 
    * @return an educated guess which suffix of the history is most useful 
    */
   def narrowDownError : History = {
      // idea: we start at the comment immediately before the last WFJudgement before the first other Judgement
      var i = steps.length - 1
      var lastWFJ = i
      var continue = true
      while (continue && i >= 0) {
         steps(i) match {
            case j: WFJudgement => lastWFJ = i
            case j: Judgement => continue = false
            case _ =>
         }
         i -= 1
      }
      if (lastWFJ+1 < steps.length && steps(lastWFJ+1).isInstanceOf[Comment]) lastWFJ += 1
      new History(steps.take(lastWFJ+1))
   }
}

object InferredType extends TermProperty[Term](utils.mmt.baseURI / "clientProperties" / "solver" / "inferred")

/**
 * A Solver is used to solve a system of constraints about Term's given as judgments.
 * 
 * The judgments may contain unknown variables (also called meta-variables or logic variables);
 * variables may represent any MMT term, i.e., object language terms, types, etc.;
 * the solution is a Substitution that provides a closed Term for every unknown variable.
 * (Higher-order abstract syntax should be used to represent unknown terms with free variables as closed function terms.) 
 * 
 * The Solver decomposes the judgments individually by applying typing rules, collecting (partial) solutions along the way and possibly delaying unsolvable judgments.
 * If the unknown variables are untyped and rules require a certain type, the Solver adds the type.
 * 
 * Unsolvable constraints are delayed and reactivated if later solving of unknowns provides further information.
 * 
 * @param controller an MMT controller that is used to look up Rule's and Constant's. No changes are made to the controller.
 * @param unknowns the list of all unknown variables including their types and listed in dependency order;
 *   unknown variables may occur in the types of later unknowns.
 * 
 * Use: Create a new instance for every problem, call apply on all constraints, then call getSolution.  
 */
class Solver(val controller: Controller, theory: Term, initUnknowns: Context) extends Logger {
   /** tracks the solution, initially equal to unknowns, then a definiens is added for every solved variable */ 
   private var solution : Context = initUnknowns
   /** the unknowns that were solved since the last call of activate (used to determine which constraints are activatable) */
   private var newsolutions : List[LocalName] = Nil
   /** tracks the delayed constraints, in any order */ 
   private var delayed : List[DelayedConstraint] = Nil
   /** tracks the errors in reverse order of encountering */
   private var errors: List[History] = Nil
   /** tracks the dependecnies in reverse order of encountering */
   private var dependencies : List[CPath] = Nil
   /** currentLevel increases during checking, at higher levels more operations are applicable
    *  currently: level 0 for sufficient-necessary rules
    *             level 1 for only sufficient rules
    */
   private var currentLevel = 0
   /** the highest level, if nothing is applicable at this level, we give up */
   private val highestLevel = 1
   /** true if unresolved constraints are left */
   def hasUnresolvedConstraints : Boolean = ! delayed.isEmpty
   /** true if unsolved variables are left */
   def hasUnsolvedVariables : Boolean = solution.toSubstitution.isEmpty
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
    * This solution may contain unsolved variables, and there may be unresolved constraints. 
    */   
   def getUnsolvedVariables : Context = solution.filter(_.df.isEmpty)
   /**
    * @return the current list of unresolved constraints
    */
   def getConstraints : List[DelayedConstraint] = delayed
   /**
    * @return the current list of errors and their history
    */
   def getErrors : List[History] = errors
   /**
    * @return the current list of dependencies
    */
   def getDependencies : List[CPath] = dependencies

   /** for Logger */ 
   val report = controller.report
   /** prefix used when logging */ 
   val logPrefix = "object-checker"
   /** shortcut for the RuleStore of the controller; used to retrieve Rule's */
   private val ruleStore = controller.extman.ruleStore
   /** the SubstitutionApplier to be used throughout */
   private implicit val sa = new MemoizedSubstitutionApplier
   /** shortcut for UOM simplification */
   private def simplify(t : Term) = controller.uom.simplify(t, theory, solution)
   /** used for rendering objects, should be used by rules if they want to log */
   implicit val presentObj : Obj => String = controller.presenter.asString
   
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
         val unsolved = solution.variables.filter(_.df.isEmpty).map(_.name)
         if (! unsolved.isEmpty)
            report(prefix, "unsolved: " + unsolved.mkString(", "))
         if (! errors.isEmpty) {
            report(prefix, "errors:")
            logGroup {
               errors.foreach {e =>
                  report(prefix, "error: " + e.getSteps.head.present)
                  logHistory(e)
               }
            }
         }
         if (! delayed.isEmpty) {
            report(prefix, "constraints (current level is " + currentLevel + "):")
            logGroup {
               delayed.foreach {d =>
                  report(prefix, d.constraint.present)
                  logGroup {
                     report(prefix, d.constraint.presentAntecedent(_.toString))
                     report(prefix, d.constraint.presentSucceedent(_.toString))
                     if (errors.isEmpty)
                        // if there are no errors, see the history of the constraints
                        logHistory(d.history)
                  }
               }
            }
         }
      }
   }
   
   /** retrieves the type type of a constant and registers the dependency
    *
    * returns nothing if the type could not be reconstructed
    */
   def getType(p: GlobalName): Option[Term] = {
      val c = controller.globalLookup.getConstant(p)
      if (c.tpC.analyzed.isDefined) dependencies ::= p $ TypeComponent
      c.tpC.analyzed
   }
   /** retrieves the definiens of a constant and registers the dependency
    *
    * returns nothing if the type could not be reconstructed
    */
   def getDef(p: GlobalName) : Option[Term] = {
      val c = controller.globalLookup.getConstant(p)
      if (c.dfC.analyzed.isDefined) dependencies ::= p $ DefComponent
      c.dfC.analyzed
   }

   /** delays a constraint for future processing
    * @param c the Judgement to be delayed
    * @param until the level at which to activate (must satisfy 0 <= until <= highestLevel)
    * @return true (i.e., delayed Judgment's always return success)
    */
   private def delay(j: Judgement, until: Int = currentLevel)(implicit history: History): Boolean = {
      // testing if the same judgement has been delayed already
      if (delayed.exists(d => d.constraint hasheq j)) {
         log("delaying (exists already): " + j.present)
      } else {
         log("delaying until level " + until + ": " + j.present)
         history += "(delayed)"
         val dc = new DelayedConstraint(j, currentLevel, until, history)
         delayed ::= dc
      }
      true
   }
   /**
    * selects an activatable constraint: a previously delayed constraint if one of its free variables has been solved since
    * @return an activatable constraint if available
    */
   private def activatable: Option[DelayedConstraint] = {
      delayed foreach {_.solved(newsolutions)}
      newsolutions = Nil
      delayed find {_.isActivatable(currentLevel)} match {
         case None =>
            if (currentLevel >= highestLevel) {
               None
            } else {
               currentLevel += 1
               log("switching to level " + currentLevel)
               activatable
            }
         case Some(dc) => Some(dc)
      }
   }
   /** registers the solution for an unknown variable
    * 
    * If a solution exists already, their equality is checked.
    * @param name the solved variable
    * @param value the solution; must not contain object variables, but may contain meta-variables that are declared before the solved variable
    * @return true unless the solution differs from an existing one
    * precondition: value is well-typed if the overall check succeeds
    */
   def solve(name: LocalName, value: Term)(implicit stack: Stack, history: History) : Boolean = {
      log("solving " + name + " as " + value)
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.df.isDefined) {
         check(Equality(stack, value, solved.df.get, solved.tp))(history + "solution must be equal to previously found solution") //TODO
      } else {
         val valueS = simplify(value ^^ left.toPartialSubstitution) // substitute solutions of earlier variables that have been found already
         parser.SourceRef.delete(valueS) // source-references from looked-up types may sneak in here
         val rightS = right ^^ (OMV(name) / valueS) // substitute in solutions of later variables that have been found already
         solution = left ::: solved.copy(df = Some(valueS)) :: rightS
         newsolutions = name :: newsolutions
         true
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
   /** registers the solved type for a variable
    * 
    * If a type exists already, their equality is checked.
    * @param name the variable
    * @param value the type; must not contain object variables, but may contain meta-variables that are declared before the solved variable
    * @return true unless the type differs from an existing one
    * precondition: value is well-typed if the the overall check succeeds
    */
   private def solveType(name: LocalName, value: Term)(implicit stack: Stack, history: History) : Boolean = {
      val valueS = simplify(value)
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.tp.isDefined)
         check(Equality(stack, valueS, solved.tp.get, None))(history + "solution for type must be equal to previously found solution") //TODO
      else {
         solution = left ::: solved.copy(tp = Some(valueS)) :: right
         //no need to register this in newsolutions
         true
      }
   }
   
   /** registers an error and returns false */
   def error(message: => String)(implicit history: History): Boolean = {
      errors ::= history + message
      // maybe return true so that more errors are found
      false
   }
   
   /** applies this Solver to one Judgement
    *  This method can be called multiple times to solve a system of constraints.
    *  @param j the Judgement
    *  @return false if the Judgment is definitely not provable; true if it has been proved or delayed
    */
   @scala.annotation.tailrec
   final def apply(j: Judgement, history: History = new History(Nil)) : Boolean = {
     implicit val h = history
     val subs = solution.toPartialSubstitution
     def prepare(t: Term) = simplify(t ^^ subs)
     def prepareS(s: Stack) =
        Stack(s.frames.map(f => Frame(f.theory, controller.uom.simplifyContext(f.context ^^ subs, theory, solution))))
     val mayhold = j match {
        case Typing(stack, tm, tp, typS) =>
           val tmS = tm ^^ subs
           check(Typing(prepareS(stack), tmS, prepare(tp), typS))
        case Equality(stack, tm1, tm2, tp) =>
           check(Equality(prepareS(stack), prepare(tm1), prepare(tm2), tp map prepare))
        case Universe(stack, tm, isIn) =>
           check(Universe(prepareS(stack), tm ^^ subs, isIn))
        case IsMorphism(stack, mor, from) =>
           checkMorphism(mor ^^ subs, prepare(from))(prepareS(stack), history)
     }
     if (mayhold) {
        //activate next constraint and recurse, if any left
        val dcOpt = activatable
        dcOpt match {
           case None => true
           case Some(dc) => 
              delayed = delayed filterNot (_ == dc)
              val j = dc.constraint
              //logState() // noticably slows down type-checking, only use for debugging
              log("activating: " + j.present)
              apply(j, dc.history)
        }
     } else false
   }

   /**
    * checks a judgement
    *  
    * This is the method that should be used to recurse into a hypothesis.
    * It handles logging and error reporting and delegates to specific methods based on the judgement.
    *  
    * The apply method is similar but additionally simplifies the judgment.
    */
   def check(j: Judgement)(implicit history: History): Boolean = {
      history += j
      log(j.presentSucceedent)
      logGroup {
         log(j.presentAntecedent)
         j match {
            case j: Typing   => checkTyping(j)
            case j: Equality => checkEquality(j)
            case j: Universe => checkUniverse(j)
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
    *  
    */
   private def checkTyping(j: Typing)(implicit history: History) : Boolean = {
     val tm = j.tm
     val tp = j.tp
     implicit val stack = j.stack
     tm match {
       // the foundation-independent cases
       case OMV(x) => (solution ++ stack.context)(x).tp match {
         case None =>
            if (solution.isDeclared(x))
              solveType(x, tp) //TODO: check for occurrences of bound variables?
            else
              false //untyped variable type-checks against nothing
         case Some(t) => check(Equality(stack, t, tp, None))
       }
       case OMS(p) =>
         getType(p) match {
           case None => getDef(p) match {
             case None => false //untyped, undefined constant type-checks against nothing
             case Some(d) => check(Typing(stack, d, tp, j.tpSymb)) // expand defined constant
           }
           case Some(t) => check(Equality(stack, t, tp, None))
         }
       // the foundation-dependent cases
       // bidirectional type checking: first try to apply a typing rule (i.e., use the type early on), if that fails, infer the type and check equality
       case tm =>
         limitedSimplify(tp,ruleStore.typingRules) match {
           case (tpS, Some(rule)) =>
             rule(this)(tm, tpS)
           case (tpS, None) =>
             // either this is an atomic type, or no typing rule is known
             inferType(tm)(stack, history.branch) match {
               case Some(itp) =>
                  check(Equality(stack, itp, tpS, None))(history + ("inferred type must be equal to expected type; the term is: " + presentObj(tm)))
               case None =>
                  delay(Typing(stack, tm, tpS, j.tpSymb))(history + "type inference failed")
             }
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
   def inferType(tm: Term)(implicit stack: Stack, history: History): Option[Term] = {
     log("inference: " + presentObj(tm) + " : ?")
     history += "inferring type of " + presentObj(tm)
     // return previously inferred type, if any (previously unsolved variables are substituted)
     InferredType.get(tm) match {
        case s @ Some(_) => return s.map(_ ^^ solution.toPartialSubstitution)
        case _ =>
     }
     val res = logGroup {
        log("in context: " + presentObj(stack.context))
        val resFoundInd = tm match {
          //foundation-independent cases
          case OMV(x) =>
             history += "lookup in context"
             (solution ++ stack.context)(x).tp
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
             return Some(OMS(l.rt.synType))
          case _ => None
        }
        //foundation-dependent cases if necessary
        //syntax-driven type inference
        resFoundInd orElse {
            val (tmS, ruleOpt) = limitedSimplify(tm,ruleStore.inferenceRules)
            ruleOpt match {
              case Some(rule) =>
                 history += ("applying rule for " + rule.head.name.toString)
                 rule(this)(tmS)
              case None => None
            }
        }
     }
     log("inferred: " + res.getOrElse("failed"))
     // remember inferred type
     if (res.isDefined) InferredType.put(tm, res.get)
     res
   }

   /** proves a Universe Judgment
    * @param j the judgment
    * @return true if succeeded or delayed
    *
    * pre: context is covered
    *
    * post: universe judgment is covered
    */
   private def checkUniverse(j : Universe)(implicit history: History): Boolean = {
     implicit val stack = j.stack
     if (j.isIn) {
         // the meaning of the judgement is u:U for some universe U, we infer U and recurse
         inferType(j.univ)(stack, history + "inferring universe") match {
             case None =>
                delay(j)
             case Some(univI) =>
                check(Universe(stack, univI, false))
          }
     } else {
        // the meaning of the judgement is that u itself is a universe U, we apply universe rules
        limitedSimplify(j.univ, ruleStore.universeRules) match {
           case (uS, Some(rule)) => rule(this)(uS)
           case (uS, None) => delay(Universe(j.stack, uS, false))  
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
      val tm1 = j.t1
      val tm2 = j.t2
      val tpOpt = j.t
      implicit val stack = j.stack
      val tm1S = simplify(tm1)
      val tm2S = simplify(tm2)
      // 1) base cases, e.g., identical terms, solving unknowns
      // identical terms
      if (tm1S hasheq tm2S) return true
      // different literals are always non-equal
      (tm1S, tm2S) match {
         case (l1: OMLIT, l2: OMLIT) => if (l1.value != l2.value) return false
         case _ =>
      }
      // solve an unknown
      val solved = solveEquality(tm1S, tm2S, tpOpt) || solveEquality(tm2S, tm1S, tpOpt)
      if (solved) return true
      
      // 2) find a TermBasedEqualityRule based on the heads of the terms
      (tm1.head, tm2.head) match {
         case (Some(c1), Some(c2)) =>
            ruleStore.termBasedEqualityRules(c1,c2) foreach {rule =>
               // we apply the first applicable rule
               val contOpt = rule(this)(tm1,tm2,tpOpt)
               if (contOpt.isDefined) {
                  return contOpt.get.apply
               }
            }
         case _ =>
      }

      // 3) find a TypeBasedEqualityRule based on the head of the type
      // first infer the type if it has not been given in tpOpt
      val tp = tpOpt match {
        case Some(tp) => tp
        case None =>
           val itp = inferType(tm1S) orElse inferType(tm2S)(stack, history + "inferring omitted type")
           itp.getOrElse(return delay(Equality(stack, tm1S, tm2S, None)))
      }
      // try to simplify the type until an equality rule is applicable 
      limitedSimplify(tp, ruleStore.typeBasedEqualityRules) match {
         case (tpS, Some(rule)) => rule(this)(tm1S, tm2S, tpS)
         case (tpS, None) =>
            // this is either a base type or an equality rule is missing
            // TorsoNormalForm is useful to inspect terms of base type
            val tm1T = TorsoForm.fromHeadForm(tm1S)
            val tm2T = TorsoForm.fromHeadForm(tm2S)
            checkEqualityExpandDef(List(tm1T), List(tm2T), false)(stack, history, tp)
      }
   }
   
   /* ********************** auxiliary methods of checkEquality ***************************/ 
   
   /**
    * definition expansion for the torso of a term
    * the result is immediately simplified
    * @param t a term whose torso should be expanded
    * @return (t', changed) where t' is the resulting term and changed is true if an expansion occurred
    */ 
   private def expandTorsoDef(t: TorsoForm, safe: Boolean = false) : (TorsoForm, Boolean) = t.torso match {
      case OMS(c) =>
         getDef(c) match {
            case Some(df) =>
               val tE = TorsoForm(df, t.apps).toHeadForm
               val tES = if (safe) tE else simplify(tE)
               (TorsoForm.fromHeadForm(tES), true)
            case None => (t, false)
         }
      case _ => (t, false)
   }
   /**
    * like expandTorsoDef but for Terms
    */ 
   private def expandTorsoDef(t: Term) : (Term, Boolean) =  {
      val tf = TorsoForm.fromHeadForm(t)
      val (tfS, b) = expandTorsoDef(tf, true)
      if (b) (tfS.toHeadForm, true)
      else (t, false)
   }
   
   /**
    * finds a TermBasedEqualityRule that applies to t1 and any of the terms in t2
    * rules are looked up based on the torsos of the terms
    * first found rule is returned
    */
   private def findEqRule(t1: TorsoForm, others: List[TorsoForm])(implicit stack : Stack, history: History, tp: Term) : Option[Continue[Boolean]] = {
      OMS.unapply(t1.torso) foreach {c1 =>
         others foreach {o => 
            OMS.unapply(o.torso) foreach {c2 =>
               ruleStore.termBasedEqualityRules(c1,c2) foreach {rule =>
                  val contOpt = rule(this)(t1.toHeadForm, o.toHeadForm, Some(tp))
                  if (contOpt.isDefined) return contOpt
               }
            }
         }
      }
      return None
   }
   
   /**
    * checks equality t1 = t2 at an atomic type by expanding the defined constant in the torso
    * 
    * Both t1 and t2 are remembered as the chain of terms resulting from repeated definition expansion 
    * @param torsos1 the chain of terms that led to t1, starting with L (must be non-empty)
    * @param torsos2 the chain of terms that led to t2, starting with L (must be non-empty)
    * @param t2Final if true, the definition expansion is not applicable to the torso of t2
    * @param flipped if true, t1 and t2 have been flipped during recursive calls (false by default)
    *   This parameter is not semantically necessary but carried around to undo flipping when calling other functions.
    * @return true if equal 
    */
   private def checkEqualityExpandDef(torsos1: List[TorsoForm], torsos2: List[TorsoForm], t2Final: Boolean, flipped: Boolean = false)(implicit stack : Stack, history: History, tp: Term) : Boolean = {
       log("equality (expanding definitions): " + torsos1.head.toHeadForm + " = " + torsos2.head.toHeadForm)
       val t1 = torsos1.head
       // see if we can expand the head of t1
       val (tE, changed) = expandTorsoDef(t1)
       if (changed) {
          log("left term expanded and simplified to " + tE)
          // check if it is identical to one of the terms known to be equal to t2
          if (torsos2.exists(_ hasheq tE)) {
             log("success by identity")
             return true
          }
          // check if there is a TermBasedEqualityRule that applies to the new torso of t1 and the torso of a term equal to t2 
          val contOpt = findEqRule(t1, torsos2)
          contOpt foreach {cont => return cont.apply()}
       }
       // if we failed to prove equality, we have multiple options:
       (changed, t2Final) match {
          // t1 expanded, t2 cannot be expanded anymore --> keep expanding t1
          case (true, true)      => checkEqualityExpandDef(tE::torsos1, torsos2, true, flipped)
          // t1 expanded, t2 can still be expanded anymore --> continue expanding t2
          case (true, false)     => checkEqualityExpandDef(torsos2, tE::torsos1, false, ! flipped)
          // t1 cannot be expanded anymore but t2 can ---> continue expanding t2
          case (false, false)    => checkEqualityExpandDef(torsos2, torsos1, true, ! flipped)
          // neither term can be expanded --> try congruence rule as last resort
          case (false, true)     =>
             // if necessary, we undo the flipping of t1 and t2
             val s1 = if (flipped) torsos2.head else t1
             val s2 = if (flipped) t1 else torsos2.head
             val j = Equality(stack, s1.toHeadForm, s2.toHeadForm, Some(tp))
             if (currentLevel < 1)
                delay(j, 1)
             else
                //TODO: if there is a simplification-rule for one of the heads, expand definition at the next lower level (and then iteratively)
                //until a simplification-rule becomes applicable at toplevel
                //That is necessary if a rule becomes applicable eventually.
                //But it is a huge overhead if we don't actually want to simplify here. 
                checkEqualityCongruence(s1, s2)
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
   

   /** tries to solve an unknown occurring as the torso of tm1 in terms of tm2.
    * 
    * This method should not be called by users (instead, use check(Equality(tm1,tm2,...))).
    * It is only public because it serves as a callback for SolutionRule's.
    * (SolutionRule's should not recurse into check(Equality(...)) because it tends to lead to cycles.)
    */
   def solveEquality(tm1: Term, tm2: Term, tpOpt: Option[Term])(implicit stack: Stack, history: History): Boolean = {
      tm1 match {
         //foundation-independent case: direct solution of an unknown variable
         case OMV(m) =>
            val mIndex = solution.index(m).getOrElse {
               // if m is not declared in unknowns, i.e., is a bound variable, nothing to do 
               return false
            }
            // fvsEarlier: all unknowns in tm2 are declared before m
            val fvs = tm2.freeVars
            val fvsEarlier = fvs.forall {v =>
               solution.index(v) match {
                  case Some(vIndex) => vIndex > mIndex
                  case None => false
               }
            }
            if (fvsEarlier) {
               // we can solve m already, the remaining unknowns in tm2 can be filled in later
               val res = solve(m, tm2)
               tpOpt foreach {case tp => solveType(m, tp)}
               res
            } else
               delay(Equality(stack,tm1,tm2,tpOpt))
               // meta-variable solution has free variable --> type error unless free variable disappears later
               // note: tm2 should be simplified - that may make a free variable disappear
         //apply a foundation-dependent solving rule selected by the head of tm1
         case TorsoNormalForm(OMV(m), Appendage(h,_) :: _) if solution.isDeclared(m) && ! tm2.freeVars.contains(m) => //TODO what about occurrences of m in tm1?
            ruleStore.solutionRules.get(h) match {
               case None => false
               case Some(rule) => rule(this)(tm1, tm2)
            }
         case _ => false
      }
   }
   /* ********************** end of auxiliary methods of checkEquality ***************************/
   
   private def checkMorphism(mor: Term, from : Term)(implicit stack: Stack, history: History) : Boolean = {
     log("typing: " + mor + " : " + from)
     logGroup {
        log("in context: " + stack.context)
        from match {
          case OMMOD(p) => controller.globalLookup.getTheory(p) match {
            case thdf : DefinedTheory =>  checkMorphism(mor, thdf.df)
            case thd : DeclaredTheory =>
              val clist : List[Declaration] = thd.getDeclarations filter (p => !p.isInstanceOf[Structure])  // list of constants in the domain theory
              //val oclist : List[Declaration] = clist.sortWith((x,y) => x.name.toString() <= y.name.toString()) //ordered list of constants in the domain theory
              mor match {
                case ExplicitMorph(rec, dom) =>
                  if (from == dom)
                  {
                    val ocassig = rec.fields.sortWith((x,y) => x._1.toString() <=  y._1.toString()) //ordered list of constants in the morphism
                    if (clist.map(_.name) == ocassig.map(_._1)) { //testing that the local names of the constants in the domain theory are the same as those in the morphism
                      (clist zip ocassig) forall (pair =>
                       {
                        inferType(OMID(pair._1.path)) match {    //matching type of constant from domain theory
                          case Some(tp) => checkTyping(Typing(stack, pair._2._2,OMM(tp,mor), None))
                          case None => true
                        }
                      })
                    }
                    else false
                  }
                 else false
                case _ => false
              }
            case _ => false
          }
          case _ => false
        }
     }
   }

   /** applies all ForwardSolutionRules of the given priority
    * @param priority exactly the rules with this Priority are applied */
   //TODO call this method at appropriate times
   private def forwardRules(priority: ForwardSolutionRule.Priority)(implicit stack: Stack, history: History): Boolean = {
      val results = solution.zipWithIndex map {
         case (vd @ VarDecl(x, Some(tp), None), i) =>
            implicit val con : Context = solution.take(i)
            limitedSimplify(tp, ruleStore.forwardSolutionRules) match {
               case (tpS, Some(rule)) if rule.priority == priority => rule(this)(vd)
               case _ => false 
            }
         case _ => false
      }
      results.exists(_ == true)
   }
   
   /** special case of the version below where we simplify until an applicable rule is found
    *  @param tm the term to simplify
    *  @param rm the RuleMap from which an applicable rule is needed
    *  @param stack the context of tm
    *  @return (tmS, Some(r)) where tmS = tm and r from rm is applicable to tmS; (tmS, None) if tm = tmS and no further simplification rules are applicable
    */  
   private def limitedSimplify[R <: Rule](tm: Term, rm: RuleMap[R])(implicit stack: Stack, history: History): (Term,Option[R]) =
      limitedSimplify[R](tm)(t => t.head flatMap {h => rm.get(h)})
   
   /** applies ComputationRule's to simplify a term until some condition is satisfied;
    *  A typical case is transformation into weak head normal form.
    *  @param tm the term to simplify (It may be simple already.)
    *  @param simple a term is considered simple if this function returns a non-None result
    *  @param stack the context of tm
    *  @return (tmS, Some(a)) if tmS is simple and simple(tm)=tmS; (tmS, None) if tmS is not simple but no further simplification rules are applicable
    */
    //TODO test for deep definition expansion
   def limitedSimplify[A](tm: Term)(simple: Term => Option[A])(implicit stack: Stack, history: History): (Term,Option[A]) = {
      simple(tm) match {
         case Some(a) => (tm,Some(a))
         case None => tm.head match {
            case Some(h) =>
               val rOpt = ruleStore.computationRules.get(h)
               rOpt match {
                  case None =>
                     val (tmS, changed) = expandTorsoDef(tm)
                     if (changed) limitedSimplify(tmS)(simple)
                     else (tmS, None)
                  case Some(rule) =>
                     rule(this)(tm) match {
                        case Some(tmS) =>
                           log("simplified: " + tm + " ~~> " + tmS)
                           limitedSimplify(tmS.from(tm))(simple)
                        case None => 
                           val (tmS, changed) = expandTorsoDef(tm)
                           if (changed) limitedSimplify(tmS)(simple)
                           else (tmS, None)
                     }
               }
            case None => (tm, None)
         }
      }
   }
}

object Solver {
  /** reconstructs a single term and returns the reconstructed term and its type */
  def check(controller: Controller, stack: Stack, tm: Term): Option[(Term,Term)] = {
      val (unknowns,tmU) = parser.AbstractObjectParser.splitOffUnknowns(tm)
      val etp = LocalName("expected_type")
      val oc = new Solver(controller, stack.theory, unknowns ++ VarDecl(etp, None, None))
      val j = Typing(stack, tmU, OMV(etp), None)
      oc(j)
      oc.getSolution map {sub =>
          val tmR = tmU ^ sub
          (tmR, sub("expected_type").get) // must be defined if there is a solution
      }
  }
}