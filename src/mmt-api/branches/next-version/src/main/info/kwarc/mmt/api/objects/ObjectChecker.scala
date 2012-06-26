package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
//import info.kwarc.mmt.api.objects._
import libraries._
import modules._
import symbols._
import frontend._
import objects.Conversions._
import scala.collection.mutable.{HashSet,HashMap}

/** A wrapper around a Judgement to maintain meta-information while that Constraint is delayed */
class DelayedConstraint(val constraint: Judgement) {
  private val freeVars = constraint.freeVars
  private var activatable = false
  /** This must be called whenever a variable that may occur free in this constraint has been solved */
  def solved(names: List[LocalName]) {
     if (! activatable && (names exists {name => freeVars contains name})) activatable = true
  }
  /** @return true iff a variable has been solved that occurs free in this Constraint */
  def isActivatable: Boolean = activatable
  override def toString = constraint.toString
}

/**
 * A Solver is used to solve a system of constraints given as judgments about Term's
 * by applying typing rules to validate the judgments.
 * The judgments may contain unknown variables (also called meta-variables or logic variables);
 * variables may represent any MMT term, i.e., object language terms, types, etc.;
 * the solution is a Substitution that provides a closed Term for every unknown variable.
 * (Higher-order abstract syntax should be used for problem where the solutions are not closed.) 
 * Unsolvable constraints are delayed and reactivated if solving other constraints provides further information.
 * @param controller used for looking up Foundation's and Constant's. No changes are made to controller.
 * @param unknowns the list of all unknown variables with their types in dependency order;
 *   unknown variables may occur in the types of later unknowns.
 * Use: create a new instance for every problem, call apply on all constraints, then call getSolution  
 */
class Solver(controller: Controller, unknowns: Context) {
   /** tracks the solution, like unknowns but a definiens is added for every solved variable */ 
   private var solution : Context = unknowns
   /** the unknowns that were solved since the last call of activate (used to determine which constraints are activatable) */
   private var newsolutions : List[LocalName] = Nil
   /** tracks the delayed constraints, in any order */ 
   private var delayed : List[DelayedConstraint] = Nil
   /** true if unresolved constraints are left */
   def hasUnresolvedConstraints : Boolean = ! delayed.isEmpty
   /** true if unsolved variables are left */
   def hasUnsolvedVariables : Boolean = solution.toSubstitution.isEmpty
   /** the solution to the constraint problem
    * @return None if there are unresolved constraints or unsolved variables; Some(solution) otherwise 
    */
   def getSolution : Option[Substitution] = if (delayed.isEmpty) solution.toSubstitution else None

   /** a string representation of the current state */ 
   override def toString = {
      "unknowns: " + unknowns.toString + "\n" +
      "solution: " + solution.toString + "\n" +
      "constraints:\n" + delayed.mkString("  ", "\n  ", "\n") 
   }
   
   /** the content that stores all constants */
   private val content = controller.globalLookup
   /** retrieve the Foundation providing the semantics of a symbol, if any */
   private val ruleStore = controller.extman.ruleStore
   
   /** delays a constraint for future processing */
   private def delay(c: Judgement): Boolean = {
      val dc = new DelayedConstraint(c)
      delayed = dc :: delayed
      true
   }
   /** activates a previously delayed constraint if one of its free variables has been solved since */
   private def activate: Boolean = {
      delayed foreach {_.solved(newsolutions)}
      delayed find {_.isActivatable} match {
         case None => true
         case Some(dc) =>
           delayed = delayed filterNot (_ == dc)
           apply(dc.constraint)
      }
      newsolutions = Nil
      true
   }
   /** registers the solution for a variable; notifies all delayed constraints */
   //TODO solutions should also be propagated to currently active constraints
   private def solve(name: LocalName, value: Term): Boolean = {
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.df.isDefined)
         checkEquality(value, solved.df.get, solved.tp)(Context())
      else {
         solution = left ::: solved.copy(df = Some(value)) :: right
         newsolutions = name :: newsolutions
         true
      }
   }
   /** applies this Solver to one constraint
    *  this method can be called multiple times to solve a system of constraints
    *  @param c the constraint
    *  @return false only if the constraints are unsatisfiable; true if constraints have been resolved or delayed  
    */
   def apply(j: Judgement): Boolean = {
     println("apply: " + j)
     println(this)
     val subs = solution.toPartialSubstitution
     val mayhold = j match {
        case Typing(con, tm, tp) =>
           checkTyping(tm ^ subs, tp ^ subs)(con ^ subs)
        case Equality(con, tm1, tm2, tp) =>
           def prepare(t: Term) = simplify(t ^ subs)(con)
           checkEquality(prepare(tm1), prepare(tm2), tp map prepare)(simplifyCon(con ^ subs))
     }
     if (mayhold) activate else false
   }
   /** proves a TypingConstraint by recursively applying rules and solving variables where applicable,
    *  delays a constraint if unsolved variables preclude further processing
    *  checkTyping(tm, tp)(con) solves the judgment con |- tm : tp
    *  @return false only if the judgment does not hold; true if it holds or constraint have been delayed
    */
   def checkTyping(tm: Term, tp: Term)(implicit context: Context): Boolean = {
      println("typing: " + context + " |- " + tm + " : " + tp)
      tm match {
         // the foundation-independent cases
         case OMV(x) => (unknowns ++ context)(x).tp match {
            case None => false //untyped variable type-checks against nothing
            case Some(t) => checkEquality(t, tp, None)
         }
         case OMS(p) =>
            val c = content.getConstant(p)
            c.tp match {
               case None => c.df match {
                  case None => false //untyped, undefined constant type-checks against nothing
                  case Some(d) => checkTyping(d, tp) // expand defined constant 
               }
               case Some(t) => checkEquality(t, tp, None)
            }
         // the foundation-dependent cases
         case tm =>
            limitedSimplify(tp) {t => t.head flatMap {h => ruleStore.typingRules.get(h)}} match {
               case (tpS, Some(rule)) => rule(this)(tm, tpS)
               case (tpS, None) =>
                   // assume this is an atomic type
                   inferType(tm) match {
                      case Some(itp) => checkEquality(itp, tpS, None)
                      case None => delay(Typing(context, tm, tpS))
                   }
            }
      }
   }
   
   /** infers the type of a term
    * @return the inferred type, if inference succeeded
    */
   def inferType(tm: Term)(implicit context: Context): Option[Term] = {
      println("inferring: " + context + " |- " + tm + " : ?")
      tm match {
         //foundation-independent cases
         case OMV(x) => (unknowns ++ context)(x).tp
         case OMS(p) =>
            val c = content.getConstant(p)
            c.tp orElse {
               c.df match {
                  case None => None
                  case Some(d) => inferType(d) // expand defined constant 
               }
            }
         //foundation-dependent cases
         case tm =>
            val hd = tm.head.get //TODO
            ruleStore.inferenceRules.get(hd) match {
               case Some(rule) => rule(this)(tm)
               case None => None
            }
      }
   }
   
   /** handles an EqualityConstraint by recursively applying rules and solving variables where applicable,
    *  delays a constraint if unsolved variables preclude further processing
    *  @param tpOpt if empty, tm1 and tm2 may be ill-typed; if non-empty, they must also type-check at that type
    *  @return false only if the judgment does not hold; true if it holds or constraint have been delayed
    */
   def checkEquality(tm1: Term, tm2: Term, tpOpt: Option[Term])(implicit con: Context): Boolean = {
      println("equality: " + con + " |- " + tm1 + " = " + tm2 + " : " + tpOpt)
      // first, we check for some common cases where it's redundant to do induction on the type
      // identical terms
      if (tm1 == tm2) return true
      // solve an unknown
      val solved = tryToSolve(tm1, tm2) || tryToSolve(tm2, tm1)
      if (solved) return true

      // use the type for foundation-specific equality reasoning
      
      // first infer the type if it has not been given in tpOpt
      val tp = tpOpt match {
        case Some(tp) => tp
        case None =>
           val itp = inferType(tm1) orElse inferType(tm2)
           itp.getOrElse(return false)
      }
      // try to simplify the type until an equality rule is applicable 
      limitedSimplify(tp) {t => t.head flatMap {h => ruleStore.equalityRules.get(h)}} match {
         case (tpS, Some(rule)) => return rule(this)(tm1, tm2, tpS)
         case (tpS, None) =>
      }
      // no equality rule applicable - we are probably at a base type
      (tm1, tm2) match {
         //most important case: elimination rules applied to a constant
         case (OMA(OMS(h1), args1), OMA(OMS(h2), args2)) if h1 == h2 =>
            ruleStore.equalityRulesByHead.get(h1) match {
               case Some(rule) => rule(this)(tm1, tm2, tp)
               case None =>
                 //default: apply inverse congruence rule
                 if (args1.length == args2.length) {
                    (args1 zip args2) forall {case (a1,a2) => checkEquality(a1, a2, None)}
                 } else false
            }
         // this method is called with simplified arguments are identity has already been checked above, so nothing else to do
         // TODO: also check binders/errors like applications above
         case _ => delay(Equality(con, tm1, tm2, Some(tp)))
      }
      //TODO: expand definitions if necessary to make heads equal
   }
   //auxiliary function of checkEquality; called twice to handle symmetry
   private def tryToSolve(tm1: Term, tm2: Term)(implicit context: Context): Boolean = {
      tm1 match {
         //foundation-independent
         case OMV(m) if unknowns.isDeclared(m) && tm2.freeVars.isEmpty => //forall {v => context.isDeclaredBefore(v,m)}
            solve(m, tm2)
         //foundation-dependent solving rules
         case OMA(OMS(h), OMV(m) :: args) if unknowns.isDeclared(m) && ! (tm2 :: args).exists(_.freeVars contains m) =>
            ruleStore.solutionRules.get(h) match {
               case None => false
               case Some(rule) => rule(this)(m, args, tm2)
            }
         case _ => false
      }
   }

   private def limitedSimplify[A](tm: Term)(simple: Term => Option[A])(implicit context: Context): (Term,Option[A]) = {
      simple(tm) match {
         case Some(a) => (tm,Some(a))
         case None => tm.head match {
            case None => (tm, None)
            case Some(h) =>
               ruleStore.computationRules.get(h) match {
                  case None => (tm, None) //TODO test for definition expansion
                  case Some(rule) =>
                     rule(this)(tm) match {
                        case Some(tmS) => limitedSimplify(tmS)(simple)
                        case None => (tm, None) 
                     }
               }
         }
      }
   }
   def simplify(tm: Term)(implicit context: Context): Term = tm.head match {
      case None => tm
      case Some(h) => ruleStore.computationRules.get(h) match {
         case None => tm
         case Some(rule) =>
            rule(this)(tm) match {
               case Some(tmS) => simplify(tmS)
               case None => tm
            }
      }
   }
   def simplifyCon(context: Context) = context mapTerms {case (con, t) => simplify(t)(context ++ con)}
}

