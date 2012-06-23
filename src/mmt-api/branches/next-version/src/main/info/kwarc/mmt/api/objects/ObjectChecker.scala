package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
//import info.kwarc.mmt.api.objects._
import libraries._
import modules._
import symbols._
//import utils._
import frontend._
import objects.Conversions._
import scala.collection.mutable.HashSet

/** the type of object level judgments as used for typing and equality of terms */
abstract class Judgement {
  /** @return the set of names of the meta-variables occurring in this judgment
   *    Constraints must come with a Context binding all object variables occurring freely in any expressions;
   *    therefore, the remaining free variables are meta-variables
   */ 
  def freeVars : HashSet[LocalName]
}

/** represents an equality judegment, optionally at a type
 * context |- t1 = t2 : tp  if t = Some(tp)
 * or
 * context |- t1 = t2       if t = None
 */
case class Equality(context: Context, t1: Term, t2: Term, t: Option[Term]) extends Judgement {
   lazy val freeVars = {
     val ret = new HashSet[LocalName]
     val fvs = context.freeVars_ ::: t1.freeVars_ ::: t2.freeVars_ ::: (t.map(_.freeVars_).getOrElse(Nil))
     fvs foreach {n => if (! context.isDeclared(n)) ret += n}
     ret
   }
}

/** represents a typing judgement
 * context |- tm : tp
 */
case class Typing(context: Context, tm: Term, tp: Term) extends Judgement {
  lazy val freeVars = {
    val ret = new HashSet[LocalName]
    val fvs = context.freeVars_ ::: tm.freeVars_ ::: tp.freeVars_
    fvs foreach {n => if (! context.isDeclared(n)) ret += n}
    ret
  }
}

//TODO case class InhabitationConstraint(name: LocalName, tp: Term) extends Constraint

/** A wrapper around a Judgement to maintain meta-information while that Constraint is delayed */
class DelayedConstraint(val constraint: Judgement) {
  private val freeVars = constraint.freeVars
  private var activatable = false
  /** This must be called whenever a variable that may occur free in this constraint has been solved */
  def solved(name: LocalName) {
     if (! activatable && (freeVars contains name)) activatable = true
  }
  /** @return true iff a variable has been solved that occurs free in this Constraint */
  def isActivatable: Boolean = activatable
}

/*
abstract class TypeConstructor {
   def extensionalityRule(t: Term) : Term
   def extensionalityRule(eq: EqualConstraint) : Option[List[EqualConstraint]]
   def introSymbol: Path
   def computationRule(t: Term): Option[Term]
}

class Unifier {
   val typeCons : List[TypeConstructor]
   def unify(con: Context, t1: Term, t2: Term, t: Term) {
      normal MMT equality check
      if t1 and t2 have the same shape and the same head, and the head is known to be an introSymbol (and thus injective) recurse into components
      otherwise, try extend
   }
   def extend(con: Context, t1: Term, t2: Term, t: Term) {
      (typeCons find {tc => (tc.typeSymbol == t.head)}) match {
        case Some(tc) => unify (... tc.extensionalityRule(...) ...)
        case None =>
           compute(con, t1, t2)
      }
   }
   def compute(con: Context, t1: Term, t2: Term) {
      val t1C = typeCons findMap {tc => tc.computationRule(t1)}
      val t2C = typeCons findMap {tc => tc.computationRule(t1)}
      unify(..)
   }
}
*/

/**
 * A Solver is used to solve a system of judgments about Term's, given by Constraint's,
 * by applying typing rules to validate the judgment.
 * The judgments may contain unknown variables (also called meta-variables or logic variables);
 * the solution is a Substitution that provides a closed Term for every unknown variable.
 * A new instance must be created for every system of judgments. 
 * @param controller used for looking up Foundation's and Constant's. No changes are made to controller.
 * @param unknowns the list of all unknown variables with their types in dependency order;
 *   variables may represent any MMT term, i.e., object language terms, types, etc.
 *   unknown variables may occur in the types of later unknowns
 */
class Solver(controller: Controller, unknowns: Context) {
   /** tracks the solution, like unknowns but a definiens is added for every solved variable */ 
   private var solution : Context = unknowns
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

   /** retrieve the Foundation providing the semantics of a symbol, if any */
   private def getFoundation(p: MPath): Option[Foundation] = controller.extman.getFoundation(p)
   
   /** delays a constraint for future processing */
   private def delay(c: Judgement): Boolean = {
      val dc = new DelayedConstraint(c)
      delayed = dc :: delayed
      true
   }
   /** activates a previously delayed constraint if one of its free variables has been solved since */
   private def activate: Boolean = {
      delayed find {_.isActivatable} match {
         case None => true
         case Some(dc) => apply(dc.constraint)
      }
   }
   /** registers the solution for a variable; notifies all delayed constraints */
   //TODO solutions should also be propagated to currently active constraints
   private def solve(name: LocalName, value: Term): Boolean = {
      val (left, solved :: right) = solution.span(_.name != name)
      if (solved.df.isDefined)
         checkEquality(value, solved.df.get, solved.tp)(Context())
      else {
         solution = left ::: solved.copy(df = Some(value)) :: right
         delayed foreach {_.solved(name)}
         true
      }
   }
   /** applies this Solver to one constraint
    *  this method can be called multiple times to solve a system of constraints
    *  @param c the constraint
    *  @return false only if the constraints are unsatisfiable; true if constraints have been resolved or delayed  
    */
   def apply(j: Judgement): Boolean = {
     val subs = solution.toPartialSubstitution
     j match {
        case Typing(con, tm, tp) =>
           checkTyping(tm ^ subs, tp ^ subs)(con ^ subs)
        case Equality(con, tm1, tm2, tp) =>
           checkEquality(tm1 ^ subs, tm2 ^ subs, tp map {_ ^ subs})(con ^ subs)
     }
     activate
   }
   /** proves a TypingConstraint by recursively applying rules and solving variables where applicable,
    *  delays a constraint if unsolved variables preclude further processing
    *  checkTyping(tm, tp)(con) solves the judgment con |- tm : tp
    *  @return false only if the judgment does not hold; true if it holds or constraint have been delayed
    */
   private def checkTyping(tm: Term, tp: Term)(implicit con: Context): Boolean = {
      true
   }
   /** handles an EqualityConstraint by recursively applying rules and solving variables where applicable,
    *  delays a constraint if unsolved variables preclude further processing
    *  @param tpOpt if empty, tm1 and tm2 may be ill-typed; if non-empty, they must also type-check at that type
    *  @return false only if the judgment does not hold; true if it holds or constraint have been delayed
    */
   private def checkEquality(tm1: Term, tm2: Term, tpOpt: Option[Term])(implicit con: Context): Boolean = {
      // the common case of identical terms
      if (tm1 == tm2) tpOpt match {
         case None => true
         case Some(tp) => checkTyping(tm1, tp)
      } else (tm1, tm2) match {
         // if no case applied, try the remaining cases
         case _ => checkEquality2(tm1, tm2, tpOpt, true)
      }
   }
   /** like checkEquality, but contains all those cases that are tried twice:
    *  if no case applies, we flip tm1 and tm2 and try again; if still no case applies, we delay
    * @param firstTime true if called for the first time, false if called for the second time
    */
   private def checkEquality2(tm1: Term, tm2: Term, tpOpt: Option[Term], firstTime: Boolean)(implicit con: Context): Boolean = {
      (tm1, tm2) match {
         // |- x = t: solve x as t
         case (OMV(x), t) =>
            if (unknowns.isDeclared(x)) {
               if (! t.freeVars.isEmpty) {
                  solve(x, t)
                  //check x.tp=tp? or t:x.tp?
               } else {
                 delay(Equality(con, tm1, tm2, tpOpt))
               }
            } else {
               delay(Equality(con, tm1, tm2, tpOpt))
            }
         case _ =>
            // if no case applied, ...
            if (firstTime)
               // flip the arguments and try again, or ...
               checkEquality2(tm2, tm1, tpOpt, false)
            else
               // delay if we've tried that already
               delay(Equality(con, tm2, tm1, tpOpt))
      }
   }
}