package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import libraries._
import objects._
import objects.Conversions._

class Continue[A](a: => A) {
   def apply() = a
}

object Continue {
   def apply[A](a : => A) = new Continue(a)
}


/**
 * passed to [[Rule]]s to permit callbacks to the Solver
 */
trait CheckingCallback {
   //def getType(p: GlobalName): Option[Term]
   //def getDef(p: GlobalName): Option[Term]
   /** checking */
   def check(j: Judgement)(implicit history: History): Boolean
   /** possibly unsafe simplification */
   def simplify(t : Term)(implicit stack: Stack, history: History): Term
   /** type inference, fails by default */
   def inferType(t : Term, covered: Boolean = false)(implicit stack: Stack, history: History): Option[Term] = None
   /** @return MightFail by default */
   def dryRun[A](code: => A): DryRunResult = MightFail
}

/**
  * rules can throw this exception after being called if they are not applicable
  * because actual back-tracking is not implemented yet, they may only do so
  * if they have not recursed into any state-changing judgments yet
  */
trait MaytriggerBacktrack {
   case class Backtrack() extends Exception("Not Applicable")
}

/** super class of all rules primarily used by the [[Solver]] */
trait CheckingRule extends SyntaxDrivenRule

object TypingRule {
   /**
    * may be thrown by a TypingRule to indicate that type of tm should be inferred and equated to tp 
    */
   object SwitchToInference extends java.lang.Throwable
}

/** An TypingRule checks a term against a type.
 *  It may recursively call other checks.
 *  @param head the head of the type to which this rule is applicable 
 */
abstract class TypingRule(val head: GlobalName) extends CheckingRule {
   /**
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm the term
    *  @param tp the type
    *  @param stack its context
    *  @return true iff the typing judgment holds
    *  
    *  may throw SwitchToInference
    */
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History) : Boolean
}

/**
 * A SubtypingRule handles [[Subtyping]] judgements
 */
abstract class SubtypingRule extends CheckingRule with MaytriggerBacktrack {
   def applicable(tp1: Term, tp2: Term): Boolean
   /**
    * pre all arguments covered
    * @return Some(b) if the judgment was proved/disproved, None if the result is inconclusive
    */
   def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History) : Option[Boolean]
}

sealed abstract class Variance
case object Covariant extends Variance
case object Contravariant extends Variance
case object Invariant extends Variance
case object Ignorevariant extends Variance

/** |- op(args1) <: op(args2)  according to variances */
class VarianceRule(val head: GlobalName, variance: List[Variance]) extends SubtypingRule {
  def applicable(tp1: Term, tp2: Term) = (tp1,tp2) match {
    case (OMA(OMS(this.head), _), OMA(OMS(this.head), _)) => true
    case _ => false
  }
  def apply(solver: Solver)(tp1: Term, tp2: Term)(implicit stack: Stack, history: History): Option[Boolean] = {
    val OMA(_, args1) = tp1
    val OMA(_, args2) = tp2
    if (args1.length != args2.length)
      return Some(false)
    if (args1.length != variance.length) {
      return Some(false) // TODO what to do here?
    }
    history += "checking subtyping by applying variance rule"
    val b = ((args1 zip args2) zip variance) forall {case ((a1,a2),v) =>
        v match {
          case Covariant => solver.check(Subtyping(stack, a1, a2))
          case Contravariant => solver.check(Subtyping(stack, a2, a1))
          case Invariant => solver.check(Equality(stack, a1, a2, None))
          case Ignorevariant => true
        }
    }
    Some(b)
  }
}

/** A SupertypeRule represnts a type as a subtype of a bigger one
  *  @param head the head of the term whose type this rule infers
  */
abstract class SupertypeRule(val head: GlobalName) extends CheckingRule {
   /**
     *  @param solver provides callbacks to the currently solved system of judgments
     *  @param tp the type
     *  @param covered if true, well-formedness may be assumed
     *  @param stack its context
     *  @param history the history so far
     *  @return the supertype and the condition defining the original type as a subtype
     */
   def apply(solver: Solver)(tp: Term, covered: Boolean)(implicit stack: Stack, history: History): (Option[(Boolean,Term)],Option[Term])
}


/** An InferenceRule infers the type of an expression
 *  It may recursively infer the types of components.
 *  @param head the head of the term whose type this rule infers 
 */
abstract class InferenceRule(val head: GlobalName, val typOp : GlobalName) extends CheckingRule with MaytriggerBacktrack {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm the term
    *  @param covered if true, well-formedness may be assumed
    *  @param stack its context
    *  @param history the history so far
    *  @return the inferred type if inference was possible
    */
   def apply(solver: Solver)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term]
}

abstract class FormationRule(val headx: GlobalName, val typOpx : GlobalName) extends InferenceRule(headx,typOpx)
abstract class IntroductionRule(val headx: GlobalName, val typOpx : GlobalName) extends InferenceRule(headx,typOpx)
abstract class EliminationRule(val headx: GlobalName, val typOpx : GlobalName) extends InferenceRule(headx,typOpx)

/** A ComputationRule simplifies an expression operating at the toplevel of the term.
 *  But it may recursively simplify the components if that makes the rule applicable.
 *  The rule must preserve equality and well-typedness of the simplified term. If necessary, additional checks must be performed.
 *  @param head the head of the term this rule can simplify 
 */
abstract class ComputationRule(val head: GlobalName) extends CheckingRule {
   /** 
    *  @param check provides callbacks to the currently solved system of judgments
    *  @param tm the term to simplify
    *  @param covered if true, term can be assumed to be well-formed
    *  @param stack its context
    *  @return the simplified term if simplification was possible
    */
   def apply(check: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Option[Term]
}

/** A UnaryTermRule checks a [[UnaryTermJudgement]]
 *  @param head the head of the term 
 */
abstract class UnaryTermRule(val head: GlobalName) extends CheckingRule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param term the Term
    *  @param stack its context
    *  @return true iff the judgment holds
    */
   def apply(solver: Solver)(term: Term)(implicit stack: Stack, history: History): Boolean
}
/** checks an [[Inhabitable]] judgement */
abstract class InhabitableRule(head: GlobalName) extends UnaryTermRule(head)
/** checks a [[Universe]] judgement */
abstract class UniverseRule(head: GlobalName) extends UnaryTermRule(head)

trait ApplicableUnder extends SyntaxDrivenRule {
   def under: List[GlobalName]
   private lazy val ops = (under:::List(head)).map(p => OMS(p))
   def applicable(tm: Term) = tm match {
      case OMA(f,a) => (f::a).startsWith(ops)
      case OMS(p) => under == Nil && p == head
      case _ => false
   }
}

/** A TypeBasedEqualityRule checks the equality of two terms based on the head of their type
 *  @param head the head of the type of the two terms 
 */
abstract class TypeBasedEqualityRule(val under: List[GlobalName], val head: GlobalName) extends CheckingRule with ApplicableUnder {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm1 the first term
    *  @param tm2 the second term
    *  @param tp their type
    *  @param stack their context
    *  @return true iff the judgment holds
    */
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack, history: History): Option[Boolean]
}

/**
 * A TermBasedEqualityRule checks the equality of two terms without considering types
 */
abstract class TermBasedEqualityRule extends CheckingRule {
   /** 
    *  @return true if the rule is applicable to tm1 == tm2
    *  
    *  This method should fail quickly if possible; the apply method may still fail even it this returned true. 
    */
   def applicable(tm1: Term, tm2: Term): Boolean
   /**
    *  @param check provides callbacks to the currently solved system of judgments
    *  @param tm1 the first term
    *  @param tm2 the second term
    *  @param tp their type if known
    *  @param stack their context
    *  @param history the history so far
    *  @return Some(cont) continuation function to apply the rule
    *          None if this rule is not applicable
    */
   def apply(check: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History): Option[Continue[Boolean]]
}


/** A TermBasedEqualityRule checks the equality of two terms with certain heads
 *  @param left the head of the first term
 *  @param right the head of the second term 
 */
abstract class TermHeadBasedEqualityRule(val under: List[GlobalName], val left: GlobalName, val right: GlobalName) extends TermBasedEqualityRule {
   val head = left
   private val opsLeft  = (under:::List(left)).map(p => OMS(p))
   private val opsRight = (under:::List(right)).map(p => OMS(p))
   def applicable(tm1: Term, tm2: Term) = (tm1,tm2) match {
      case (OMA(f1,a1),OMA(f2,a2)) => (f1::a1).startsWith(opsLeft) && (f2::a2).startsWith(opsRight)
      case (ComplexTerm(c1,_,_,_), ComplexTerm(c2,_,_,_)) => under.isEmpty && c1 == left && c2 == right 
      case _ => false
   }
   def apply(check: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History): Option[Continue[Boolean]]
}

/** Congruence as a TermBasedEqualityRule for two terms with the same head
 *  @param head the head of the terms
 *  This rule can be added whenever a constructor is known to be injective,
 *  which is typically the case for type formation and term introduction.
 */
class CongruenceRule(head: GlobalName) extends TermHeadBasedEqualityRule(Nil, head, head) {
   def apply(checker: CheckingCallback)(tm1: Term, tm2: Term, tp: Option[Term])(implicit stack: Stack, history: History) = {
      (tm1,tm2) match {
         case (ComplexTerm(this.head, args1, cont1, scps1), ComplexTerm(this.head, args2, cont2, scps2)) =>
            if (args1.length == args2.length && cont1.length == cont2.length && scps1.length == scps2.length) {
               val cont = Continue {
                  val args = (args1 zip args2) forall {case (Sub(l1,a1),Sub(l2,a2)) =>
                     l1 == l2 && checker.check(Equality(stack,a1,a2,None))}
                  val argsCont = args && checker.check(EqualityContext(stack, cont1, cont2))
                  val alpha = (cont2 alpha cont1).get // defined because cont1.length == cont2.length
                  val argsContScps = argsCont && (scps1 zip scps2).forall {case (s1,s2) =>
                     checker.check(Equality(stack ++ cont1, s1, s2 ^ alpha, None))
                  }
                  argsContScps
               }
               Some(cont)
            } else
               None
         case _ => None
      }
   }
}

/** A ForwardSolutionRule solves for an unknown by inspecting its declarations (as opposed to its use)
 * It can solve a variable directly (e.g., if it has unit type) or apply a variable transformation (e.g., X --> (X1,X2) if X has product type).
 * @param head the head of the type of the unknown to which this rule applies
 * @param priority rules with high priority are applied to a freshly activated constraint is activated;
 *   others when no activatable constraint exists 
 */
abstract class ForwardSolutionRule(val head: GlobalName, val priority: ForwardSolutionRule.Priority) extends CheckingRule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param decl the declaration of an unknown
    *  @return true iff it solved a variable
    */
   def apply(solver: Solver)(decl: VarDecl)(implicit stack: Stack, history: History): Boolean
}

/** auxiliary object for the class ForwardSolutionRule */
object ForwardSolutionRule {
   /** the two-valued type of priorities */
   type Priority = Boolean
   /** high priority */
   val high = true
   /** low priority */
   val log = false
}

/**
 * A SolutionRule tries to solve for an unknown that occurs in an equality judgement.
 * 
 * It may be partial by, e.g., by inverting the toplevel operation of a Term without completely isolating an unknown occurring in it.
 * 
 * f(t1) = t2   --->   t1 = g(t2), where t1 contains a target variable that we try to isolate
 * 
 * @param head the operator that the rule tries to invert
 * 
 * Because solution rules must be tried often and may fail, they do not have access to the Solver state
 * and instead transform one judgement into another.
 * This also allows reusing them in other situations, in particular when matching already-type-checked terms.
 */
abstract class SolutionRule(val head: GlobalName) extends CheckingRule {
   /**
    * @return Some(i) if the rule is applicable to t1 in the judgment t1=t2,
    *   in that case, i is the position of the argument of t1 (starting from 0) that the rule will try to isolate
    */
   def applicable(t: Term) : Option[Int]
   /**
    *  @param j the equality in which to isolate a variable on the left side 
    *  @return the transformed equality and a log message if a step towards isolation was possible
    */
   def apply(j: Equality): Option[(Equality,String)]
}

/** A TypeSolutionRule tries to solve for an unknown that occurs in a non-solved position.
 * It may also be partial, e.g., by inverting the toplevel operation of a Term without completely isolating an unknown occurring in it.
 */
abstract class TypeSolutionRule(val head: GlobalName) extends CheckingRule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm the term that contains the unknown to be solved
    *  @param tp its type 
    *  @param stack the context
    *  @return false if this rule is not applicable;
    *    if this rule is applicable, it may return true only if the Typing Judgement is guaranteed
    *    (by calling an appropriate callback method such as delay or checkTyping)
    */
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack, history: History): Boolean
}