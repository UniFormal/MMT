package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import libraries._
import objects.Conversions._
import scala.collection.mutable.{HashMap}

/** A RuleStore maintains sets of foundation-dependent rules that are used by a Solver.
 * 
 *  It maintains a separate set of TypingRule's for every subtype of Rule; the Rule's are indexed by their heads.
 *  An instance of RuleStore is created by the ExtensionManager.
 */
class RuleStore {
   val typingRules = new HashMap[ContentPath,TypingRule]
   val inferenceRules = new HashMap[ContentPath, InferenceRule]
   val computationRules = new HashMap[ContentPath, ComputationRule]
   val universeRules = new HashMap[ContentPath, UniverseRule]
   val equalityRules = new HashMap[ContentPath, EqualityRule]
   val atomicEqualityRules = new HashMap[ContentPath, AtomicEqualityRule]
   val solutionRules = new HashMap[ContentPath, SolutionRule]
   val forwardSolutionRules = new HashMap[ContentPath, ForwardSolutionRule]
   
   /** add some Rule to this RuleStore */
   def add(rs: Rule*) {
      rs foreach {
         case r: TypingRule => typingRules(r.head) = r
         case r: InferenceRule => inferenceRules(r.head) = r
         case r: ComputationRule => computationRules(r.head) = r
         case r: UniverseRule => universeRules(r.head) = r
         case r: EqualityRule => equalityRules(r.head) = r
         case r: AtomicEqualityRule => atomicEqualityRules(r.head) = r
         case r: SolutionRule => solutionRules(r.head) = r
         case r: ForwardSolutionRule => forwardSolutionRules(r.head) = r
      }
   }
   def add(rs: RuleSet) {
      add(rs.rules : _*)
   }
}

/** A pair (theory, context) used by rules
 * @param theory the theory
 * @param context the context
 */
case class Frame(theory : Term, context : Context) {
   def ^(subs: Substitution) = Frame(theory, context ^ subs)
}

case class Stack(frames: List[Frame]) {
   def pop = Stack(frames.tail)
   def push(f: Frame) = Stack(f::frames)
   def theory = frames.head.theory
   def context = frames.head.context
   /** applies the same substitution to all contexts on this stack
    */
   def ^(subs: Substitution) = Stack(frames.map(_ ^ subs))
   def ++(con: Context) = Stack(Frame(theory, context ++ con) :: frames.tail)
}

object Stack {
   def apply(f: Frame) : Stack = Stack(List(f))
   def apply(t: MPath) : Stack = empty(OMMOD(t))
   def empty(t: Term) : Stack = Stack(Frame(t, Context()))
}

/** the type of all Rules
 * 
 * All Rules have an apply method that is passed a Solver for callbacks.
 * The Solver does not implement any back-tracking. Therefore, rules may only use callbacks if their effects are required.
 */
trait Rule {
   /** an MMT URI that is used to indicate when the Rule is applicable */
   val head: GlobalName
}

/** A RuleSet groups some rules together */
abstract class RuleSet {
   val rules: List[Rule]
}

/** An TypingRule checks a term against a type.
 *  It may recursively call other checks.
 *  @param head the head of the type to which this rule is applicable 
 */
abstract class TypingRule(val head: GlobalName) extends Rule {
   /**
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm the term
    *  @param the type
    *  @param context its context
    *  @return true iff the typing judgment holds
    */
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit stack: Stack) : Boolean
}

/** An InferenceRule infers the type of an expression
 *  It may recursively infer the types of components.
 *  @param head the head of the term whose type this rule infers 
 */
abstract class InferenceRule(val head: GlobalName) extends Rule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm the term
    *  @param context its context
    *  @return the inferred type if inference was possible
    */
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack): Option[Term]
}

/** A ComputationRule simplifies an expression operating at the toplevel of the term.
 *  But it may recursively simplify the components if that makes the rule applicable.
 *  The rule must preserve equality and well-typedness of the simplified term. If necessary, additional checks must be performed.
 *  @param head the head of the term this rule can simplify 
 */
abstract class ComputationRule(val head: GlobalName) extends Rule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm the term to simplify
    *  @param context its context
    *  @return the simplified term if simplification was possible
    */
   def apply(solver: Solver)(tm: Term)(implicit stack: Stack): Option[Term]
}

/** A UniverseRule checks if a term is a universe
 *  @param head the head of the universe expression 
 */
abstract class UniverseRule(val head: GlobalName) extends Rule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param univ the Term
    *  @param stack its context
    *  @return true iff the judgment holds
    */
   def apply(solver: Solver)(univ: Term)(implicit stack: Stack): Boolean
}

/** A EqualityRule checks the equality of two expressions
 *  @param head the head of the type of the two expressions 
 */
abstract class EqualityRule(val head: GlobalName) extends Rule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm1 the first term
    *  @param tm2 the second term
    *  @param tp their type
    *  @param context their context
    *  @return true iff the judgment holds
    */
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack): Boolean
}


/** An AtomicEqualityRule is called to handle equality of atomic terms with the same shape at a base type.
 * Same shape means that their TorsoNormalForm has identical torso and heads.
 * @param head the head of the two terms
 */
abstract class AtomicEqualityRule(val head: GlobalName) extends Rule {
  def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit stack: Stack): Boolean
}

/** A ForwardSolutionRule solves for an unknown by inspecting its declarations (as opposed to its use)
 * It can solve a variable directly (e.g., if it has unit type) or apply a variable transformation (e.g., X --> (X1,X2) if X has product type).
 * @param head the head of the type of the unknown to which this rule applies
 * @param priority rules with high priority are applied to a freshly activated constraint is activated;
 *   others when no activatable constraint exists 
 */
abstract class ForwardSolutionRule(val head: GlobalName, val priority: ForwardSolutionRule.Priority) extends Rule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param decl the declaration of an unknown
    *  @return true iff it solved a variable
    */
   def apply(solver: Solver)(decl: VarDecl)(implicit stack: Stack): Boolean
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

/** A SolutionRule tries to solve for an unknown that occurs in a non-solved position.
 * It may also be partial, e.g., by inverting the toplevel operation of a Term without completely isolating an unknown occurring in it.
 */
abstract class SolutionRule(val head: GlobalName) extends Rule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm1 the term that contains the unknown to be solved
    *  @param tm2 the second term 
    *  @param tpOpt their type, if known
    *  @param context their context
    *  @return false if this rule is not applicable;
    *    if this rule is applicable, it may return true only if the Equality Judgement is guaranteed
    *    (by calling an appropriate callback method such as delay or checkEquality)
    */
   def apply(solver: Solver)(tm1: Term, tm2: Term)(implicit stack: Stack): Boolean
}