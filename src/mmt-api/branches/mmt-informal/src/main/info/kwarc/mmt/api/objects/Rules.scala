package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import libraries._
import objects.Conversions._
import scala.collection.mutable.{HashMap,HashSet}

/** empty class as a type abbreviation */
class RuleMap[R <: Rule] extends HashMap[ContentPath,R]

/** A RuleStore maintains sets of foundation-dependent rules that are used by a Solver.
 * 
 *  It maintains a separate set of TypingRule's for every subtype of Rule; the Rule's are indexed by their heads.
 *  An instance of RuleStore is created by the ExtensionManager.
 */
class RuleStore {
   val typingRules = new RuleMap[TypingRule]
   val inferenceRules = new RuleMap[InferenceRule]
   val computationRules = new RuleMap[ComputationRule]
   val universeRules = new RuleMap[UniverseRule]
   val equalityRules = new RuleMap[EqualityRule]
   val atomicEqualityRules = new RuleMap[AtomicEqualityRule]
   val solutionRules = new RuleMap[SolutionRule]
   val forwardSolutionRules = new RuleMap[ForwardSolutionRule]
   val introProvingRules = new utils.HashMapToSet[ContentPath, IntroProvingRule]
   val elimProvingRules = new utils.HashMapToSet[ContentPath, ElimProvingRule]

   /** the DepthRule's that this UOM will use, hashed by (outer,inner) pairs */
   val depthRules = new utils.HashMapToSet[(GlobalName,GlobalName), uom.DepthRule]
   /** the BreadthRule's that this UOM will use, hashed by (outer,inner) pairs */
   val breadthRules = new utils.HashMapToSet[GlobalName, uom.BreadthRule]
   /** the AbbrevRule's that this UOM will use, hashed by the abbreviating operator */
   val abbrevRules = new utils.HashMapToSet[GlobalName, uom.AbbrevRule]
   
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
         case r: IntroProvingRule => introProvingRules(r.head) += r
         case r: ElimProvingRule => elimProvingRules(r.head) += r
         case r: uom.DepthRule => depthRules((r.outer,r.inner)) += r
         case r: uom.BreadthRule => breadthRules(r.head) += r
         case r: uom.AbbrevRule => abbrevRules(r.head) += r
      }
   }
   def add(rs: RuleSet) {
      rs.rules.foreach(add(_))
   }
   
   def stringDescription = {
      var res = ""
      def mkM[A<:Rule](label: String, map: HashMap[ContentPath,A]) {
         if (! map.isEmpty) {
            res += "  " + label + "\n"
            map.values.foreach {r => res += "    " + r.toString + "\n\n"}
         }
      }
      def mkS[A, B<:Rule](label: String, map: utils.HashMapToSet[A,B]) {
         if (! map.isEmpty) {
            res += "  " + label + "\n"
            map.foreach {e => e._2.foreach {r => res += "    " + r.toString + "\n\n"}}
         }
      }
      mkM("typing rules", typingRules) 
      mkM("inference rules", inferenceRules)
      mkM("computation rules", computationRules)
      mkM("universe rules", universeRules)
      mkM("equality rules", equalityRules)
      mkM("atomic equality rules", atomicEqualityRules)
      mkM("solution rules", solutionRules)
      mkM("forwardSolution rules", forwardSolutionRules)
      mkS("intro proving rules", introProvingRules)
      mkS("elim proving rules", elimProvingRules)
      mkS("depth rules", depthRules)
      mkS("breadth rules", breadthRules)
      mkS("abbrev rules", abbrevRules)
      res
   }
}

/** the type of all Rules
 * 
 * All Rules have an apply method that is passed a Solver for callbacks.
 * The Solver does not implement any back-tracking. Therefore, rules may only use callbacks if their effects are required.
 */
trait Rule {
   /** an MMT URI that is used to indicate when the Rule is applicable */
   val head: GlobalName
   override def toString = String.format("%-60s", head.toPath) + " of " + getClass.toString 
}

/** A RuleSet groups some Rule's. Its construction and use corresponds to algebraic theories. */
trait RuleSet {
   val rules = new HashSet[Rule]

   def declares(rs: Rule*) {rs foreach {rules += _}}
   def imports(rss: RuleSet*) {rss foreach {rules ++= _.rules}}

   def allRules = rules
   def depthRules = rules filter {_.isInstanceOf[uom.DepthRule]}
   def breadthRules = rules filter {_.isInstanceOf[uom.BreadthRule]}
   def abbrevRules = rules filter {_.isInstanceOf[uom.AbbrevRule]}
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
    *  @param stack the context
    *  @return false if this rule is not applicable;
    *    if this rule is applicable, it may return true only if the Equality Judgement is guaranteed
    *    (by calling an appropriate callback method such as delay or checkEquality)
    */
   def apply(solver: Solver)(tm1: Term, tm2: Term)(implicit stack: Stack): Boolean
}

/** A continuation returned by [[info.kwarc.mmt.api.objects.ProvingRule]] */
abstract class ApplicableProvingRule {
  def label: String
  //def ranking: Int
  def apply() : Term
}

/** An IntroProvingRule solves a goal with a certain head.
 */
abstract class IntroProvingRule(val head: GlobalName) extends Rule {
   /** 
    * @param goal the type for which a term is needed
    * @param stack the context
    * @return if applicable, a continuation that applies the rule
    */
   def apply(goal: Term)(implicit stack: Stack): Option[ApplicableProvingRule]
}

/**
 * An ElimProvingRule uses a term of a given type with a given head.
 */
abstract class ElimProvingRule(val head: GlobalName) extends Rule {
   /** 
    * @param evidence the proof of the fact, typically an OMS or OMV
    * @param fact the type representing the judgment to be used (whose type is formed from head)
    * @param goal the type for which a term is needed
    * @param stack the context
    * @return if applicable, a continuation that applies the rule
    */
   def apply(evidence: Term, fact: Term, goal: Term)(implicit stack: Stack): Option[ApplicableProvingRule]
}