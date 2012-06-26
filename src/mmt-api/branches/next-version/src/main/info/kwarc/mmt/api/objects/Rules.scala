package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import objects.Conversions._
import scala.collection.mutable.{HashMap}

/** A RuleStore maintains sets of foundation-dependent rules. */
class RuleStore {
   val typingRules = new HashMap[ContentPath,TypingRule]
   val inferenceRules = new HashMap[ContentPath, InferenceRule]
   val computationRules = new HashMap[ContentPath, ComputationRule]
   val equalityRules = new HashMap[ContentPath, EqualityRule]
   val equalityRulesByHead = new HashMap[ContentPath, EqualityRule]
   val solutionRules = new HashMap[ContentPath, SolutionRule]
   
   def add(rs: Rule*) {
      rs foreach {
         case r: TypingRule => typingRules(r.head) = r
         case r: InferenceRule => inferenceRules(r.head) = r
         case r: ComputationRule => computationRules(r.head) = r
         case r: EqualityRule => equalityRules(r.head) = r
         case r: SolutionRule => solutionRules(r.head) = r
      }
   }
}

/** Unifies all Rules */ 
trait Rule {
   val head: GlobalName
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
   def apply(solver: Solver)(tm: Term, tp: Term)(implicit context: Context): Boolean
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
   def apply(solver: Solver)(tm: Term)(implicit context: Context): Option[Term]
}

/** A ComputationRule simplifies an expression operating at the toplevel of the term.
 *  But it may recursively simplify the components if that makes the rule applicable.
 *  @param head the head of the term this rule can simplify 
 */
abstract class ComputationRule(val head: GlobalName) extends Rule {
   /** 
    *  @param solver provides callbacks to the currently solved system of judgments
    *  @param tm the term to simplify
    *  @param context its context
    *  @return the simplified term if simplification was possible
    */
   def apply(solver: Solver)(tm: Term)(implicit context: Context): Option[Term]
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
   def apply(solver: Solver)(tm1: Term, tm2: Term, tp: Term)(implicit context: Context): Boolean
}

/** A SolutionRule solves for an unknown that occurs in a non-solved position.
 */
abstract class SolutionRule(val head: GlobalName) extends Rule {
   /** 
    *  @return true iff it solved a variable
    */
   def apply(solver: Solver)(unknown: LocalName, args: List[Term], tm2: Term)(implicit context: Context): Boolean
}