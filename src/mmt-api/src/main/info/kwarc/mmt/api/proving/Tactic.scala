package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import checking._
import objects._

/**
 * A rule used for forward or backward proof search
 */
trait Tactic extends SyntaxDrivenRule {
   /**
    * convenience function to create an ApplicableTactic
    *
    * to be used as in
    *
    * {{{
    * def apply(...) = {
    *    // code for checking applicability
    *    onApply {
    *       // code to run when applying the rule
    *    }
    * }
    * }}}
    */
   protected def onApply(l: => String)(cont: => Alternative) = {
      val at = new ApplicableTactic {
         def apply() = Some(cont)
         def label = l
      }
      Some(at)
   }
}

/**
 * invertible tactics can be applied greedily without leading a proof into a dead end
 *
 * this type includes both invertible forward (e.g., existential elimination) and backwards rules (e.g., universal introduction)
 */
trait InvertibleTactic extends Tactic {
   /**
    *  applies the tactic to a goal
    *
    *  @param prover the calling prover, used for callbacks
    *  @param g the goal
    *  @return a continuation that returns the new goal(s)
    */
   def apply(prover: Searcher, g: Goal): Option[ApplicableTactic]
}

/**
 * an InvertibleTactic, whose behavior depends only on the conclusion of a goal
 */
trait BackwardInvertible extends InvertibleTactic {
   def apply(prover: Searcher, g: Goal): Option[ApplicableTactic]
}

/**
 * an InvertibleTactic, whose behavior depends only on the context of a goal
 */
trait ForwardInvertible extends InvertibleTactic {
   def apply(prover: Searcher, context: Context): Option[ApplicableTactic]
   def apply(prover: Searcher, g: Goal): Option[ApplicableTactic] = apply(prover, g.context)
}

/**
 * a backward tactic generates additional ways to reach the goal
 *
 * This is used in backward proof search
 */
trait BackwardSearch extends Tactic {
   /** applies the tactic to a goal
    *
    *  @param prover the calling prover, used for callbacks
    *  @param g the goal
    *  @return a list of continuations each of which might solve the goal
    */
   def apply(prover: Searcher, g: Goal): List[ApplicableTactic]
}

/**
 * a forward tactic generates additional facts that are implied by the assumptions
 *
 * This is used in forward proof search
 */
trait ForwardSearch extends Tactic {
   /**
    * enriches a database of facts by one iteration
    */
   def generate(prover: Searcher, interactive: Boolean): Unit
}

/**
 * a continuation function returned by a [[Tactic]] to be run if the tactic is to be applied
 *
 * A tactic may return multiple continuations if it is applicable in multiple different ways.
 * Low-priority tactics may move expensive computations into the continuation to avoid unnecessary work
 */
abstract class ApplicableTactic {
   def label : String
   /** runs the continuation
    *  @return the new goals, None if application was not possible
    */
   def apply() : Option[Alternative]
}

object ApplicableTactic {
   def apply(a: Alternative, l: String) = new ApplicableTactic {
      def apply() = Some(a)
      val label = l
   }
}
