package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._
import scala.collection.mutable.HashSet

/** The return type of a simplification rule
 * @see DepthRule
 * @see BreadthRule)
 */
abstract class Change

/** A LocalChange leaves the structure of a term unchanged, but changes its arguments
 * @param inside the new argument list
 * For the precise meaning of this Change, see DepthRule and BreadthRule.
 */
case class LocalChange(inside: List[Term]) extends Change

/** A GlobalChange changes the structure of a term.
 * @param it the new term
 */
case class GlobalChange(it: Term) extends Change
/** no change */
case object NoChange extends Change

/** a type to unify BreadRule's and DepthRule's */
sealed abstract class Rule

/** A DepthRule looks one level deep into the structure of one of the arguments of an operator
 * 
 * It is applicable to a term of the form
 * {{{
 * OMA(outer, before ::: OMA(inner, inside) :: after)
 * }}}
 * A LocalChange replaces OMA(inner,inside).
 */
abstract class DepthRule(val outer: GlobalName, val inner: GlobalName) extends Rule {
   /** a Rewrite takes the triple (before, inside, after) and returns a simplification Result */
   type Rewrite = (List[Term], List[Term], List[Term]) => Change
   /** the implementation of the simplification rule */
   def apply : Rewrite
}

/** A DepthRuleUnary is the special case of a DepthRule where the outer operator is unary */
abstract class DepthRuleUnary(outer: GlobalName, inner: GlobalName) extends DepthRule(outer, inner) {
   /** a RewriteUnary takes only "inside" as an argument because "before" and "after" are empty */
   type RewriteUnary = List[Term] => Change
   /** the implementation of the simplification rule */
   val applyUnary : RewriteUnary
   val apply : Rewrite = (before, inside, after) => applyUnary(inside)
}

/** A BreadthRule looks at all arguments of an operator, but usually does not look inside them
 * 
 * It is applicable to a term of the form
 * {{{
 * OMA(op, args)
 * }}}
 * A LocalChange replaces args.
 */ 
abstract class BreadthRule(val op: GlobalName) extends Rule {
   /** a Rewrite takes the arguments args and returns a simplification Result */
   type Rewrite = List[Term] => Change
   /** the implementation of the simplification rule */
   val apply: Rewrite
}

/** A RuleSet groups some Rule's. Its construction and use corresponds to algebraic theories. */
abstract class RuleSet {
   private val rules = new HashSet[Rule]

   def declares(rs: Rule*) {rs foreach {rules += _}}
   def imports(rss: RuleSet*) {rss foreach {rules ++= _.rules}}

   def allRules = rules
   def depthRules = rules filter {_.isInstanceOf[DepthRule]}
   def breadthRules = rules filter {_.isInstanceOf[BreadthRule]}
}
