package info.kwarc.mmt.uom

import info.kwarc.mmt.api._
import objects._

/** The return type of a simplification rule
 * @see DepthRule
 * @see BreadthRule)
 */
abstract class Change
/** A LocalChange leaves the structure of a term unchanged, but changes its arguments
 * @param inside the new argument list
 */
case class LocalChange(inside: List[Term]) extends Change
/** A GlobalChange changes the structure of a term.
 * @param it the new term
 */
case class GlobalChange(it: Term) extends Change
/** no change */
case object NoChange extends Change

/** A DepthRule looks one level deep into the structure of one of the arguments of an operator
 * 
 * It is applicable to a term of the form
 * {{{
 * OMA(outer, before ::: OMA(inner, inside) :: after)
 * }}}
 */
abstract class DepthRule(outer: GlobalName, inner: GlobalName) {
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
 */ 
abstract class BreadthRule(op: GlobalName) {
   /** a Rewrite takes the arguments args and returns a simplification Result */
   type Rewrite = List[Term] => Change
   /** the implementation of the simplification rule */
   val apply: Rewrite
}