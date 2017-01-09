package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._

/** The return type of a simplification rule
 * @see DepthRule
 * @see BreadthRule)
 */
abstract class Change {
   /** corresponds to Option.orelse */
   def orelse(that: => Change): Change = if (this == NoChange) that else this
}

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

object UOMRule {
  type Phase = Int
  val Simplify = 1
  val Complify = -1
}

/** super class of all rules used by the [[UOM]] */
trait UOMRule extends SyntaxDrivenRule {
  def phase: UOMRule.Phase = UOMRule.Simplify
}

/** an arbitrary rewrite/computation/simplification rule */
// TDOO this should become part of UOMRule, breadth/depth rules should be instances
trait TermTransformationRule extends UOMRule {
  /** try to apply this return, result result if applicable */
  def apply(matcher: Matcher, goalContext: Context, goal: Term): Option[Term]
}

/** a rule that should only be used during complification
 *  
 *  separating these rules is important to avoid cycles during simplification
 */
trait ComplificationRule extends TermTransformationRule {
  override def phase = UOMRule.Complify
}

/** A DepthRule looks one level deep into the structure of one of the arguments of an operator
 * 
 * It is applicable to a term of the form
 * {{{
 * OMA(outer, before ::: OMA(inner, inside) :: after)
 * }}}
 * A LocalChange replaces OMA(inner,inside).
 */
abstract class DepthRule(val outer: GlobalName, val inner: GlobalName) extends UOMRule {
   val head = outer
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
abstract class BreadthRule(val head: GlobalName) extends UOMRule {
   /** a Rewrite takes the arguments args and returns a simplification Result */
   type Rewrite = List[Term] => Change
   /** the implementation of the simplification rule */
   val apply: Rewrite
}

/** An AbbrevRule expands a symbol into a term */ 
class AbbrevRule(val head: GlobalName, val term: Term) extends UOMRule {
  object complifyRule extends RewriteRule(???, Context.empty, term, OMS(head)) with ComplificationRule
  override def getRules = complifyRule :: super.getRules
}

/** a general rewrite rule
 *  @param templateVars the free variables to fill in through matching
 *  @param template the left-hand side
 *  @param result the right-hand side
 */
class RewriteRule(ops: List[GlobalName], templateVars: Context, template: Term, val result: Term) extends TermTransformationRule {
  val head = ops.last
  def apply(matcher: Matcher, goalContext: Context, goal: Term) = {
    matcher(goalContext, goal, templateVars, template) match {
      case MatchSuccess(sub) => Some(result ^? sub)
      case _ => None
    }
  }
}