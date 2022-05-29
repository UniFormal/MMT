package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._

/** super class of all rules used by simplification */
trait SimplifierRule extends SyntaxDrivenRule

/** rules for simplifying expressions */
abstract class MatchingSimplificationRule(val head: GlobalName) extends SimplifierRule {
  def apply(context: Context, rules: RuleSet, t: Term): Simplifiability
}

/**
  * Special case for rules that do not need to know about other rules.
  *
  * @param h head of applicable terms
  */
abstract class SimplificationRule(h: GlobalName) extends MatchingSimplificationRule(h) {
  def apply(context: Context, rules: RuleSet, t: Term): Simplifiability = apply(context, t)

  def apply(context: Context, t: Term): Simplifiability

  /** override and set to true for rules that should only be used for complification */
  def complificationOnly = false
}

/** return type of applying a simplification rule to a term t */
sealed abstract class Simplifiability {
  def get: Option[Term]
}

/** simplify t to result */
case class Simplify(result: Term) extends Simplifiability {
  def get = Some(result)
}

/** this rule cannot be applied to t at toplevel */
sealed abstract class CannotSimplify extends Simplifiability {
  def get = None

  /** a bounded semi-lattice, ordered by uncertainty about stability; least/neutral element: NoRecurse, greatest/attractive element Recurse */
  def join(that: CannotSimplify): CannotSimplify = (this, that) match {
    case (Recurse, _) | (_, Recurse) => Recurse
    case (RecurseOnly(p1), RecurseOnly(p2)) => RecurseOnly(p1 ::: p2)
  }

  /** get all positions, up to top */
  def getPositions(top: Int): Seq[Int]
}

/** this rule cannot become applicable unless a subterm in one of the given positions is simplified; the first argument has position 1 */
case class RecurseOnly(positions: List[Int]) extends CannotSimplify {
  def getPositions(top: Int) = positions.distinct
}

/** this rule might become applicable if any subterm is simplified
  *
  * this should be returned by default; it replaces the return value "None" from the previous ComputationRule.apply method that returned Option[Term]
  */
case object Recurse extends CannotSimplify {
  def getPositions(top: Int) = 1 to top
}


object Simplifiability {
  /** this rule cannot become applicable no matter what and how subterms are simplified */
  val NoRecurse = RecurseOnly(Nil)
}

/** An AbbrevRule expands a symbol into a term */
class AbbrevRule(h: GlobalName, val term: Term) extends SimplificationRule(h) {
  def apply(context: Context, t: Term) =
    if (t == OMS(h)) Simplify(term) else Simplifiability.NoRecurse
}