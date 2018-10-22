package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._

/** super class of all rules used by simplification */
trait SimplifierRule extends SyntaxDrivenRule

/** rules for simplifying expressions */
abstract class MatchingSimplificationRule(val head: GlobalName) extends SimplifierRule {
  def apply(context: Context, rules: RuleSet, t: Term): Simplifiability
}

/** special case for rules that do not need to know about other rules */
abstract class SimplificationRule(h: GlobalName) extends MatchingSimplificationRule(h) {
  def apply(context: Context, rules: RuleSet, t: Term): Simplifiability = apply(context, t)
  def apply(context: Context, t: Term): Simplifiability
}

/** return type of applying a simplification rule to a term t*/
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
  def join(that: CannotSimplify): CannotSimplify = (this,that) match {
    case (Recurse, _) | (_, Recurse) => Recurse 
    case (RecurseOnly(p1),RecurseOnly(p2)) => RecurseOnly(p1:::p2)
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
 *  this should be returned by default; it replaces the return value "None" from the previous ComputationRule.apply method that returned Option[Term]
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

/**
 * a general rewrite rule
 * 
 * @param templateVars the free variables to fill in through matching
 * @param template the left-hand side
 * @param ths the right-hand side (relative to the template variables)
 * 
 * to allow for using the same rewrite rule in different contexts (where different solution rules for matching may be available),
 * this class must be provided with a Matcher before producing an actual rule 
 */
// TODO this is barely used so far (once in Mizar)
class RewriteRule(val head: GlobalName, templateVars: Context, template: Term, val rhs: Term) extends TermTransformationRule {
  /** 
   * @param matcher the matcher to use
   * @return the simplification rule
   */
  def makeRule(matcher: Matcher) = new SimplificationRule(head) {
    def apply(goalContext: Context, goal: Term) = {
      matcher(goalContext, goal, templateVars, template) match {
        case MatchSuccess(sub) =>
          Simplify(rhs ^? sub)
        case _ =>
          Recurse // TODO this is terrible for stability
      }
    }
  }
  
  def apply(matcher: Matcher, goalContext: Context, goal: Term) = {
    val rule = makeRule(matcher)
    rule(goalContext, goal).get
  }
}

/**
 * general purpose transformation rule
 * //TODO this is barely used and should be merged with SimplificationRule
 */
trait TermTransformationRule extends SimplifierRule {
  /** try to apply this, return result if applicable */
  def apply(matcher: Matcher, goalContext: Context, goal: Term): Option[Term]
}

/** the term transformation rules that should be used for complification
 *
 *  separating these rules is important to avoid cycles during simplification
 */
trait ComplificationRule extends TermTransformationRule
