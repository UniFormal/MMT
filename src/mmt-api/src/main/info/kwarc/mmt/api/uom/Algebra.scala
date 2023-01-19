/* nice, but were never used; should be merged into a novel algebra tactic

package info.kwarc.mmt.api.uom
import info.kwarc.mmt.api._
import objects._
import checking._
import Conversions._

import utils.MyList._

/** Associativity
 * {{{
 * a1 ... (b1 ... bn) ... am ---> a1 ...b1 ... bn ... am
 * }}}
 */
class Association(comp: GlobalName) extends DepthRule(comp, comp) {
   val apply : Rewrite = (before, inside, after) => LocalChange(inside)
}

/** A neutral element
 * {{{
 * a1 ... e ... am ---> a1 ... am
 * }}}
 */
class Neutral(comp: GlobalName, unit: GlobalName) extends DepthRule(comp, unit) {
   val apply : Rewrite = (before, inside, after) =>
      (before ::: after).length match {
         case 0 => GlobalChange(OMID(unit))
         case 1 => GlobalChange((before:::after).head)
         case _  => LocalChange(Nil)
      }
}

/** An attractor element
 * {{{
 * a1 ... b ... an ---> b
 * }}}
 */
class Attractor(comp: GlobalName, attr: GlobalName) extends DepthRule(comp, attr) {
   val apply: Rewrite = (before, inside, after) => GlobalChange(OMID(attr))
}

/** A class that defines several rules for a unary inverse operator in an algebraic theory */
class UnaryInverse(inv: GlobalName) {
   /**
    * {{{
    * inv inv a ---> a
    * }}}
    */
   object Involution extends Involution(inv)
   /**
    * {{{
    * inv e ---> e
    * }}}
    */
   class Neutral(unit: GlobalName) extends DepthRuleUnary(inv, unit) {
      val applyUnary: RewriteUnary = inside => GlobalChange(OMID(unit))
   }
   /**
    * {{{
    * inv (a1 ... an) ---> inv(an) ... inv(a1)
    * }}}
    */
   class Composition(comp: GlobalName) extends DepthRuleUnary(inv, comp) {
      val applyUnary : RewriteUnary = inside => GlobalChange(comp(inside.reverseMap(inv(_))))
   }
   /**
    * {{{
    * inv (a/b) ---> b/a
    * }}}
    */
   class BinaryInverse(bininv: GlobalName) extends DepthRuleUnary(inv, bininv) {
      val applyUnary : RewriteUnary = inside => GlobalChange(bininv(inside.reverse))
   }
}

/** Distributivity
 * {{{
 * a1 ... (b1 + ... + bn) ... am ---> (a1 ... b1 ... am) + ... + (a1 ... bn ... am)
 * }}}
 */
class Distribution(times: GlobalName, plus: GlobalName) extends DepthRule(times, plus) {
   val apply : Rewrite = (before, inside, after) => GlobalChange(plus(inside.map(s => times(before ::: s :: after))))
}

/** A semigroup acting on a set
 * {{{
 * a1...am * (b1 ... bn * v) ---> (a1 ... am b1 ... bn) * v
 * }}}
 */
class SemigroupAction(comp: GlobalName, act: GlobalName) extends DepthRule(act, act) {
   val apply: Rewrite = (before, inside, after) => GlobalChange(act(comp(before ::: inside.init), inside.last))
}

/** Additional rules for a monoid acting on a set
 * @see SemigroupAction
 * {{{
 * e * v ---> v
 * a1 ... e ... am * v ---> a1 ... am * v
 * }}}
 */
class MonoidAction(unit: GlobalName, act: GlobalName) extends DepthRule(act, unit) {
   val apply: Rewrite = (before, inside, after) =>
      before ::: after.init match {
         case Nil => GlobalChange(after.last)
         case l => LocalChange(Nil)
      }
}

/** Signs in a product
 * {{{
 * a1 ... -b ... am ---> - a1 ... b ... am
 * }}}
 * sign must be unary
*/
class Sign(op: GlobalName, sign: GlobalName) extends DepthRule(op, sign) {
   val apply: Rewrite = (before, inside, after) => GlobalChange(sign(op(before ::: inside ::: after)))
}

/** a projective function
 * {{{
 * f(f(a)) ---> f(a)
 * }}}
 */
class Projection(op: GlobalName) extends DepthRuleUnary(op, op) {
   val applyUnary: RewriteUnary = inside => LocalChange(inside)
}

/** a self-inverse function
 * {{{
 * f(f(a)) ---> a
 * }}}
 */
class Involution(op: GlobalName) extends DepthRuleUnary(op, op) {
   val applyUnary : RewriteUnary = inside => GlobalChange(inside.head)
}

/** the antonym of a predicate under negation
 * {{{
 * not p(args) ---> p'(args)
 * }}}
 * This rule should be used with care because its use might make other rules inapplicable.
 * @param neg negation
 * @param pred the predicate
 * @param negpred the antonym
 */
class Antonym(neg: GlobalName, pred: GlobalName, negpred: GlobalName) extends DepthRuleUnary(neg, pred) {
   val applyUnary : RewriteUnary = inside => GlobalChange(negpred(inside))
}

/** A homomorphism interacting with an operation
 * {{{
 * hom(op1(a1,...,an)) ---> op2(hom(a1),...,hom(an))
 * }}}
 * This includes algebra morphisms, linear maps, functors, etc.
 * @param hom the homomorphism
 * @param op1 the operator in the domain
 * @param op2 the corresponding operator in the codomain
 */
class Homomorphism(hom: GlobalName, op1: GlobalName, op2: GlobalName) extends DepthRuleUnary(hom, op1) {
   val applyUnary: RewriteUnary = inside => GlobalChange(op2(inside.map(hom(_))))
}

/** beta and eta reduction
 *
 * This class only applies to the case where application is a special symbol.
 */
class Lambda(lambda: GlobalName, app: GlobalName) {
   /** {{{
    * (lambda x1:A1, ..., xm:Am.s) a1 ... an ---> s[x1/a1, ..., xm/am] a{m+1} ... an   if n >= m
    * (lambda x1:A1, ..., xm:Am.s) a1 ... an ---> lambda x{n+1}:A{n+1}, ..., xm:Am. s[x1/a1, ..., xn/an]   if n < m
    * }}}
    */
   object Beta extends DepthRule(app, lambda) {
      val apply: Rewrite = (before, inside, after) => {
         if (! before.isEmpty) NoChange else {
            val vars = inside.init.asInstanceOf[List[VarDecl]]
            val numvars = vars.length
            val scope = inside.last
            val numargs = after.length
            val reds = if (numvars > numargs)
               vars.take(numargs).zip(after)
            else
               vars.zip(after.take(numvars))
            val sub = reds map {case (v,a) => Sub(v.name, a)}
            val reduced = scope ^ sub
            if (numvars > numargs)
               GlobalChange(OMBIND(OMID(lambda), vars.drop(numargs), reduced))
            else if (numvars == numargs)
               GlobalChange(reduced)
            else
               LocalChange(reduced :: after.drop(numvars))
         }
      }
   }
   /** currently unimplemented */
   object Eta extends DepthRule(lambda, app) {
      val apply: Rewrite = (before, inside, after) => {
         NoChange //TODO eta reduction
      }
   }
}

/*
/** Implementations of Counter permit basic counting and taking multiples of quantities
 *
 * This is used as an auxiliary class to collect and count multiples of the same quantity, e.g., in a polynomial.
 * @see Collect
 */
trait Counter {
   /** 0, usually either a symbol or OMI(0) */
   val zero : Term //
   /** 1, usually either a symbol or OMI(1) */
   val one  : Term
   /** -1, usually either a symbol or OMI(-1) */
   val minusone : Term
   /** binary addition of amounts, e.g., arith1.plus */
   val add: GlobalName
   /** "of(a, b)" represents the quantity "a bs", e.g., arith1.times */
   val of: GlobalName
   /** is it OMA(of, base, amount)? */
   val ofBaseAmount : Boolean
   /** is it OMA(of, amount, base)? */
   val ofAmountBase = ! ofBaseAmount
   /** a simplifier for counting values, e.g., OMI(1)+OMI(1) ---> OMI(2) */
   val simplify : Term => Term
}

object IntegerCounter extends Counter {
   val zero = OMI(0)
   val one = OMI(1)
   val minusone = OMI(-1)
   val add = utils.OpenMath.arith1.plus
   //OpenMath declares this for the multiplicative power in rings even though it works for any semigroup
   val of = utils.OpenMath.base ? "ring1" ? "power"
   val ofBaseAmount = true
   def simplify(t: Term) = t match {
      case OMA(add, args) =>
         var nums : BigInt = 0
         val symArgs = args filter {
            case OMI(n) => nums += n; false
            case _ => true
         }
         val all = OMI(nums) :: symArgs
         all.length match {
            case 0 => OMI(0) //impossible
            case 1 => all.head
            case _ => OMA(add, all)
         }
      case _ => t
   }
}
*/

/** Collection and counting of quantities that are integer multiples of the same base
 * {{{
 *  a1 ... am is simplified by merging successive occurrences of a, k @ a, inv(a), and unit:
 * 1) a ---> 1 @ a
 * 1) unit ---> (remove)
 * 1) inv(a) ---> -1 @ a
 * 2) (k @ a)(l @ a) ---> (k + l) @ a
 * 3) k @ a ---> l @ a  by adding integers
 * 4) 1 @ a ---> a
 * 4) 0 @ a ---> (remove)
 * 4) -1 @ a ---> inv(a)
 * where k @ a = k of a
 * }}}
 *
 * @param comp the aggregation operation, e.g., +
 * @param action the above @
 * @param unit the value of the empty aggregation (i.e., when all terms cancel out), e.g., 0
 * @param inv the unary operator producing a negative aggregate, e.g., -
 * @param commutative If true, comp is commutative, and the elements are first reordered according to the hashcode of a.
 * action, unit, inv, commutative are mutable so that the Collect rule can be strengthened easily when forming RuleSet's by inheritance
*/
class Collect(comp : GlobalName, var action: GlobalName, var unit: Option[GlobalName], var inv: Option[GlobalName], var commutative: Boolean) extends BreadthRule(comp) {
   private case class Quantity(base: Term, amount: BigInt) {
      def add(that: Quantity) = Quantity(base, this.amount + that.amount)
   }
   import OMLiteral.OMI
   private object QuantityMatcher {
      def unapply(t: Term) : Option[(Term,BigInt)] = t match {
         case OMA(OMS(op), List(base, OMI(n))) if op == action => Some((base, n))
         case _ => None
      }
   }
   private def fromTerm(t : Term) : Option[Quantity] = {
      t match {
         case OMS(u) if unit == Some(u) => None
         case OMA(OMS(i), List(QuantityMatcher(base, n))) if inv == Some(i) => Some(Quantity(base, -n))
         case OMA(OMS(i), List(arg)) if inv == Some(i) => Some(Quantity(arg, -1))
         case QuantityMatcher(base, n) => Some(Quantity(base,n))
         case t => Some(Quantity(t, 1))
      }
   }
   private def toTerm(f: Quantity) = {
      f.amount match {
         case a if a == 0 => None
         case a if a == 1 => Some(f.base)
         case a if a == -1 && inv.isDefined => Some(inv.get(f.base)) //inv.isEmpty is unreasonable if count.minusone is possible
         case a => Some(action(f.base, OMI(a)))
      }
   }
   val apply: Rewrite = args => {
      var qs = args mapPartial fromTerm
      if (commutative) {
         qs = qs.sortBy(_.base.hashCode) // not optimal if there are hash collisions; a more subtle ordering might be provided
      }
      var done : List[Quantity] = Nil
      var current : Quantity = qs.head
      var todo : List[Quantity] = qs.tail
      // invariant: comp(done.reverse, current, todo) = comp(comp, qs)
      while (todo != Nil) {
         val next = todo.head
         todo = todo.tail
         if (next.base == current.base)
            current = current.add(next)
         else {
            done = current :: done
            current = next
         }
      }
      done = (current :: done)
      val newargs = done.reverse.mapPartial(toTerm)
      newargs.length match {
         case 0 if unit.isDefined => GlobalChange(OMID(unit.get)) // unit.isEmpty is unreasonable; then we use OMA(op, Nil) in lieu of unit element
         case 1 => GlobalChange(newargs.head)
         case _ => LocalChange(newargs)
      }
   }
}

/** Commutativity of op
 *
 * This orders the arguments by their hash code.
 */
class Commutative(op: GlobalName) extends BreadthRule(op) {
   val apply: Rewrite = args => {
      val newargs = args.sortBy(_.hashCode)
      LocalChange(newargs)
   }
}

/** Idempotency of an operation (e.g., in a semi-lattice)
 * {{{
 * a a ---> a
 * }}}
 */
class Idempotent(op: GlobalName) extends BreadthRule(op) {
   val apply: Rewrite = args => {
      var rest = args
      var done : List[Term] = Nil
      var previous : Term = null
      // op(done.reverse, rest) = op(args)
      while (rest != Nil) {
         val current = rest.head
         if (current != previous) {
            done = current :: done
            previous = current
         }
         rest = rest.tail
      }
      LocalChange(done.reverse)
   }
}

/** Complementation
 * {{{
 * a1 ... b ... (neg b) ... an ---> bound
 * }}}
 *
 * this covers the two rules for the complement in a boolean lattice
 * @param the aggregate operation, e.g., conjunction/disjunction
 * @param neg negation
 * @param bound the result if an argument occurs both positive and negative, e.g., false/true
 */
class Complement(op: GlobalName, neg: GlobalName, bound: GlobalName) extends BreadthRule(op) {
   val apply: Rewrite = args => {
      if (args exists {arg => args exists {_ == neg(arg)}})
         GlobalChange(OMID(bound))
      else
         NoChange
   }
}

// now some prepackaged sets of rules for common structures

/** A Semigroup packages the simplification rules that yield normalization in a semigroup. */
class Semigroup(op: GlobalName) extends MutableRuleSet {
   protected val collect = new Collect(op, utils.OpenMath.base ? "ring1" ? "power", None, None, false)
   declares (
     new Association(op),
     collect
   )
  def associate(args: List[Term]): Term = args match {
     case Nil => OMA(OMS(op),Nil) // should not happen
     case hd::Nil => hd
     case hd::tl => op(hd, associate(tl))
  }
}

/** A Monoid packages the simplification rules that yield normalization in a monoid. */
class Monoid(op: GlobalName, unit: GlobalName) extends Semigroup(op) {
   collect.unit = Some(unit)
   declares (
      new Neutral(op, unit)
   )

  override def associate(args: List[Term]) = if (args.isEmpty) OMS(unit) else super.associate(args)
}

/** A Group packages the simplification rules that yield normalization in a group. */
class Group(op: GlobalName, unit: GlobalName, inv: GlobalName) extends Monoid(op,unit) {
   private val unaryInverse = new UnaryInverse(inv)
   collect.inv = Some(inv)
   declares (
      unaryInverse.Involution,
      new unaryInverse.Neutral(unit),
      new unaryInverse.Composition(op)
   )
}

/** A CGroup packages the simplification rules that yield normalization in a commutative group. */
class CGroup(op: GlobalName, unit: GlobalName, inv: GlobalName) extends Group(op,unit,inv) {
   collect.commutative = true
}

/** A Ring packages the simplification rules that yield normalization in a ring (as a sum of products). */
class Ring(plus: GlobalName, zero: GlobalName, minus: GlobalName, times: GlobalName, one: GlobalName) extends CGroup(plus, zero, minus) {
   protected val multCollect = new Collect(times, utils.OpenMath.base ? "ring1" ? "power", Some(one), None, false)
   collect.action = times
   imports (
      new Monoid(times, one)
   )
   declares (
      new Distribution(times, plus),
      multCollect,
      // some important theorems for simplification
      new Attractor(times, zero),
      new Sign(times, minus)
   )
}

/** A CRing packages the simplification rules that yield normalization in a commutative ring (as a sum of products). */
class CRing(plus: GlobalName, zero: GlobalName, minus: GlobalName, times: GlobalName, one: GlobalName) extends Ring(plus, zero, minus, times, one) {
   multCollect.commutative = true
}

/** A BoundedSemiLattice packages the simplification rules that yield normalization in a bounded semilattice. */
class BoundedSemiLattice(op: GlobalName, bound: GlobalName) extends Monoid(op, bound) {
   declares (
      new Idempotent(op),
      new Commutative(op)
   )
}

/** A BooleanLattice packages the simplification rules that yield normalization in a Boolean lattice
 *
 * For example, "BooleanLattice(conj, disj, truth, falsity, neg)" yields the rules for a disjunctive normal form
 * in which redundant occurrences of "truth" and "falsity" are eliminated.
 */
class BooleanLattice(meet: GlobalName, join: GlobalName, top: GlobalName, bottom: GlobalName, compl: GlobalName) extends MutableRuleSet {
   imports (
       new BoundedSemiLattice(meet, top),
       new BoundedSemiLattice(join, bottom)
   )

   declares (
      // disjunction normal form (only one distributivity rule to ensure termination)
      new Distribution(meet, join),
      //new Distribution(join, meet)

      // negation-normal form
      new Homomorphism(compl, meet, join),
      new Homomorphism(compl, join, meet),
      new Antonym(compl, top, bottom),
      new Antonym(compl, bottom, top),
      new Involution(compl),

      new Complement(meet, compl, bottom),
      new Complement(join, compl, top)
   )
}

*/