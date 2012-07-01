package info.kwarc.mmt.uom
import info.kwarc.mmt.api._
import objects._
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
   val involution = new Involution(inv)
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

/** the antony of a predicate under negation
 * {{{
 * not p(args) ---> p'(args)
 * }}}
 * This rule should be used with care because its use might make other rules inapplicable.
 * @param neg negation
 * @param pred the predicate
 * @parem negpred the antonym
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
   /** "OMA(of, a, b)" represents the quantity "a bs", e.g., arith1.times */
   val of: GlobalName
   /** a simplifier for counting values, e.g., OMI(1)+OMI(1) ---> OMI(2) */
   val simplify : Term => Term
}

/** Collection and counting of quantities that are multiple of the same base
 * {{{
 *  a1 ... am is simplified by merging successive occurrences of a, k @ a, inv(a), and unit:
 *  1) a ---> 1 @ a
 * 1) unit ---> (remove)
 * 1) inv(a) ---> -1 @ a
 * 2) (k @ a)(l @ a) ---> (k + l) @ a
 * 3) k @ a ---> l @ a  by calling simplify
 * 4) 1 @ a ---> a
 * 4) 0 @ a ---> (remove)
 * // 4) -1 @ a ---> inv(a) 
 * where k @ a = k of a
 * }}}
 *
 * @param comp the aggregation operation, e.g., +
 * @param unit the value of the empty aggregation (i.e., when all terms cancel out), e.g., 0
 * @param inv the unary operator producing a negative aggregate, e.g., -
 * @param commutative If true, comp is commutative, and the elements are first reordered according to the hashcode of a.
 * @param counter an implementation of counting that is used to merge quantities that are multiples of the same base   
*/
class Collect(comp : GlobalName, unit: Option[GlobalName], inv: Option[GlobalName], commutative: Boolean)(count: Counter) extends BreadthRule(comp) {
   private case class Quantity(base: Term, amount: Term) {
      def add(that: Quantity) = Quantity(base, count.add(this.amount, that.amount))
   }
   private def fromTerm(t : Term) : Option[Quantity] = {
      unit match {
         case Some(u) => if (t == OMID(u)) return None
         case _ =>
      }
      inv match {
         case Some(i) => t match {
            case OMA(OMID(`i`), List(arg:Term)) => return Some(Quantity(arg, count.minusone))
            case _ =>
         }
         case _ =>
      }
      t match {
         case OMA(OMID(action), List(arg:Term, amount:Term)) => Some(Quantity(arg, amount))
         case t => Some(Quantity(t, count.one))
      }
   }
   private def toTerm(f: Quantity) = {
      val newamount = count.simplify(f.amount)
      newamount match {
         case count.zero => None
         case count.one => Some(f.base)
         // case count.minusone => Some(inv(f.base))
         case a => Some(count.of(f.base, a))
      }
   }
   val apply: Rewrite = args => {
      var qs = args mapPartial fromTerm
      if (commutative) {
         qs = qs.sortBy(_.base.hashCode) // not optimal if there are hash collisions; a more subtle ordering might be provided  
      }
      var done : List[Quantity] = Nil
      var current : Quantity = qs.head
      var todo : List[Quantity] = qs.init
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
         case 0 => GlobalChange(OMID(unit.get)) // only possible if args == Nil (forbidden) or if all terms cancelled (then inverse and thus unit must exist)
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

/** A Semigroup packages the simplification rules that yield normalization in a semigroup. */
class Semigroup(op: GlobalName) {
   val assoc = new Association(op)
}

/** A Monoid packages the simplification rules that yield normalization in a monoid. */
class Monoid(op: GlobalName, unit: GlobalName) extends Semigroup(op) {
   val neutral = new Neutral(op, unit)
}

/** A BoundedSemiLattice packages the simplification rules that yield normalization in a bounded semilattice. */
class BoundedSemiLattice(op: GlobalName, bound: GlobalName) extends Monoid(op, bound) {
   val idem = new Idempotent(op)
   val comm = new Commutative(op)
}

/** A BooleanLattice packages the simplification rules that yield normalization in a Boolean lattice
 *
 * For example, "BooleanLattice(conj, disj, truth, falsity, neg)" yields the rules for a disjunctive normal form
 * in which redundant occurrences of "truth" and "falsity" are eliminated.  
 */ 
class BooleanLattice(meet: GlobalName, join: GlobalName, top: GlobalName, bottom: GlobalName, compl: GlobalName) {
   val meetSL = new BoundedSemiLattice(meet, top)
   val joinSL = new BoundedSemiLattice(join, bottom)

   // disjunction normal form (only one distributivity rule to ensure termination)
   val distribMJ = new Distribution(meet, join)
   //val distribJM = new Distribution(join, meet)
   
   // negation-normal form
   val deMorganM = new Homomorphism(compl, meet, join)
   val deMorganJ = new Homomorphism(compl, join, meet)
   val complTop = new Antonym(compl, top, bottom)
   val complBottom = new Antonym(compl, bottom, top)
   val doubleComplement = new Involution(compl)

   val contradiction = new Complement(meet, compl, bottom)
   val tnd = new Complement(join, compl, top)
}
