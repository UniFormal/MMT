package info.kwarc.mmt.uom
import info.kwarc.mmt.api._
import objects._
import Conversions._

import utils.MyList._

abstract class Result
case class LocalChange(inside: List[Term]) extends Result
case class GlobalChange(it: Term) extends Result
case object NoChange extends Result

abstract class DepthRule(outer: GlobalName, inner: GlobalName) {
   type Rewrite = (List[Term], List[Term], List[Term]) => Result
   def apply : Rewrite
}

// a1 ... (b1 ... bn) ... am ---> a1 ...b1 ... bn ... am
class Association(comp: GlobalName) extends DepthRule(comp, comp) {
   val apply : Rewrite = (before, inside, after) => LocalChange(inside)
}

// a1 ... e ... am ---> a1 ... am
class Neutral(comp: GlobalName, unit: GlobalName) extends DepthRule(comp, unit) {
   val apply : Rewrite = (before, inside, after) =>
      (before ::: after).length match {
         case 0 => GlobalChange(OMID(unit))
         case 1 => GlobalChange((before:::after).head)
         case _  => LocalChange(Nil)
      }
}

class UnaryInverse(inv: GlobalName) {
   // inv inv a ---> a
   object Involution extends DepthRule(inv, inv) {
      val apply : Rewrite = (before, inside, after) => GlobalChange(inside.head)
   }
   // inv e ---> e
   class Neutral(unit: GlobalName) extends DepthRule(inv, unit) {
      val apply : Rewrite = (before, inside, after) => GlobalChange(OMID(unit))
   }
   // inv (a1 ... an) ---> inv(an) ... inv(a1)
   class Composition(comp: GlobalName) extends DepthRule(inv, comp) {
      val apply : Rewrite = (before, inside, after) => GlobalChange(comp(inside.reverseMap(inv(_))))
   }
   // inv (a/b) ---> b/a
   class BinaryInverse(bininv: GlobalName) extends DepthRule(inv, bininv) {
      val apply : Rewrite = (before, inside, after) => GlobalChange(bininv(inside.reverse))
   }
}

// a1 ... (b1 + ... + bn) ... am ---> (a1 ... b1 ... am) + ... + (a1 ... bn ... am)
class Distribution(times: GlobalName, plus: GlobalName) extends DepthRule(times, plus) {
   val apply : Rewrite = (before, inside, after) => GlobalChange(plus(inside.map(s => times(before ::: s :: after))))
}

// a1...am * (b1 ... bn * v) ---> (a1 ... am b1 ... bn) * v
class SemigroupAction(comp: GlobalName, act: GlobalName) extends DepthRule(act, act) {
   val apply: Rewrite = (before, inside, after) => GlobalChange(act(comp(before ::: inside.init), inside.last))
}

// e * v ---> v
// a1 ... e ... am * v ---> a1 ... am * v
class MonoidAction(unit: GlobalName, act: GlobalName) extends DepthRule(act, unit) {
   val apply: Rewrite = (before, inside, after) =>
      before ::: after.init match {
         case Nil => GlobalChange(after.last)
         case l => LocalChange(Nil)
      }
}

// beta and eta reduction, this class only applies to the case where application is a special symbol
class Lambda(lambda: GlobalName, app: GlobalName) {
   // (lambda x1:A1, ..., xm:Am.s) a1 ... an ---> s[x1/a1, ..., xm/am] a{m+1} ... an   if n >= m
   // (lambda x1:A1, ..., xm:Am.s) a1 ... an ---> lambda x{n+1}:A{n+1}, ..., xm:Am. s[x1/a1, ..., xn/an]   if n < m
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
   object Eta extends DepthRule(lambda, app) {
      val apply: Rewrite = (before, inside, after) => {
         NoChange //TODO eta reduction
      }
   }
}

abstract class BreadthRule(op: GlobalName) {
   type Rewrite = List[Term] => Result
}

/** Implementations of Counter permit basic counting and taking multiples of quantities */
trait Counter {
   val zero : Term // symbol or OMI(0)
   val one  : Term // symbol or OMI(1)
   val minusone : Term // symbol or OMI(-1)
   val add: GlobalName // add amount
   val of: GlobalName // apply amount to base
   val simplify : Term => Term
}

/* a1 ... am is simplified by merging successive occurrences of a, k @ a, inv(a), and unit:
 1) a ---> 1 @ a
 1) unit ---> (remove)
 1) inv(a) ---> -1 @ a
 2) (k @ a)(l @ a) ---> (k + l) @ a
 3) k @ a ---> l @ a  by calling simplify
 4) 1 @ a ---> a
 4) 0 @ a ---> (remove)
 // 4) -1 @ a ---> inv(a) 
where k @ a = k of a
If commutative == true, the elements are first reordered according to the hashcode of a.
*/
class Collect(comp : GlobalName, unit: Option[GlobalName], inv: Option[GlobalName], commutative: Boolean)(count: Counter) extends BreadthRule(comp) {
   case class Quantity(base: Term, amount: Term) {
      def add(that: Quantity) = Quantity(base, count.add(this.amount, that.amount))
   }
   def fromTerm(t : Term) : Option[Quantity] = {
      unit match {
         case Some(u) => if (t == OMID(u)) return None
         case _ =>
      }
      inv match {
         case Some(i) => t match {
            case OMA(OMID(`i`), List(arg)) => return Some(Quantity(arg, count.minusone))
            case _ =>
         }
         case _ =>
      }
      t match {
         case OMA(OMID(action), List(arg, amount)) => Some(Quantity(arg, amount))
         case t => Some(Quantity(t, count.one))
      }
   }
   def toTerm(f: Quantity) = {
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
         qs = qs.sortBy(_.base.hashCode)(scala.math.Ordering.Int) // not optimal if there are hash collisions; a more subtle ordering might be provided  
      }
      var done : List[Quantity] = Nil
      var current : Quantity = qs.head
      var todo : List[Quantity] = qs.init
      // invariant: done.reverse current todo = qs
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