/* package info.kwarc.mmt.uom
import info.kwarc.mmt.api._
import objects._

abstract class Result
case class LocalChange(inside: List[Term]) extends Result
case class GlobalChange(it: Term) extends Result

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

abstract class BreadthRule(op: GlobalName) {
   type Rewrite = List[Term] => Result
}

class Cancellation(comp : GlobalName, inv: GlobalName, unit: GlobalName, quantifier: GlobalName) extends BreadthRule(comp) {
   case class Quantity(unit: Term, amount: Term) {
      def +(that: Quantity) = Quantity(unit, add(this.amount, that.amount))
      def nonzero : Boolean = amount 
   }
   def fromTerm(t : Term) : Quantity = t match {
      case OMA(OMID(inv), List(arg))                => Factor(arg, OMI(-1))
      case OMA(OMID(quantifier), List(arg, amount)) => Factor(arg, amount)
      case t => Factor(t, OMI(1))
   }
   def toTerm(f: Factor) = quantifier(f.unit, f.amount)

   val apply: Rewrite = args => {
      val qs = args map fromTerm
      var done : List[Quantity] = Nil
      var current : Quantity = qs.head
      var todo : List[Quantity] = qs.init
      // invariant: done.reverse current todo = qs
      while (todo != Nil) {
         val next = todo.head
         todo = todo.tail
         if (next.unit == current.unit)
            current = current + next
         else {
            done = current :: done
            current = next
         }
      }
      done = (current :: done).filter(nonzero)
      val newargs = done.reverseMap(toTerm)
      if (newargs.length == 1) GlobalChange(newargs.head) else LocalChange(newargs)
   }
}


class Simplifier {
   def rules = new HashMap[GlobalName, List[GlobalName]]  
   def apply(t: Term) : Term = {
      rules.get(t.head) match {
         case Some(r) => r(t)
         case None => 
      }
   }
}

*/