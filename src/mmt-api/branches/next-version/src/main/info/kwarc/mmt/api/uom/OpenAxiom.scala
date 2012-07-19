/*
package info.kwarc.mmt.api.uom

class Axiom protected () {
   protected var form : Boolean = true
   def asserts(p: Boolean) {form = p}
}

abstract class ProofTerm

class Proof(term: ProofTerm) extends Axiom() {
   def check: Boolean = {
      // check if term proofs form
      true
   }
}

abstract class Category {
   type %
   def equal(x: %, y: %) : Boolean = x == y
   def member(x: %) : Boolean = true
}

trait Magma extends Category {
   def op(x: %,y: %): %
}

trait Commutative extends Magma {
   val comm : Axiom
   comm.asserts(true)
}

trait Semigroup extends Magma {
   val assoc : Axiom
   assoc.asserts(true)
}

trait Monoid extends Semigroup {
   val e: %
   val neutrality: Axiom
   neutrality.asserts(true)
}

trait Group extends Monoid {
   def inv(x: %): %
   val inversion: Axiom
   inversion.asserts(true)
}

trait AbelianGroup extends Group with Commutative

trait Ring extends Category {thering =>
   val add: AbelianGroup {
      type % = thering.%
   }
   val mult: Monoid {
      type % = thering.%
   }
   val distrib: Axiom
   distrib.asserts(true)
}

class PolynomialRing(val r: Ring) extends Ring {thepoly =>
   type % = List[r.%]
   val add = new AbelianGroup {
      type % = thepoly.%
      def op(p: %,q: %) = Range(0,math.max(p.length, q.length)).map(
         i => r.add.op(p(i),q(i))
      ).toList
      val e = Nil
      def inv(p: %) = p.map(r.add.inv)
      val assoc = new Proof(null)
      val comm = new Proof(null)
      val inversion = new Proof(null)
      val neutrality = new Proof(null)
   }
   val mult = new Monoid {
      type % = thepoly.%
      def op(p: %, q: %) = Nil
      val e = List(r.add.e, r.mult.e)
      val assoc = new Proof(null)
      val neutrality = new Proof(null)
   }
   val distrib = new Proof(null)
}

abstract class Module extends Category {
   val R: Ring
   val M: AbelianGroup
   type % = M.%
   def scalarMult(s: R.%, m: %): %
   val assoc : Axiom
   assoc.asserts(true)
}

object Integers extends Ring {
   type % = Int
   val add = new AbelianGroup {
      type % = Int
      def op(x: Int, y: Int) = x + y
      val e = 0
      def inv(x: Int) = - x
      val assoc = new Proof(null)
      val comm = new Proof(null)
      val inversion = new Proof(null)
      val neutrality = new Proof(null)
   }
   val mult = new Monoid {
      type % = Int
      def op(x: Int, y: Int) = x * y
      val e = 1
      val assoc = new Proof(null)
      val neutrality = new Proof(null)
   }
   val distrib = new Proof(null)
}

class IntegersMod(n: Int) extends Ring {
   type % = Integers.%
   override def equal(x: %, y: %) = (x - y) % n == 0  
   val add = Integers.add
   val mult = Integers.mult
   val distrib = new Proof(null)
}

class IntModule(val M: AbelianGroup) extends Module {
  val R = Integers
  def scalarMult(n: Integers.%, x: M.%) : M.% =
     if (n == 0) M.e
     else if (n < 0) scalarMult(-n, M.inv(x))
     else if (n == 1) x
     else M.op(scalarMult(n - 1, x), x)
   val assoc = new Proof(null)
}*/