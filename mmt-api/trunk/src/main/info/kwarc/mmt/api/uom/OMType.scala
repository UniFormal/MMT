package info.kwarc.mmt.api.uom

/**
 * a partial equivalence relation on a Scala type
 * 
 * @tparam U the underlying Scala type;
 * keeping track of this this is semantically redundant, but it boosts Scala's type checking tremendously 
 * 
 * This is called Category in OpenAxiom where 'universe' is called '%'.
 */
abstract class OMType[U] {
   /** the underlying type Scala */
   type universe = U
   /** the membership test
    * @return true iff the argument is in the type
    */
   def is(a: universe): Boolean
   /** the equality test
    * 
    * @return if is(a) && is(b), then true iff the arguments are considered equal; otherwise, unspecified
    */
   def equal(a: universe, b: universe): Boolean
   
   /** syntactic sugar for infix notation 'T subtype predicate' */
   def subtype(f: U => Boolean) = Subtype(this)(f)
   /** syntactic sugar for infix notation 'T quotient relation' */
   def quotient(f: (U,U) => Boolean) = Quotient(this)(f)
   /** syntactic sugar for infix notation 'S product T' */
   def product[A](t: OMType[A]) = Product(this, t)
}

object BaseType {
   /** An OMType that is identical to a Scala type */
   def apply[A] = new OMType[A] {
      def is(a: universe): Boolean = true
      def equal(a: universe, b: universe): Boolean = a == b
   }
}

object Product {
   /** the product of two OMType's implemented in terms of Scala products */
   def apply[A,B](left: OMType[A], right: OMType[B]) = new OMType[(A,B)] {
      def is(x: universe): Boolean = left.is(x._1) && right.is(x._2)
      def equal(x: universe, y: universe): Boolean = left.equal(x._1, y._1) && right.equal(x._2,y._2)
   }
}

object Subtype {
   /** the subtype of a OMType by a Scala predicate implemented in terms of the same underlying Scala type */
   def apply[A](of: OMType[A])(where: of.universe => Boolean) = new OMType[A] {
      def is(a: universe): Boolean = of.is(a) && where(a)
      def equal(a: universe, b: universe): Boolean = of.equal(a,b)
   }
}

object Quotient {
   /** the quotient of an OMType by an Scala relation implemented in terms of the same underlying Scala type
    * 
    * @param of the base type
    * @param by the equivalence relation
    * @return the quotient
    * 
    * pre: by must be an equivalence relation that refines of.equal
    */
   def apply[A](of: OMType[A])(by: (of.universe,of.universe) => Boolean) = new OMType[A] {
      def is(a: universe): Boolean = of.is(a)
      def equal(a: universe, b: universe): Boolean = of.equal(a,b) && by(a,b)
   }
}

object Lists {
   /** the type of lists over an OMType implemented in terms of Scala lists
    */
   def apply[A](over: OMType[A]) = new OMType[List[A]] {
      def is(a: universe): Boolean = a.forall {x => over.is(x)}
      def equal(a: universe, b: universe): Boolean = a.length == b.length && (a zip b).forall {
         case (x,y) => over.equal(x,y)
      }
   }
}

object Options {
   /** the type of options over an OMType implemented in terms of Scala options
    */
   def apply[A](over: OMType[A]) = new OMType[Option[A]] {
      def is(a: universe): Boolean = a.forall {x => over.is(x)}
      def equal(a: universe, b: universe): Boolean = (a,b) match {
         case (None,None) => true
         case (Some(x), Some(y)) => over.equal(x,y)
         case _ => false
      }
   }
}

/** defines some standard types of numbers (starting with Scala integers) */
object Numbers {
   /** integer numbers */
   val Int = BaseType[BigInt]
   /** natural numbers with 0 */
   val Nat = Int subtype (_ >= 0)
   /** natural numbers without 0 */
   val PositiveNat = Int subtype (_ > 0)
   /** rational numbers */
   val Rat = (Int product PositiveNat) quotient ((x,y) => x._1 * y._2 == x._2 * y._1)
   /** complex numbers with rational real and imaginary part */
   val Complex = Rat product Rat
   /** integers modulo n
    * @param modulus the number n (must be positive non-zero)
    * @return the type representing Z / nZ
    */
   def IntModulo(modulus: Int) = Int quotient ((x,y) => (x-y) % modulus == 0)
}

import Numbers._

object Plus {
   def apply(x: Nat.universe*): Nat.universe = {
      x.foldLeft[BigInt](0){case (a,b) => a + b}
   }
}
