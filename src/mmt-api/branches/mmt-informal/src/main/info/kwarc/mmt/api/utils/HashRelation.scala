package info.kwarc.mmt.api.utils
import scala.collection.mutable._

/**
 * This implements a relation on A and B as a mutable total map from A to the power set of B.
 */
class HashMapToSet[A,B] extends HashMap[A,HashSet[B]] {
   /** returns the set of b such that (a,b) in this (returned as a mutable object)
    *  It returns the empty set by default.
    */
   override def apply(a : A) : HashSet[B] = getOrElseUpdate(a, HashSet.empty)
   /** add a pair to the relation */
   def +=(a: A, b: B) {apply(a) += b}
   /** remove a pair from the relation */
   def -=(a: A, b: B) {apply(a) -= b}
   /** get an iterator over all pairs */
   def pairs : Iterator[(A,B)] = flatMap({case (a, s) => s.map(b => (a,b))}).iterator
   /** remove the entry for a if there are no b such (a,b) is in the relation */
   def cleanup(a : A) {
      val bs = get(a)
      if (bs.isDefined && bs.get.isEmpty) this -= a
   }
}

/**
   This implements a binary relation between A and B.
   The entries are hashed twice so that lookups of both image and preimage are efficient.
*/
class HashRelation[A,B] {
   private val succ = new HashMapToSet[A,B]
   private val pred = new HashMapToSet[B,A]
   def +=(pairs : (A,B)*) {
      for ((a,b) <- pairs) {
         succ(a) += b
         pred(b) += a
      }
   }
   def +=(a : A, b : B) {this += ((a,b))}
   def -=(pairs : (A,B)*) {
      for ((a,b) <- pairs) {
         succ(a) -= b
         succ.cleanup(a)
         pred(b) -= a
         pred.cleanup(b)
      }
   }
   def -=(a : A, b : B) {this -= ((a,b))}
   def contains(a : A, b : B) : Boolean =
      succ(a).contains(b)
   def image(a : A) : HashSet[B] = succ(a)
   def preimage(b : B) : HashSet[A] = pred(b)
   def apply(a : A) = image(a)
   def clear {
      succ.clear
      pred.clear
   }
}

/**
   This implements the reflexive and transitive closure of a HashRelation[A,A].
   The closure is not maintained and lookups use depth-first search.
   In particular, the methods += and -= change the underlying relation, not only the closure.
*/
class ReflTransHashRelation[A] extends HashRelation[A,A] {
   override def contains(a : A, b : A) : Boolean = {
      if (a == b)
         true
      else
         super.image(a).exists(contains(_,b))
   }
   override def image(a : A) : HashSet[A] = {
      //invariant: If result.contains(a) then result.contains(b) for all indirect successors s of a.
      val result = HashSet.empty[A]
      //adds s to the result maintaining the invariant
      def addReachable(s : A) {
         if (! result.contains(s)) {
            result += s
            super.image(s).foreach(addReachable(_))
         }
      }
      addReachable(a)
      result
   }
   def irrImage(a : A) = image(a) - a
   override def preimage(a : A) : HashSet[A] = {
      //invariant: If result.contains(a) then result.contains(b) for all indirect predecessors s of a.
      val result = HashSet.empty[A]
      //adds s to the result maintaining the invariant
      def addReachedFrom(s : A) {
         if (! result.contains(s)) {
            result += s
            super.preimage(s).foreach(addReachedFrom(_))
         }
      }
      addReachedFrom(a)
      result
   }
   def irrPreimage(a : A) = preimage(a) - a
   /** as preimage, but DFO is guaranteed */
   def preimageDFO(a : A) : List[A] = {
      var result : List[A] = Nil
      def addReachedFrom(s : A) {
         if (! result.contains(s)) {
            result = s :: result
            super.preimage(s).foreach(addReachedFrom(_))
         }
       }
       addReachedFrom(a)
       result.reverse
   }
}