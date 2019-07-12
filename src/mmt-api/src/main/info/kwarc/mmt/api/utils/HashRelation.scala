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
   def getOrEmpty(a : A) : HashSet[B] = getOrElse(a, HashSet.empty)
   /** add a pair to the relation */
   def +=(a: A, b: B) {apply(a) += b}
   /** remove a pair from the relation */
   def -=(a: A, b: B) {apply(a) -= b}
   /** get an iterator over all pairs */
   def pairs : Iterator[(A,B)] = flatMap({case (a, s) => s.map(b => (a,b))}).iterator
   /** remove the entry for a if there are no b such (a,b) is in the relation */
   def cleanup(a : A) {
      get(a).foreach {b => if (b.isEmpty) this -= a}
   }

   override def toString = {
      val sb = new StringBuilder
      foreach {case (a,hs) =>
         if (!hs.isEmpty) {
            sb.append(a.toString + "\n")
            hs foreach {b => sb.append("  " + b.toString + "\n")}
         }
      }
      sb.result
   }
}

import java.util.Date

class InvalidatableValue[B](val value: B) {
  var validSince = new Date()
}

/** represents a map this:A -> Set[B] in a way that supports change management
 *  
 *  entries know their cause, and classes of entries with the same cause can be invalidated in constant time
 */
abstract class MoCHashMapToSet[A,B,Cause] {
  /** the underlying map
   *  entries may be invalid and must be checked using isValid before exposing to the outside
   */
  private val underlying = new HashMapToSet[A,InvalidatableValue[B]]
  
  /** invalidSince(k) == d means that all entries with cause k added before d are invalid */
  private val invalidSince = new HashMap[Cause,Date]

  /** the causes of an entry
   *  if there are multiple causes, they must all be valid for the entry to be valid
   */ 
  def getCauses(a: A, b: B): Iterator[Cause]
  
  /** checks if v is valid */
  private def isValid(a: A, v: InvalidatableValue[B]) = {
    getCauses(a,v.value) forall {c => 
      invalidSince.get(c) match {
        case None =>
          // cause is valid
          true
        case Some(d) =>
          // cause was invalidated: check if value was restored more recently 
          v.validSince after d
      }
    }
  }
  
  /** adds b to this(a) */
  def +=(a: A, b: B) {
    val current = underlying.getOrEmpty(a)
    val exists = current.find {v =>
      v.value == b
    }
    exists match {
      case None =>
        // new value: add
        current += new InvalidatableValue(b)
      case Some(v) =>
        // existing value: keep and update validity timestamp
        v.validSince = new Date()
    }
  }
  
  /** retrieves (a copy of) the set of values for a */ 
  def apply(a: A) = {
    val vs = underlying.getOrEmpty(a)
    // filter out the invalid values and (clean up) remove them
    vs foreach {v =>
      if (!isValid(a,v)) {
        vs -= v
      }
    }
    vs.map(_.value)
  }
  
  /** invalidates all entries with a given cause */
  def invalidate(c: Cause) {
    invalidSince(c) = new Date()
  }
  
  /** delete all data */
  def clear {
    underlying.clear
    invalidSince.clear
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
