package info.kwarc.mmt.api.utils
import info.kwarc.mmt.api.GeneralError

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
   def +=(a: A, b: B): Unit = {apply(a) += b}
   /** remove a pair from the relation */
   def -=(a: A, b: B): Unit = {apply(a) -= b}
   /** get an iterator over all pairs */
   def pairs : Iterator[(A,B)] = flatMap({case (a, s) => s.map(b => (a,b))}).iterator
   /** remove the entry for a if there are no b such (a,b) is in the relation */
   def cleanup(a : A): Unit = {
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

import scala.collection.mutable.ListBuffer
abstract class HashMapToOrderedSet[A,B] extends HashMap[A,ListBuffer[B]] {
  def value(b: B): Int
  override def apply(a: A) = getOrElseUpdate(a, new ListBuffer[B])
  def insert(a: A, b: B): Unit = {
    val bs = apply(a)
    val v = value(b)
    val i = bs.indexWhere(x => value(x) > v)
    bs.insert(i,b)
  }
}

/**
   This implements a binary relation between A and B.
   The entries are hashed twice so that lookups of both image and preimage are efficient.
*/
class HashRelation[A,B] {
   private val succ = new HashMapToSet[A,B]
   private val pred = new HashMapToSet[B,A]
   def +=(pairs : (A,B)*): Unit = {
      for ((a,b) <- pairs) {
         succ(a) += b
         pred(b) += a
      }
   }
   def +=(a : A, b : B): Unit = {this += ((a,b))}
   def -=(pairs : (A,B)*): Unit = {
      for ((a,b) <- pairs) {
         succ(a) -= b
         succ.cleanup(a)
         pred(b) -= a
         pred.cleanup(b)
      }
   }
   def -=(a : A, b : B): Unit = {this -= ((a,b))}
   def contains(a : A, b : B) : Boolean =
      succ(a).contains(b)
   def image(a : A) : HashSet[B] = succ(a)
   def preimage(b : B) : HashSet[A] = pred(b)
   def apply(a : A) = image(a)
   def clear: Unit = {
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
      def addReachable(s : A): Unit = {
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
      def addReachedFrom(s : A): Unit = {
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
      def addReachedFrom(s : A): Unit = {
         if (! result.contains(s)) {
            result = s :: result
            super.preimage(s).foreach(addReachedFrom(_))
         }
       }
       addReachedFrom(a)
       result.reverse
   }
}


/** efficiently maintains the transitive closure a changing relation on a type T
  * the generating relation is defined calls to add and delete
  *
  * lookup is constant time
  * adding/deleting is linear in the number of paths using the new/deleted edge
  */
class IncrementalTransitiveClosure[T] {
  /** direct edges */
  private val edges = new HashSet[(T,T)]
  /* t in pathsFrom(f) iff there is a path from f to t */
  private val pathsFrom = new HashMapToSet[T,T]
  /* f in pathsTo(t) iff there is a path from f to t */
  private val pathsTo = new HashMapToSet[T,T]
  /* numPaths(f,t) is the number of paths from f to t, use getNumPaths for lookup */
  private val numPaths = new HashMap[(T,T),Int]

  /** number of paths including reflexive edges, defaults to 0 */
  @inline private def getNumPaths(f: T, t: T) = {
    if (f == t) 1 else numPaths.getOrElse((f,t), 0)
  }

  private def checkInvariant(s: String): Unit = {
    //println(s)
    numPaths.keys foreach {case (a,b) =>
      if (! (pathsFrom(a) contains b)) {
        println("class invariant violated after " + s)
        println(s"no path from $a to $b exists but number of paths is non-zero")
      }
      if (! (pathsTo(b) contains a)) {
        println("class invariant violated after " + s)
        println(s"no path to $b from $a exists but number of paths is non-zero")
      }
    }
    pathsFrom foreach {case (f,ts) =>
      ts foreach {t =>
        if (!(pathsTo(t) contains f)) {
          println("class invariant violated after " + s)
          println(s"path from $f to $t but no path to $t from $f")
        }
      }
    }
    pathsTo foreach {case (t,fs) =>
      fs foreach {f =>
        if (!(pathsFrom(f) contains t)) {
          println("class invariant violated after " + s)
          println(s"path to $t from $f but no path from $f to $t")
        }
      }
    }
    edges foreach {case (f,t) =>
      if (!apply(f,t)) {
        println("class invariant violated after " + s)
        println(s"edge $f -> $t but no path $f -> $t")
      }
    }
    pathsFrom.keys foreach {f =>
      val outEdges = edges.filter {case (a,b) => a == f}.toList
      pathsTo.keys foreach {t =>
        val pathsAfter = outEdges.map {case (_,b) =>
          (b,getNumPaths(b,t))
        }
        val npExpected = if (f == t) 1 else pathsAfter.map(_._2).sum
        val npStored = getNumPaths(f,t)
        if (npExpected != npStored) {
          println("class invariant violated after " + s)
          println(s"paths $f -> $t via: " + pathsAfter.mkString("\n","\n",""))
          println(s"$npExpected paths exist but $npStored paths stored")
          //throw GeneralError(s"invalid state")
        }
      }
    }
  }

  /** add a number of paths from f to t */
  @inline private def addPaths(f:T, t: T, num: Int): Unit = {
    if (f == t) println(s"invariant violated: self-edge $f -> $t")
    numPaths((f,t)) = getNumPaths(f,t) + num
    pathsFrom(f) += t
    pathsTo(t) += f
  }
  /** remove a number of paths from f to t */
  @inline private def deletePaths(f:T, t: T, num: Int): Unit = {
    if (f == t && num == 1) {
      // reflexive paths are not stored
      return
    }
    val np = numPaths((f,t)) - num
    if (np < 0) throw GeneralError(s"invalid state: $np paths $f -> $t")
    if (np == 0) {
      // no more path, remove from closure
      pathsFrom(f) -= t
      pathsTo(t) -= f
      numPaths -= ((f,t))
    } else {
      numPaths((f,t)) = np
    }
  }
  /** iterate over nodes y such y < x; y <= x if refl == true */
  @inline private def iterateBelow(x: T, refl: Boolean)(f: T => Unit): Unit = {
    if (refl) f(x)
    pathsTo.getOrEmpty(x) foreach f
  }
  /** iterate over nodes y such x < y; x <= y if refl == true */
  @inline private def iterateAbove(x: T, refl: Boolean)(f: T => Unit): Unit = {
    if (refl) f(x)
    pathsFrom.getOrEmpty(x) foreach f
  }

  /** true if (from,to) is in the transitive closure */
  def apply(from: T, to: T) = getNumPaths(from,to) > 0

  @inline def getAbove(x: T, refl: Boolean) = {
    val it = pathsFrom(x).iterator
    if (refl) Iterator(x) ++ it else it
  }
  @inline def getBelow(x: T, refl: Boolean) = {
    val it = pathsTo(x).iterator
    if (refl) Iterator(x) ++ it else it
  }

  /** add an edge to the underlying relation */
  def add(from: T, to: T): Unit = {
    if (edges contains (from,to)) {
      return
    } else {
      edges += ((from,to))
    }
    iterateBelow(from, true) {b =>
      iterateAbove(to, true) {a =>
        // b in below  -old path-> from -new edge-> to -old path-> a in above
        addPaths(b,a, getNumPaths(b, from)*getNumPaths(to,a))
      }
    }
    //checkInvariant(s"adding $from -> $to")
  }
  /** delete an edge from the underlying relation */
  def delete(from: T, to: T): Unit = {
    if (!(edges contains (from,to))) {
      if (apply(from,to))
        throw GeneralError("cannot remove edge induced by transitive closure")
      else
        return
    }
    edges -= ((from,to))
    iterateBelow(from, true) {b =>
      iterateAbove(to, true) {a =>
        // b in below  -old path-> from -deleted edge-> to -old path-> a in above
        deletePaths(b,a, getNumPaths(b, from)*getNumPaths(to,a))
      }
    }
    //checkInvariant(s"deleting $from -> $to")
  }
  /** delete node x and paths involving x
    * @param into delete paths into x
    * @param outOf delete paths out of x
    * @param through delete paths strictly through x
    */
  def delete(x: T, into: Boolean, outOf: Boolean, through: Boolean): Unit = {
    if (through) {
      iterateBelow(x,false) {b =>
        iterateAbove(x,false) {a =>
          // b -old path-> x -old path-> a
          deletePaths(b,a,getNumPaths(b,x) * getNumPaths(x,a))
        }
      }
    }
    // paths into/out of x must be deleted after paths through x, because the former are needed for the latter
    if (outOf) {
      iterateAbove(x,false) {a =>
        edges -= ((x,a)) // only some of these edges exist
        pathsTo(a) -= x
        numPaths -= ((x,a))
      }
      pathsFrom -= x
    }
    if (into) {
      iterateBelow(x,false) {b =>
        edges -= ((b,x))
        pathsFrom(b) -= x
        numPaths -= ((b,x))
      }
      pathsTo -= x
    }
    //checkInvariant(s"deleting $x")
  }
  /** empty the underlying relation */
  def clear: Unit = {
    edges.clear
    pathsFrom.clear
    pathsTo.clear
    numPaths.clear
  }
}