package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import objects._
import utils._

import scala.collection.mutable._

/**
 * a container for relations like ==, !=, and <= on terms which are indexed by shape)
 *
 * the class approximates the relations based on the available knowledge
 * initially nothing is known, and instances are added over time
 *
 * knowledge grows monotonously and cannot be removed
 * inconsistency can arise (which can, e.g., be used to terminate a proof)
 *
 * the class implements
 * - reflexivity, symmetry, and (cached) transitivity of ==
 * - irreflexivity and symmetry of !=
 * - reflexivity, anti-symmetry wrt ==, and (not cached) transitivity of <=
 * - congruence of <= wrt ==
 * - disjointness of == and != (used to detect inconsistency)
 */
class EquivalenceRelation(levels: Int) {
   private var classes = new HashSet[EquivalenceClass]
   private var _inconsistent = false

   /** true if the available knowledge is inconsistent */
   def inconsistent = _inconsistent

   /** creates a new class and adds it */
   private def addClass(ts: Term*) = {
     val c = new EquivalenceClass(levels)
     ts.foreach {t => c += t}
     classes += c
     c
   }

   /** merges two classes */
   private def merge(c: EquivalenceClass, d: EquivalenceClass, cBelowD: Boolean): Unit = {
     val betw = if (cBelowD || (c below d)) {
       c between d
     } else if (d below c) {
       d between c
     } else
       Nil
     val toMerge = d :: betw
     toMerge foreach {m =>
        c add m
        classes -= m
     }
     classes.foreach {e => e.merged(c, toMerge)}
   }

   /** the equivalence class of s (if any) */
   def getClass(s: Term) = classes find (_ contains s)
   /** the equivalence class of s (possibly a singleton) */
   def getOrAddClass(t: Term) = getClass(t).getOrElse(addClass(t))

   /** records the knowledge s == t */
   def equivalent(s: Term, t: Term): Unit = {
     if (s hasheq t) return
     (getClass(s), getClass(t)) match {
       case (Some(c), Some(d)) =>
         if (c != d) {
           if (c disjointFrom d) {
             _inconsistent = true
           } else {
             c add d
             classes -= d
           }
         }
       case (Some(c), None) =>
         c += t
       case (None, Some(c)) =>
         c += s
       case (None, None) =>
         addClass(s,t)
     }
   }

   /** records the knowledge s != t */
   def notEquivalent(s: Term, t: Term): Unit = {
     val c = getOrAddClass(s)
     val d = getOrAddClass(t)
     if (c == d) {
        _inconsistent = true
     } else {
        c mustBeDisjointFrom d
        d mustBeDisjointFrom c
     }
   }

   /** records the knowledge s <= t */
   def belowOrEqual(s: Term, t: Term): Unit = {
     val c = getOrAddClass(s)
     val d = getOrAddClass(t)
     if (c below d) return
     if (d below c) {
       merge(d,c, true)
     } else {
       c mustBeBelow d
     }
   }

   /** records the knowledge s < t */
   def strictlyBelow(s: Term, t: Term): Unit = {
      belowOrEqual(s,t)
      notEquivalent(s,t)
   }

   /** checks if s == t or s != t is known; possibly inconclusive */
   def checkEquivalent(s: Term, t: Term): Option[Boolean] = {
     if (s hasheq t) return Some(true)
     (getClass(s), getClass(t)) match {
       case (Some(c), Some(d)) =>
         if (c == d)
           Some(true)
         else if (c disjointFrom d)
           Some(false)
         else
           None
       case _ => None
     }
   }

   /** checks if s <= t is known to be true or false; possibly inconclusive */
   def checkBelowOrEqual(s: Term, t: Term): Option[Boolean] = {
     if (s hasheq t) return Some(true)
     (getClass(s), getClass(t)) match {
       case (Some(c), Some(d)) =>
         if (c below d)
           Some(true)
         else if ((c disjointFrom d) && (d below c))
           Some(false)
         else
           None
       case _ => None
     }
   }

   /** checks if s < t is known to be true or false; possibly inconclusive */
   def checkStrictlyBelow(s: Term, t: Term): Option[Boolean] = {
     val eq = checkEquivalent(s,t)
     val be = checkBelowOrEqual(s,t)
     (eq,be) match {
       case (Some(false), Some(true)) => Some(true)
       case (Some(true), _) => Some(false)
       case (_, Some(false)) => Some(false)
       case _ => None
     }
   }
}

/** class in an [[EquivalenceRelation]] */
class EquivalenceClass(levels: Int) extends ShapeIndexedSet[Term](levels, x => x) {
  /** classes that must be disjoint from this one */
  private var _disjointFrom: List[EquivalenceClass] = Nil
  /** true if two classes must be disjoint */
  def disjointFrom(c: EquivalenceClass) = _disjointFrom contains c
  /** records the knowledge that two classes must be disjoint */
  def mustBeDisjointFrom(c: EquivalenceClass): Unit = {_disjointFrom ::= c}

  /** classes that must be directly above this from this one */
  private var _below: List[EquivalenceClass] = Nil
  /** true if this is below that */
  def below(that: EquivalenceClass): Boolean = this == that || _below.exists(b => b below this)
  /** records the knowledge that two classes must be disjoint */
  def mustBeBelow(c: EquivalenceClass): Unit = {_below ::= c}
  /** all classes between this and that */
  def between(that: EquivalenceClass): List[EquivalenceClass] = {
    var res: List[EquivalenceClass] = Nil
    def aux(seen: List[EquivalenceClass], next: EquivalenceClass): Unit = {
      if (next == that) {
        res :::= seen
      } else {
        next._below foreach {b => aux(next::seen, b)}
      }
    }
    res.distinct
  }

  /** merges another class into this one */
  def add(c: EquivalenceClass): Unit = {
     c.foreach {x => this += x}
     _disjointFrom = (_disjointFrom ::: c._disjointFrom).distinct
     _below = (_below ::: c._below).distinct
  }
  /** replaces any reference to a d in ds with c */
  def merged(c: EquivalenceClass, ds: List[EquivalenceClass]): Unit = {
    def needsC(l: List[EquivalenceClass]) = !l.contains(c) && !utils.disjoint(l,ds)
    if (needsC(_disjointFrom))
      _disjointFrom ::= c
    _disjointFrom = _disjointFrom diff ds
    if (needsC(_below))
      _below ::= c
    _below = _below diff ds
  }
}
