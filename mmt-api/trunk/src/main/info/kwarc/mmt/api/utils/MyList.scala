package info.kwarc.mmt.api.utils
import info.kwarc.mmt.api.utils.MyList.fromList

class MyList[A](l : List[A]) {
   implicit def toList : List[A] = l
   /** folding for non-associative folding functions, e.g. Nil => start; List(a) => a; List(a,b) => map(a,b), ... */
   def myFold[B >: A](start : B)(map : (B,A) => B) : B =
      if (l.isEmpty) start
      else l.reduceLeft(map)
   
   /** returns the first non-None result of applying the argument function to elements of the list */
   def mapFind[B](f : A => Option[B]) : Option[B] = l match {
       case Nil => None
       case hd :: tl => f(hd) match {case Some(b) => Some(b) case None => tl.mapFind(f)}
   }
   //def init = new MyList(l.init)
   //def last = l.last
   /** like map but with a partial function; removes all results that are <code>None</code> */
   def mapPartial[B](f : A => Option[B]) : List[B] = l.map(f).filter(_.isDefined).map(_.get)
   
   def  #:(a : A) = l.exists(_ == a)
   def !#:(a : A) = ! l.exists(_ == a)
}

object MyList {
   implicit def fromList[A](l : List[A]) : MyList[A] = new MyList[A](l)
   def fromString(s: String, sep: String) = s.split(sep).toList match {
      case List("") => Nil
      case l => l
   }
}

class LListElem[A](var here : A, var prev: LListElem[A], var next: LListElem[A]) {
   def this(here: A) = this(here, null, null)
}

class LList[A] {
   var _first: LListElem[A] = null
   var _last: LListElem[A] = null
   /** e =: l prepends e to l, l is changed */
   def =:(a: A) {
      val e = new LListElem(a, null, _first)
      _first = e
   }
   /** m =: l prepends m to l, l is changed but not m
    * linear time in size of m: list structure of m is copied but not the elements of m
    */
   def =:(that: LList[A]) {
      if (that.nonempty) {
         that.last =: this
         that.init =: this
      }
   }
   /** l := e appends e to l, l is changed */
   def :=(a: A) {
      val e = new LListElem(a, _last, null)
      _last = e
   }
   /** l := m appends m to l, l is changed but not m
    * linear time in size of m: list structure of m is copied but not the elements of m
    */
   def :=(that: LList[A]) {
      if (that.nonempty) {
         this := that.head
         this := that.tail
      }
   }
   /** l :=: m concatenates l and m, both are changed
    * constant time
    */
   def :=:(that: LList[A]) {
      if (that.empty) {
         that._first = this._first
         that._last = this._last
      }
      else if (this.empty) {
         this._first = that._first
         this._last = that._last
      }
      else {
         that._last.next = this._first
         this._first.prev = that._last
      }
   }
   def empty : Boolean = _first == null
   def nonempty : Boolean = _first != null
   def head : A        = if (empty) throw LList.Empty else _first.here
   def last : A        = if (empty) throw LList.Empty else _last.here
   /** constant time, changes to tail affect original list */
   def tail : LList[A] = if (empty) throw LList.Empty else {
      LList.fromto(_first.next, _last)
   }
   /** constant time, changes to init affect original list */
   def init : LList[A] = if (empty) throw LList.Empty else {
      LList.fromto(_first, _last.prev)
   }
   
   def toList : List[A] = if (empty) Nil else head :: tail.toList
}

object LList {
   def apply[A]() : LList[A] = new LList[A]
   def apply[A](a: A) : LList[A] = {
      val l = new LList[A]
      a =: l
      l
   }
   def fromto[A](from: LListElem[A], to: LListElem[A]) : LList[A] = {
      val l = new LList[A]()
      l._first = new LListElem[A](from.here, null, from.next)
      l._last  = new LListElem[A](to.here, to.prev, null)
      l
   }
   case object Empty extends java.lang.Throwable
}