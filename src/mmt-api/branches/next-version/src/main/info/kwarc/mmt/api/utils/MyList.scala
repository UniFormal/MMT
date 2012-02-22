package info.kwarc.mmt.api.utils
import info.kwarc.mmt.api.utils.MyList.fromList

/** Wrapper around List to provide some custom extensions */
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
      
   def quotient[B](f : A => B) : List[(B,List[A])] = {
      var in : List[(B, A)] = l.map(x => (f(x), x))      
      var out : List[(B, List[A])] = Nil
      def add(e : (B,A), found: List[(B, List[A])], todo: List[(B, List[A])]) {todo match {
        case Nil => out = (e._1,List(e._2)) :: found
        case hd :: tl => if (hd._1 == e._1)
           out = found ::: (hd._1, e._2 :: hd._2) :: todo
        else add(e, hd :: found, tl) 
      }}
      in foreach {add(_, Nil, out)}
      out
   }
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

class LList[A](private var _first: LListElem[A], private var _last: LListElem[A]) {
   def copy : LList[A] = {
      val l = LList[A]()
      foreach {a => l := a}
      l
   }
   def slice(from: LListElem[A], to: LListElem[A]) : LList[A] = {
      val fst = new LListElem[A](from.here, null, from.next)
      val lst = new LListElem[A](to.here, to.prev, null)
      new LList[A](fst, lst)
   }
   /** e =: l prepends e to l, l is changed */
   def =:(a: A) {
      val e = new LListElem(a, null, _first)
      if (empty) {
         _first = e
         _last = e
      } else {
         _first.prev = e
         _first = e
      }
   }
   /** m =: l prepends m to l, l is changed but not m
    * linear time in size of m: list structure of m is copied but not the elements of m
    */
   def =:(that: LList[A]) {
      var e : LListElem[A] = that._last
      while (e != null) {
         e.here =: this
         e = e.prev
      }
   }
   /** l := e appends e to l, l is changed */
   def :=(a: A) {
      val e = new LListElem(a, _last, null)
      if (empty) {
         _first = e
         _last = e
      } else {
         _last.next = e
         _last = e
      }
   }
   /** l := m appends m to l, l is changed but not m
    * linear time in size of m: list structure of m is copied but not the elements of m
    */
   def :=(that: LList[A]) {
      var e : LListElem[A] = that._first
      while (e != null) {
         this := e.here
         e = e.next
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
   def head : A = if (empty) throw LList.Empty else _first.here
   def last : A = if (empty) throw LList.Empty else _last.here
   /** removes and returns the head of this list; constant time */
   def popHead : A = {
      if (empty) throw LList.Empty
      val a = _first.here
      if (_first.next == null) {
         _first = null
         _last = null
      } else {
         _first.next.prev = null
      }
      a
   }
   /** removes and returns the last element of this list; constant time */
   def popLast : A = {
      if (empty) throw LList.Empty
      val a = _last.here
      if (_last.prev == null) {
         _first = null
         _last = null
      } else {
         _last.prev.next = null
      }
      a
   }
   def foreach(f: A => Unit) {
      var e = _first
      while (e != null) {
         f(e.here)
         e = e.next
      }
   }
   def reverseCopy : LList[A] = {
      val l = LList[A]()
      foreach {a => a =: l}
      l
   }
   def reverseForeach(f: A => Unit) {
      var e = _last
      while (e != null) {
         f(e.here)
         e = e.prev
      }
   }
   def toList : List[A] = {
      var l : List[A] = Nil
      reverseForeach {a => l = a :: l}
      l
   }
   override def toString = toList.mkString("[",",","]")
}

object LList {
   def apply[A]() : LList[A] = new LList[A](null, null)
   def apply[A](as: A*) : LList[A] = {
      val l = apply[A]()
      as foreach {a => l := a}
      l
   }
   case object Empty extends java.lang.Throwable
}