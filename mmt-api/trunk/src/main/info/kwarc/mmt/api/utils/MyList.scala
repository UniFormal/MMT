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
}