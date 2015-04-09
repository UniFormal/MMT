package info.kwarc.mmt.api.utils
import info.kwarc.mmt.api.utils.MyList.fromList

/** Wrapper around List to provide some custom extensions */
class MyList[A](val l : List[A]) {
   /** returns the first non-None result of applying the argument function to elements of the list */
   def mapFind[B](f : A => Option[B]) : Option[B] = l match {
       case Nil => None
       case hd :: tl => f(hd) match {case Some(b) => Some(b) case None => tl.mapFind(f)}
   }
   /** like map but with a partial function; removes all results that are <code>None</code> */
   def mapPartial[B](f : A => Option[B]) : List[B] = l.map(f).filter(_.isDefined).map(_.get)
   
   def quotient[B](f : A => B) : List[(B,List[A])] = {
      var in : List[(B, A)] = l.map(x => (f(x), x))      
      var out : List[(B, List[A])] = Nil
      def add(e : (B,A), found: List[(B, List[A])], todo: List[(B, List[A])]) {todo match {
        case Nil => out ::= (e._1,List(e._2))
        case hd :: tl => if (hd._1 == e._1)
           out = found ::: (hd._1, e._2 :: hd._2) :: tl
        else add(e, hd :: found, tl) 
      }}
      in foreach {add(_, Nil, out)}
      out
   }
}

object MyList {
   implicit def fromList[A](l : List[A]) : MyList[A] = new MyList[A](l)
   implicit def toList[A](m: MyList[A]) : List[A] = m.l
}