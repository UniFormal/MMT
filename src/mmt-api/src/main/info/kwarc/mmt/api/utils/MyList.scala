package info.kwarc.mmt.api.utils

object SkipThis extends Throwable

/** Wrapper around List to provide some custom extensions */
case class MyList[A](l: List[A]) {
  /** returns the first non-None result of applying the argument function to elements of the list */
  def mapFind[B](f: A => Option[B]): Option[B] = l match { // DM: Isn't this just .collectFirst?
    case Nil => None
    case hd :: tl => f(hd) match {
      case Some(b) => Some(b)
      case None => MyList(tl).mapFind(f)
    }
  }

  /** like map but with a partial function; removes all results that are <code>None</code> */
  def mapPartial[B](f: A => Option[B]): List[B] = l.map(f).filter(_.isDefined).map(_.get) // DM: This is just .collect, but less efficient

  /** like map but with a partial function; returns a result only if no results are <code>None</code> */
  def mapPartialStrict[B](f: A => Option[B]): Option[List[B]] = {
    val bs = l map {a =>
       f(a) match {
         case Some(b) => b
         case None => return None
       }
    }
    Some(bs)
  }


  /** a map function in which SkipThis() can be called to skip an element */
  def mapOrSkip[B](f: A => B): List[B] = l flatMap {a => 
    try {List(f(a))}
    catch {case SkipThis => Nil}
  }
  
  /** values of l (in same order) that yield biggest result under f */
  def argMax[B](f: A => B)(implicit order: Ordering[B]): List[A] = {
    if (l.isEmpty) Nil else {
      val lF = l.map(f)
      val max = lF.max
      (l zip lF).filter(_._2 == max).map(_._1)
    }
  }
  
  def quotient[B](f: A => B): List[(B, List[A])] = {
    var in: List[(B, A)] = l.map(x => (f(x), x))
    var out: List[(B, List[A])] = Nil
    def add(e: (B, A), found: List[(B, List[A])], todo: List[(B, List[A])]): Unit = {
      todo match {
        case Nil => out ::=(e._1, List(e._2))
        case hd :: tl => if (hd._1 == e._1)
          out = found :::(hd._1, e._2 :: hd._2) :: tl
        else add(e, hd :: found, tl)
      }
    }
    in foreach {
      add(_, Nil, out)
    }
    out
  }

  /** do not print start and end for lists with a single element */
  def myMkString(start: String, sep: String, end: String): String =
    if (l.length == 1) l.mkString(sep) else l.mkString(start, sep, end)
}

object MyList {
  implicit def toList[A](m: MyList[A]): List[A] = m.l
}

/** adds convenience methods to a class that wraps around a list */
abstract class ListWrapper[A, W <: ListWrapper[A,W]](private[utils] val _as: List[A]) {
  /** this should return the companion object of the implementing case class */
  def companion: ListWrapperCompanion[A,W]
  def +(that: W):W = companion.apply(_as ::: that._as)
  def +(that: List[A]):W = this+companion.apply(that)
  def +(that: A*): W = this+that.toList
}

/** should be mixed into the companion object of a [[ListWrapper]] subclass */
trait ListWrapperCompanion[A, W <: ListWrapper[A,W]] {
  def apply(as: List[A]): W
  def apply(a: A*): W = apply(a.toList)
  val empty = apply()
  implicit def fromList(as: List[A]) = apply(as)
  implicit def toList(w: ListWrapper[A,W]) = w._as
  implicit def toMyList(w: ListWrapper[A,W]) = MyList(w._as)
}

sealed abstract class NestableList[+A] extends Iterable[A]
case class EmptyNestable[+A]() extends NestableList[A] {
  def iterator = Iterator.empty
}
case class ConsNestable[+A](hd: A, tl: NestableList[A]) extends NestableList[A] {
  def iterator = Iterator(hd) ++ tl.iterator
}
case class ConcatNestable[+A](left: NestableList[A], right: NestableList[A]) extends NestableList[A] {
  def iterator = left.iterator ++ right.iterator
}