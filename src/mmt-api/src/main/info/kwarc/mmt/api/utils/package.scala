package info.kwarc.mmt.api

import utils._

import scala.collection.generic.CanBuildFrom

/**
  * This package defines various MMT-independent high-level APIs.
  * Various basic functions are declared directly in this package object in order to be easily available.
  * 
  * Most other files in this package are self-contained and independent of the rest of MMT and each other.
  * We describe them in groups below.
  *
  * =Data structures*
  * 
  * - [[Union]] disjoint union of Scala types
  * - [[MyList]] extensions of Scala's lists (via implicit conversions)
  * - [[HashRelation]], [[HashMapToSet]], and [[HashEquality]]: hash-related data structures for
  * - [[While]] while loops that allow for break and continue
  * 
  * =General purpose utilities=
  * 
  * - [[Killable]] tasks that can be notified that they should be canceled. In particular, [[MMTTask]]s can be aborted without risking an inconsistent state.
  * - [[Unparsed]] for simple parsing of strings
  * - [[ScalaTo]] serialization helpers for Scala objects
  * - [[ValueCache]] factory methods that introduce structure sharing by resuing previous pointers
  * - [[XMLToScala]] framework for conveniently turning a set of case classes into an XML parser for the corresponding schema  
  * 
  * =Wrappers for low-level APIs=
  * 
  * MMT provides various APIs that extend or simplify APIs provided Java or Scala:
  * - [[File]] file paths and interacting with files
  * - [[URI]] URIs
  * - [[xml]] various helper function for working with XML and dereferencing URLs (not really a wrapper but fits best here)
  * - [[ShellCommand]] commands executed on the system shell
  * 
  * =APIs for external languages=
  * 
  * - [[Dot]] the dot languages for graph layouting
  * - [[JSON]] the JSON language
  * - [[HTML]] API for building HTML pages programmatically
  * 
  */
package object utils {
   /** converts a string to an integer, returns None on format exception */
   def stringToInt(s: String) = try {Some(s.toInt)} catch {case _: Exception => None}
   /** matches a string as an integer */
   object NumericString {
     def unapply(s: String) = stringToInt(s)
   }

   /** splits a string at a separator (returns Nil for the empty string) */
   def stringToList(s: String, sep: String = "\\s") = s.split(sep).toList match {
      case List("") => Nil
      case l => l
   }

   /** string index modulo string length */
   def moduloLength(s:String, i: Int) = {
     if (i >= 0) i else i+s.length
   }
   def charAt(s: String, i: Int) = {
     val iA = moduloLength(s, i)
     if (iA < s.length) Some(s(i)) else None
   }
   /** substring of a string given by begin and end, negative indices allowed */
   def substringFromTo(s: String, from: Int, to: Int) = {
     s.substring(moduloLength(s,from), moduloLength(s,to))
   }
   /** substring of a string given by begin and length, negative index allowed */
   def substringFrom(s: String, from: Int, length: Int) = substringFromTo(s, from, from+length)

   /** splits a string at whitespace, quoted segments may contain whitespace, \" for quote, ignores leading/trailing whitespace */
   def splitAtWhitespace(s: String): List[String] = {
      var segments : List[String] = Nil
      var current = ""
      var inquoted = false
      var todo = s
      def done: Unit = {
        if (current.nonEmpty) {
          segments ::= current
          current = ""
        }
      }
      while (todo.nonEmpty) {
        if (todo.startsWith("\\\"")) {
          current += '"'
          todo = todo.substring(2)
        } else {
          val c = todo(0)
          todo  = todo.substring(1)
          if (inquoted) {
            if (c == '"') {
              done
              inquoted = false
            } else {
              current += c
            }
          } else {
            if (c == '"') {
              inquoted = true
            } else if (c.isWhitespace) {
              done
            } else {
              current += c
            }
          }
        }
      }
      done
      segments.reverse
   }

  /** a pair of an open and a closing bracket */
  case class Bracket(begin: String, end: String) {
    def wrapAround(s: String) = begin + s + end
    val beginL = begin.length
    val endL = end.length
  }

   /** implicit conversion into lists with extra functionality */
   implicit def fromList[A](l: List[A]): MyList[A] = new MyList[A](l)
  
   /** turns a list into a string by inserting a separator */
   def listToString[A](l: Iterable[A], sep: String) = l.map(_.toString).mkString(sep)

   /** repeats a strings a number of times, optionally with a separator */
   def repeatString(s: String, n: Int, sep: String = "") = Range(0,n).map(_ => s).mkString(sep)

   /** inserts a separator element in between all elements of a list */
   def insertSep[A](l: List[A], sep: A): List[A] = if (l.isEmpty) Nil else l.flatMap(a => List(sep,a)).tail

   /** applies a list of pairs seen as a map */
   def listmap[A,B](l: Iterable[(A,B)], a: A): Option[B] = l.find(_._1 == a).map(_._2)
   /** applies a list of pairs seen as the inverse of a map */
   def invlistmap[A,B](l: Iterable[(A,B)], b: B): Option[A] = l.find(_._2 == b).map(_._1)
   
   /** like map, but the map function knows what previous values produced */
   def mapInContext[A,B](l: Iterable[A])(f: (List[(A,B)],A) => B) : List[B] = {
     var sofar: List[(A,B)] = Nil
     l foreach {a =>
       sofar = sofar ::: List((a, f(sofar, a)))
     }
     sofar.map(_._2)
   }

   /** disjointness of two lists (fast if first argument is empty) */
   def disjoint[A](l: Seq[A], m: Seq[A]) = l.forall(a => ! m.contains(a))
   /** subset property of two lists (seen as sets) (fast if first argument is empty) */
   def subset[A](l: Seq[A], m: Seq[A]) = l.forall(a => m.contains(a))
   /** intersection of two lists */
   def inter[A](l: Seq[A], m: Seq[A]) = l.filter(a => m.contains(a))
   /** difference of two lists */
   def diff[A](l: Seq[A], m: Seq[A]) = l.filter(a => !m.contains(a))

   /** variant of fold such that associate(List(a), unit)(comp) = a instead of comp(unit, a) */
   def associate[A](l: List[A], unit: A)(comp: (A,A) => A): A = l match {
     case Nil => unit
     case hd::tl => tl.fold(hd)(comp)
   }

   /** checks if a list has duplicates */
   def hasDuplicates[A](l: Iterable[A]): Boolean = {
     val seen = new scala.collection.mutable.HashSet[A]
     l.foreach {a =>
       if (seen contains a)
         return true
       else
         seen += a
     }
     false
   }

   /** slurps an entire stream into a string */
   def readFullStream(is: java.io.InputStream) = scala.io.Source.fromInputStream(is, "UTF-8").mkString

   /** a cast function that allows only casting into a subtype and returns None if the cast fails */
   def downcast[A, B<:A](cls: Class[B])(a: A): Option[B] = a match {
     case b: B@unchecked if cls.isInstance(b) => Some(b)
     case _ => None
   }

   /** returns a histogram of pf over lst, i.e. counts how often each return value occurs */
   def histogram[T1, T2](lst: Seq[T1], pf: PartialFunction[T1, T2]): Seq[(T2, Int)] = {
     lst.groupBy(pf.lift).collect({
       case (Some(e), l) => (e, l.size)
     }).toSeq
   }

   /** calls a list of functions in order and finds the first defined one or None */
   def firstDefined[T](alternatives: (Unit => Option[T])*): Option[T] = {
     alternatives.view.map(x => x(())).find(_.isDefined).flatten
   }
}
