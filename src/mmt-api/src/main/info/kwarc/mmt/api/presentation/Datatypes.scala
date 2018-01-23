package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._

/* superseded by parser.Fixity
/** The fixity of a notation determines the relative placement of operator and arguments. */
sealed abstract class Fixity(s: String) {
   override def toString = s
}
/** prefix: operator before arguments */
case object Pre extends Fixity("pre")
/** prefix: operator after arguments */
case object Post extends Fixity("post")
/**
 * infix: operator after a certain number of arguments
 * @param pos: the number of arguments before the operator
 */
case class  In(pos: Int) extends Fixity(if (pos == 1) "in" else pos.toString)
/** interfix: operator duplicated in between all arguments */
case object Inter extends Fixity("inter")
/**
 * a special fixity useful for binders:
 * the operator has a hole that is filled with the arguments except for the last one, which is appended
 */
case object Bind extends Fixity("bind")

case object Tree extends Fixity("tree")

/** The application style of a prefix of postfix notation determines how operator and arguments are connected. */
sealed abstract class AppStyle(s: String) {
   override def toString = s
}
/** Mathematical application: the arguments are bracketed as in f(a1,...,an) (here with ``,'' as the separator). */
case object Math extends AppStyle("math")
/** Lambda calculus application: a separator is placed between operator and arguments
 *  as in f a1 ... an (here with `` '' as the separator). */
case object LC extends AppStyle("lc")

/** The associativity of an infix or interfix notation determines how brackets are restored. */
sealed abstract class Associativity(s: String) {
   override def toString = s
}
/** associative, i.e., no restoring of brackets */
case object AssocNone extends Associativity("none")
/** left-associative, i.e., restoring of brackets from the left */
case object Left extends Associativity("left")
/** right-associative, i.e., restoring of brackets from the right */
case object Right extends Associativity("right")


/**
 * Elidability is modelled by positive numbers. (Other values mean objects are required.)
 */
sealed case class Elidability(level : Int) {
   /** true if positive elidability */
   def elidable : Boolean = level > 0
}

/** helper object for elidabilities */
object Elidability {
   val None = Elidability(0)
   /** parses an elidability from a string */
   def parse(s : String) : Elidability = {
      try {Elidability(s.toInt)}
      catch {case _ : Throwable => throw ParseError("illegal elidability: " + s)}
   }
}

*/
