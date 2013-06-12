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

*/

/**
 * InfInt: integers with positive and negative infinity
*/
sealed abstract class InfInt(s: String) extends scala.math.Ordered[InfInt] {

   /**
    * sum
    * satisfies x + (- x) = 0
    */
   def +(that : InfInt) : InfInt =
     (this, that) match {
        case (Infinite, NegInfinite) => Finite(0)
        case (NegInfinite, Infinite) => Finite(0)
        case (Infinite,_) => Infinite
        case (_, Infinite) => Infinite
        case (NegInfinite, _) => NegInfinite
        case (_,NegInfinite) => NegInfinite
        case (Finite(a), Finite(b)) => Finite(a+b)
      }
   def +(i: Int): InfInt = this + Finite(i)  
   /**
    * difference
    * satisfies x - x = 0
    */
   def -(that : InfInt) : InfInt =
      if (this == that) Finite(0)
      else (this, that) match {
        case (Infinite, _) => Infinite
        case (NegInfinite, _) => NegInfinite
        case (_, Infinite) => NegInfinite
        case (_, NegInfinite) => Infinite
        case (Finite(a), Finite(b)) => Finite(a-b)
      }
   def -(i: Int): InfInt = this - Finite(i)
   /**
    * true if (non-zero and) positive
    */
   def positive : Boolean = this > Finite(0)
   override def toString = s 
}
/** integer */
case class Finite(ones : Int) extends InfInt(ones.toString) {
   def compare(that: InfInt) = that match {
      case Finite(t) => ones - t
      case Infinite => -1
      case _ => 1
   }
}
/** positively infinite InfInt */
case object Infinite extends InfInt("infinity") {
  def compare(that: InfInt) = if (that == Infinite) 0 else 1
}
/** positively infinite InfInt */
case object NegInfinite extends InfInt("-infinity") {
  def compare(that: InfInt) = if (that == NegInfinite) 0 else -1
}
/** helper object for InfInts */
object InfInt {
   /** parses a InfInt from a string */
   def parse(s : String) : InfInt = s.trim match {
      case "infinity" => Infinite
      case "-infinity" => NegInfinite
      case i =>
         try {Finite(i.toInt)}
         catch {case _ : Throwable => throw ParseError("illegal InfInt: " + i)}
   }
}

/**
 * binding precedences of operators are represented
 * @param p the precedence, smaller precedence means weaker binding
 * @param loseTie tie-breaking flag for comparisons
*/
sealed case class Precedence(prec : InfInt, loseTie : Boolean) extends scala.math.Ordered[Precedence] {
   def weaken = Precedence(prec, true)
   /**
    * irreflexive comparison
    * satisfies: Precedence(p, true) < Precedence(p, false)
    */
   def compare(that: Precedence) =
      if (this == that) 0
      else if (prec < that.prec || (prec == that.prec && loseTie && ! that.loseTie)) -1
      else 1
   def +(i: Int) = Precedence(prec+i, loseTie)
   override def toString = prec.toString + (if (loseTie) "*" else "")
}
/** helper object for precedences */
object Precedence {
   /** shortcut for integer precedences */
   def integer(ones : Int) = Precedence(Finite(ones), false)
   /** shortcut for positively infinite precedence */
   val infinite = Precedence(Infinite, false)
   /** shortcut for negatively infinite precedence */
   val neginfinite = Precedence(NegInfinite, false)
   /**
    * parses a precedence from a string ([-]infinity | INT)[*]
    * if * is present, loseTie is true
    */
   def parse(s : String) : Precedence = {
      try {Precedence(InfInt.parse(s.replace("*","")), s.endsWith("*"))}
      catch {case _ : Throwable => throw ParseError("illegal precedence: " + s)}
   }
}


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