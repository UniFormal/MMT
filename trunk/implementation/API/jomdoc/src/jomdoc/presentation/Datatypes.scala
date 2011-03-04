package jomdoc.presentation
import jomdoc._
import jomdoc.utils._

/** The fixity of a notation determines the relative placement of operator and arguments. */
sealed abstract class Fixity
/** prefix: operator before arguments */
case object Pre extends Fixity
/** prefix: operator after arguments */
case object Post extends Fixity
/**
 * infix: operator after a certain number of arguments 
 * @param pos: the number of arguments before the operator
 */
case class  In(pos: Int) extends Fixity
/** interfix: operator duplicated in between all arguments */
case object Inter extends Fixity
/**
 * bindfix (a special fixity useful for binders):
 * the operator has a hole that is filled with the arguments except for the last one, which is appened
 */
case object Bind extends Fixity

/** The application style of a prefix of postfix notation determines how operator and arguments are connected. */
sealed abstract class AppStyle
/** Mathematical application: the arguments are bracketed as in f(a1,...,an) (here with ``,'' as the separator). */
case object Math extends AppStyle
/** Lambda calculus application: a separator is placed between operator and arguments
 *  as in f a1 ... an (here with `` '' as the separator). */
case object LC extends AppStyle

/** The associativity of an infix or interfix notation determines how brackets are restored. */
sealed abstract class Associativity
/** associative, i.e., no restoring of brackets */
case object AssocNone extends Associativity
/** left-associative, i.e., restoring of brackets from the left */
case object Left extends Associativity
/** right-associative, i.e., restoring of brackets from the right */
case object Right extends Associativity

/**
 * InfInt: integers with positive and negative infinity
 * satisfies x - x = 0
*/
sealed abstract class InfInt {
   def -(that : InfInt) : InfInt =
      if (this == that) Finite(0)
      else (this, that) match {
        case (Infinite, _) => Infinite
        case (NegInfinite, _) => NegInfinite
        case (_, Infinite) => NegInfinite
        case (_, NegInfinite) => Infinite
        case (Finite(a), Finite(b)) => Finite(a-b)
      }
}
/** integer */
case class Finite(ones : Int) extends InfInt
/** positively infinite InfInt */
case object Infinite extends InfInt  
/** positively infinite InfInt */
case object NegInfinite extends InfInt
/** helper object for InfInts */
object InfInt {
   /** parses a InfInt from a string */
   def parse(s : String) : InfInt = s match {
      case "infinity" => Infinite
      case "-infinity" => NegInfinite
      case _ =>
         try {Finite(s.toInt)}
         catch {case _ => throw ParseError("illegal InfInt: " + s)}
   }
}

/**
 * binding precedences of operators are represented
 * @param p the precedence, smaller precedence means weaker binding
 * @param loseTie tie-breaking flag, Precedence(p, true) < Precedence(p, false)
*/
sealed case class Precedence(prec : InfInt, loseTie : Boolean) {
   def weaken = Precedence(prec, true)
}
/** helper object for precedences */
object Precedence {
   def integer(ones : Int) = Precedence(Finite(ones), false)
   def infinite = Precedence(Infinite, false)
   def neginfinite = Precedence(NegInfinite, false)
   /** parses a precedence from a string ([-]infinity | INT)[*] */
   def parse(s : String) : Precedence = {
      try {Precedence(InfInt.parse(s.replace("*","")), s.endsWith("*"))}
      catch {case _ => throw ParseError("illegal precedence: " + s)}
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
      catch {case _ => throw ParseError("illegal elidability: " + s)}
   }
}