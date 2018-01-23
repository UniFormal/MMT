package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._

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
         catch {case _ : Exception => throw ParseError("illegal InfInt: " + i)}
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
      catch {case _ : Exception => throw ParseError("illegal precedence: " + s)}
   }
}
