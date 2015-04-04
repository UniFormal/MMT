package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import parser._

/**
 * SemanticType's are closed under subtypes and quotients by using valid and normalform.
 * 
 * In other words, the triple (univ, valid, normalform) forms a partial equivalence relation on a Scala type.
 */
abstract class SemanticType {
   /**
    * the semantic type, which containing the value realizing terms of the syntactic type
    */
   type univ
   /** the predicate on the semantic type used to obtain subtypes
    *  
    *  true by default
    */
   def valid(u: univ): Boolean = true
   /** the equivalence relation (given as a normal form conversion) on the semantic type used to obtain quotient types
    *  
    *  identity by default
    */
   def normalform(u: univ): univ = u
   /** converts strings into values */
   def fromString(s: String) : univ
   /** converts values into strings */
   def toString(u: univ) : String = u.toString
   /** convenience method to construct a lexer for this type */
   def quotedLiteral(key: String) = Some(new AsymmetricEscapeLexer(key+"\"", "\""))
   /** convenience method to construct a lexer for this type */
   def escapedLiteral(begin: String, end: String) = Some(new AsymmetricEscapeLexer(begin, end))
   /** @return a LexerExtension that is to be used when this type is in Scope */
   def lex: Option[parser.LexFunction] = None
}

class Product(val left: SemanticType, val right: SemanticType) extends SemanticType {
   private val matcher = new utils.StringMatcher2("(",",",")")
   type univ = (left.univ, right.univ)
   override def valid(u: univ) = left.valid(u._1) && right.valid(u._2)
   override def normalform(u: univ) = (left.normalform(u._1), right.normalform(u._2))
   def fromString(s: String) = {
      val (s1,s2) = matcher.unapply(s).get
      (left.fromString(s1), right.fromString(s2))
   }
   override def toString(u: univ) = matcher(left.toString(u._1), right.toString(u._2))
}

object Subtype {
   def apply(of: SemanticType)(by: of.univ => Boolean) = new SemanticType {
      type univ = of.univ
      override def valid(u: of.univ) = of.valid(u) && by(u)
      override def normalform(u: of.univ) = of.normalform(u)
      def fromString(s: String) = of.fromString(s)
   }
}

object Quotient {
   def apply(of: SemanticType)(by: of.univ => of.univ) = new SemanticType {
      type univ = of.univ
      override def valid(u: of.univ) = of.valid(u)
      override def normalform(u: of.univ) = by(of.normalform(u))
      def fromString(s: String) = of.fromString(s)
   }
}
/** bundles functions that are typically used when defining literals based on integers */
trait IntegerLiteral extends SemanticType {
   type univ = BigInt
   def fromString(s: String) = s.toInt
   override def lex = Some(new parser.NumberLiteralLexer(false))
}

/** standard natural numbers
 *  
 *  synType must still be set as desired after creating an instance
 */
class StandardInt extends IntegerLiteral

/** standard natural numbers
 *  
 *  synType must still be set as desired after creating an instance
 */
class StandardNat extends StandardInt {
   override def valid(u: BigInt) = u >= 0
}

/** standard integers modulo, i.e., a finite type of size modulus
 *  
 *  synType must still be set as desired after creating an instance
 */
class IntModulo(modulus: Int) extends IntegerLiteral {
   override def normalform(u: BigInt) = u mod modulus
}

/** standard rational numbers
 *  
 *  synType must still be set as desired after creating an instance
 */
class StandardRat extends Product(new StandardNat, new StandardNat) {
   type univS = (BigInt,BigInt) // equal to this.univ but Scala does not know that
   override def valid(u: univ) = u._2 != 0
   override def normalform(u: univ) = {
      val uS = u.asInstanceOf[univS] 
      val gcd = uS._1 gcd uS._2
      (uS._1 / gcd, uS._2 / gcd).asInstanceOf[univ]
   }
}

/** rational complex numbers
 *  
 *  synType must still be set as desired after creating an instance
 */
class ComplexRat extends Product(new StandardRat, new StandardRat) {
}
