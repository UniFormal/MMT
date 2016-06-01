package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import parser._

/**
 * SemanticType's are closed under subtypes and quotients by using valid and normalform.
 * 
 * In other words, the triple (U, valid, normalform) forms a partial equivalence relation on a Scala type.
 * 
 * For technical reasons, U is fixed as the type [[Any]].
 */
abstract class SemanticType {
   /** the predicate on the semantic type used to obtain subtypes
    *  
    *  true by default
    */
   def valid(u: Any): Boolean = true
   /** the equivalence relation (given as a normal form conversion) on the semantic type used to obtain quotient types
    *  
    *  pre: valid(u) == true
    *  
    *  identity by default
    */
   def normalform(u: Any): Any = u
   /** converts strings into values; pre: s == toString(u) for some u with valid(u) == true */
   def fromString(s: String) : Any
   /** converts values into strings; pre: valid(u) == true */
   def toString(u: Any) : String
   /** convenience method to construct a lexer for this type */
   def quotedLiteral(key: String) = Some(new AsymmetricEscapeLexer(key+"\"", "\""))
   /** convenience method to construct a lexer for this type */
   def escapedLiteral(begin: String, end: String) = Some(new AsymmetricEscapeLexer(begin, end))
   /** @return a LexerExtension that is to be used when this type is in scope */
   def lex: Option[parser.LexFunction] = None
}

object Product {
   val matcher = new utils.StringMatcher2("(",",",")")
}
class Product(val left: SemanticType, val right: SemanticType) extends SemanticType {
   override def valid(u: Any) = u match {
      case (l,r) => left.valid(l) && right.valid(r)
      case _ => false
   }
   override def normalform(u: Any) = u match {
      case (l,r) => (left.normalform(l), right.normalform(r))
   }
   def fromString(s: String) = {
      val (s1,s2) = Product.matcher.unapply(s).get
      (left.fromString(s1), right.fromString(s2))
   }
   def toString(u: Any) = u match {
      case (l,r) => Product.matcher(left.toString(l), right.toString(r))
   }
}

object ListType {
   val matcher = new utils.StringMatcher2Sep("[",",","]")
}
class ListType(val over: SemanticType) extends SemanticType {
   override def valid(u: Any) = u match {
      case us: List[_] => us.forall(over.valid)
      case _ => false
   }
   override def normalform(u: Any) = u match {
      case us: List[_] => us map over.normalform
   }
   def fromString(s: String) = {
      val us = ListType.matcher.unapply(s).get
      us map over.fromString
   }
   def toString(u: Any) = u match {
      case us: List[_] => ListType.matcher(us map over.toString)
   }
}

object TupleType {
   val matcher = new utils.StringMatcher2Sep("(",",",")")
}
class TupleType(val over: SemanticType, val dim: Int) extends SemanticType {
   override def valid(u: Any) = u match {
      case us: List[_] if us.length == dim => us.forall(over.valid)
      case _ => false
   }
   override def normalform(u: Any) = u match {
      case us: List[_] => us map over.normalform
   }
   def fromString(s: String) = {
      val us = TupleType.matcher.unapply(s).get
      us map over.fromString
   }
   def toString(u: Any) = u match {
      case us: List[_] => TupleType.matcher(us map over.toString)
   }
}

abstract class Subtype(val of: SemanticType) extends SemanticType {
   def by(u: Any): Boolean
   override def valid(u: Any) = of.valid(u) && by(u)
   override def normalform(u: Any) = of.normalform(u)
   def fromString(s: String) = of.fromString(s)
   def toString(u: Any) = of.toString(u)
   override def lex = of.lex
}

abstract class Quotient(val of: SemanticType) extends SemanticType {
   def by(u: Any): Any
   override def valid(u: Any) = of.valid(u)
   override def normalform(u: Any) = by(of.normalform(u))
   def fromString(s: String) = by(of.fromString(s))
   def toString(u: Any) = of.toString(by(u))
   override def lex = of.lex
}

trait RepresentationType[V] {
   val cls: Class[V]
   def unapply(u: Any): Option[V] = u match {
      case v: V@unchecked if cls.isInstance(v) =>
         Some(v)
      case v: V@unchecked if cls.isPrimitive && cls == u.getClass =>
         Some(v) // isInstance is always false for primitive classes
      case _ => None
   }
}

abstract class Atomic[V] extends SemanticType with RepresentationType[V] {
   override def valid(u: Any) = unapply(u).isDefined
   def toString(u: Any) = unapply(u).get.toString
   /** narrower type */
   def fromString(s: String): V
}

/** bundles functions that are typically used when defining literals based on integers */
abstract class IntegerLiteral extends Atomic[BigInt] {
   val cls = classOf[BigInt]
   def fromString(s: String) = BigInt(s)
   override def lex = Some(new parser.NumberLiteralLexer(false,false))
}

/** standard integer numbers */
object StandardInt extends IntegerLiteral

/** standard natural numbers */
object StandardNat extends Subtype(StandardInt) with RepresentationType[BigInt] {
   val cls = StandardInt.cls
   def by(u: Any) = StandardInt.unapply(u).get >= 0
}

/** standard integers modulo, i.e., a finite type of size modulus */
class IntModulo(modulus: Int) extends Quotient(StandardInt) {
   def by(u: Any) = StandardInt.unapply(u).get mod modulus
}

/** standard rational numbers */
object StandardRat extends Quotient(new Product(StandardInt,StandardNat)) {
   def by(u: Any) = {
      val (e:BigInt,d:BigInt) = u
      val gcd = e gcd d
      (e / gcd, d / gcd)
   }
   override def toString(u: Any) = {
      val (e:BigInt,d:BigInt) = u
      if (d == 1) e.toString
      else (e.toString + "/" + d.toString)
   }
   private val matcher = utils.StringMatcher("","/","")
   override def fromString(s: String) = s match {
      case this.matcher(e,d) => (StandardInt.fromString(e), StandardNat.fromString(d))
      case s => (StandardInt.fromString(s.trim),1)
   }
   override def lex = Some(new parser.NumberLiteralLexer(false,true))
}


/** rational complex numbers */
object ComplexRat extends Product(StandardRat, StandardRat) {
}
// switched to java.lang.Double, because that's what .toDouble returns and
// java.lang.Double =/= scala.Double (problem in RepresentationType.unapply)
object StandardDouble extends Atomic[java.lang.Double] {
   val cls = classOf[java.lang.Double]
   val key = "OMF"
   def fromString(s: String) = s.toDouble //s.toDouble
   override def lex = Some(new parser.NumberLiteralLexer(true, false))
}

object StandardString extends Atomic[String] {
   val cls = classOf[String]
   def fromString(s: String) = s
   override def lex = quotedLiteral("")
}

object StandardBool extends Atomic[java.lang.Boolean] {
   val cls = classOf[java.lang.Boolean]
   def fromString(s: String) = s match {
     case "true" => true
     case "false" => false
   }
   override def lex = Some(FiniteKeywordsLexer(List("true","false")))
}

import utils.URI
/** URI literals, concrete syntax is uri"..." */
object URILiteral extends Atomic[URI] {
   val cls = classOf[URI]
   def fromString(s: String) = URI(s)
   override def lex = quotedLiteral("uri")
}

/** OpenMath's literals
 *  These should be moved to an OpenMath plugin, but they are used by the API, e.g., for metadata
 */
object OMLiteral {
   def apply[V <: Any](key: String, semType: Atomic[V]) =
      new RepresentedRealizedType(objects.OMS(OpenMath._path ? key), semType)
   /** OpenMath OMI - unlimited precision integers */
   val OMI = apply("OMI", StandardInt)
   /** OpenMath OMF - IEEE double precision floats */
   val OMF = apply("OMF", StandardDouble)
   /** OpenMath OMSTR - strings */
   val OMSTR = apply("OMSTR", StandardString)
   /** URIs (not actually part of OpenMath) */
   val URI = apply("URI", URILiteral)
}