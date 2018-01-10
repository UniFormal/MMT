package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import parser._

/**
 * A Scala-level type to be used in a [[RealizedType]]
 * 
 * See also [[SemanticOperator]]
 * 
 * SemanticType's are closed under subtypes and quotients by using valid and normalform.
 * In other words, the triple (U, valid, normalform) forms a partial equivalence relation on a Scala type U.
 * 
 * For technical reasons, U is fixed as the type [[Any]] (Scala cannot type check enough to make the overhead worthwhile.)
 * Instead, type-safety-supporting syntax is offered wherever reasonable, see also [[RSemanticType]].
 */
abstract class SemanticType extends SemanticObject {
  
   /** string representation of this type (should be toString, but we want to force people to implement this) */
   def asString: String
   
   override def toString = asString
  
   /** the predicate on the semantic type used to obtain subtypes
    *  
    *  true by default
    */
   def valid(u: Any): Boolean = true
   /** the equivalence relation (given as a normal form conversion) on the semantic type used to obtain quotient types
    *  pre: valid(u) == true
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
   /** @return a fresh iterator over values of this type */
   def enumerate: Option[Iterator[Any]] = None
   
   /** returns a canonical embedding from this type into some other type
    *  only the identity of this type by default, override as needed
    */
   def embed(into: SemanticType): Option[SemanticOperator.Unary] = if (this == into) Some(id) else None
   
   /** the identify function of this type */
   def id = SemanticOperator.Unary(this,this)(x => x)

   /** enables the notation (a,b) =>: c for function types */
   def =>:(arg: SemanticType) = SemOpType(List(arg), this)
   def =>:(args: List[SemanticType]) = SemOpType(args, this)
}

/** a semantic type whose underlying representation type is made explicit */
trait RSemanticType[V] extends SemanticType {
   /** this must be the class object of V (which cannot be implemented generically here in Scala) */
   val cls: Class[V]
   
   /** does nothing but triggers Scala type checking */
   def apply(v: V): Any = v
   
   /** does nothing but refines the Scala type if possible */ 
   def unapply(u: Any): Option[V] = u match {
      //TODO not typesafe for complex types, cls == u.getClass works for complex types but does not consider subtyping
      case v: V@unchecked if cls.isInstance(v) =>
         Some(v)
      case v: V@unchecked if cls.isPrimitive && cls == u.getClass =>
         Some(v) // isInstance is always false for primitive classes
      case _ => None
   }
}

object SemanticType {
  /** the type of types (experimental) */
  object AllTypes extends RSemanticType[SemanticType] {
    def asString = "TypeOfTypes"
    val cls = classOf[SemanticType]
    def toString(u: Any) = u match {
      case u: SemanticType => u.asString
    }
    def fromString(s: String) = throw ParseError("cannot parse semantic types")
  }
}

/** a type that is equal to an existing Scala type */ 
abstract class Atomic[V] extends RSemanticType[V] {
   override def valid(u: Any) = unapply(u).isDefined
   def toString(u: Any) = unapply(u).get.toString
   /** narrower type */
   def fromString(s: String): V
}