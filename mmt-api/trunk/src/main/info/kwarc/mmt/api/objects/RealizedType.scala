package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._ 
import parser._

/**
 * 
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


/**
 * A RealizedType couples a syntactic type with a semantic type.
 */
abstract class RealizedType extends SemanticType {
   private var _synType: GlobalName = null
   private var _realization: MPath = null
   /** Due to various constraints including limitations of the Scala type system,
    *  the most convenient solution is to initialize _synType and _realization as null.
    *  
    *  Therefore, instances of RealizedType must set them by calling this method exactly once.
    *  MMT does that automatically when using a Scala object as a realization.
    */
   def init(synType: GlobalName, real: MPath) {
      if (_synType == null) {
         _synType = synType
         _realization = real
      } else
         throw ImplementationError("syntactic type already set")
   }
   /** the syntactic type */
   def synType = _synType 
   /** the realization */
   def home = _realization 
   /** the path of the corresponding RealizedTypeConstant */
   def path = home ? synType.name
   /**
    * two RealizedTypes are equal if they realize the same type and arise from the same realization 
    */
   override def equals(that: Any) = that match {
      case that: RealizedType =>
         synType == that.synType && home == that.home 
      case _ => false
   }
   /** apply method to construct OMLITs as rt(u) */
   def apply(u: univ) = OMLIT(this)(u)
   /** unapply method to pattern-match OMLITs as rt(u) */
   def unapply(t: Term) : Option[univ] = t match {
      case t : OMLIT if t.rt == this =>
         Some(t.value.asInstanceOf[univ]) // type cast succeeds by invariant of OMLIT
      case _ => None
   }
   /** @return the OMLIT obtained by using fromString */
   def parse(s: String) = {
      val sP = fromString(s)
      OMLIT(this)(sP)
   }
   /** @return the lexer extension to be used by the lexer, defined in terms of the LexFunction lex */
   def lexerExtension = lex map {case l => new LexParseExtension(l, new LiteralParser(this))} 
}

class Product(val left: SemanticType, val right: SemanticType) extends SemanticType {
   type univ = (left.univ, right.univ)
   override def valid(u: univ) = left.valid(u._1) && right.valid(u._2)
   override def normalform(u: univ) = (left.normalform(u._1), right.normalform(u._2))
   def fromString(s: String) = null // TODO
   override def toString(u: univ) = "(" + left.toString(u._1) + "," + right.toString(u._2) + ")"
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