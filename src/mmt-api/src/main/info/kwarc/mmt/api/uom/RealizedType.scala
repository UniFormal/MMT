package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._
import parser._

/**
 * A RealizedType couples a syntactic type (a [[Term]]) with a semantic type (a PER on the universe of Scala values).
 */
case class RealizedType(synType: Term, semType: SemanticType) extends uom.UOMRule {
   private type univ = Any
   def head = synType.head match {
      case Some(h: GlobalName) => h
      case _ => throw ImplementationError("syntactic type must have head")
   }
   /** apply method to construct OMLITs as this(u) */
   def apply(u: univ) = {
      if (!semType.valid(u))
         throw ParseError("invalid literal value for type " + synType + ": " + u)
      val vN = semType.normalform(u)
      OMLIT(vN, this)
   }
   /** unapply method to pattern-match OMLITs as this(u) */
   def unapply(t: Term) : Option[univ] = t match {
      case OMLIT(v, rt) if rt == this => Some(v)
      case OMLIT(v, rt) =>
        // subtyping: assuming the input is well-formed, we know rt.synType <: synType; so we pick the canonical embedding rt.semType -> semType
        rt.semType.embed(semType) match {
          case Some(e) => Some(e.map(v))
          case _ => None
        }
      case UnknownOMLIT(v,st) if st == synType => Some(semType.fromString(v))
      case _ => None
   }
   /** @return the OMLIT obtained by using fromString */
   def parse(s: String) = {
      val sP = semType.fromString(s)
      apply(sP)
   }
   /** @return the lexer extension to be used by the lexer, defined in terms of the LexFunction lex */
   def lexerExtension = semType.lex map {case l => new LexParseExtension(l, new LiteralParser(this))} 
}

class RepresentedRealizedType[V](synType: Term, override val semType: RSemanticType[V]) extends RealizedType(synType,semType) {
   override def unapply(u: objects.Term): Option[V] =
     super.unapply(u) flatMap semType.unapply
}
