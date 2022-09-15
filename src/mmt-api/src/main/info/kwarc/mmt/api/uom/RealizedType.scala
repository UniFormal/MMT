package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._
import parser._

/**
 * A RealizedType couples a syntactic type (a [[Term]]) with a semantic type (a PER on the universe of Scala values).
 */
case class RealizedType(synType: Term, semType: SemanticType) extends SimplifierRule {self =>
   def head = synType.head match {
      case Some(h: GlobalName) => h
      case _ => throw ImplementationError("syntactic type must have head")
   }
   /** allows constructing OMLITs as this of u
    *
    *  this checks validity at run-time but does not check Scala typing at compile-time
    *  a type-safe apply method is added in [[RepresentedRealizedType]])
    */
   def of(u: Any) = {
      if (!semType.valid(u)) {
         // semType.valid(u) // only needed for debugging, useful to have a breakpoint here to debug the validity check
         throw ParseError("invalid literal value for type " + synType + " realized as " + semType.asString + ": " + u)
      }
      val vN = semType.normalform(u)
      OMLIT(vN, this)
   }
   /** unapply method to pattern-match OMLITs as this(u) */
   def unapply(t: Term) : Option[Any] = t match {
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
      of(sP)
   }
   /** @return the lexer extension to be used by the lexer, defined in terms of the LexFunction lex */
   def lexerExtension = semType.lex map {l =>
     val p = new LiteralParser(self)
     new LexParseExtension(l, p) {
        override def toString = "rule LexParseExtension " + semType.toString
        override val priority = self.priority // reusing priority of this rule for the associated lexing rule
     }
   }
}

class RepresentedRealizedType[V](synType: Term, override val semType: RSemanticType[V]) extends RealizedType(synType,semType) {
   // better matching by returning sharper type
   override def unapply(u: objects.Term): Option[V] =
     super.unapply(u) flatMap semType.unapply
   /** like 'of' but type-safe */
   def apply(v: V) = of(semType.apply(v))
}
