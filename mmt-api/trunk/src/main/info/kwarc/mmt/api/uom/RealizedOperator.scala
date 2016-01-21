package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._
import parser._

/**
 * A RealizedType couples a syntactic type (a [[Term]]) with a semantic type (a PER on the universe of Scala values).
 */
class RealizedType(val synType: Term, val semType: SemanticType) extends uom.UOMRule {
   override def equals(that: Any) = that match {
      case rt: RealizedType => synType == rt.synType && semType == rt.semType
      case _ => false
   }
   type univ = Any
   def head = synType.head match {
      case Some(h: GlobalName) => h
      case _ => throw ImplementationError("syntactic type must have head")
   }
   /** apply method to construct OMLITs as this(u) */
   def apply(u: univ) = {
      if (!semType.valid(u))
         throw ParseError("invalid literal value: " + u)
      val vN = semType.normalform(u)
      OMLIT(vN, this)
   }
   /** unapply method to pattern-match OMLITs as this(u) */
   def unapply(t: Term) : Option[univ] = t match {
      case OMLIT(v, rt) if rt == this => Some(v)
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

class RepresentedRealizedType[V](synType: Term, semType: SemanticType with RepresentationType[V]) extends RealizedType(synType,semType) {
   override def unapply(u: objects.Term): Option[V] = super.unapply(u) flatMap semType.unapply
}

/** A RealizedOperator couples a syntactic function (a Constant) with a semantic function (a Scala function) */
abstract class RealizedOperator(op: GlobalName) extends BreadthRule(op) {
   val argTypes: List[RealizedType]
   val retType : RealizedType
   private def applicable(args: List[Term]): Boolean = {
      if (args.length != argTypes.length) throw ParseError("")
      (args zip argTypes).forall {
         case (l: OMLIT, lt) => l.rt == lt
         case _ => false
      }
   }
   def apply(args: List[Term]): OMLIT
   
   val apply: Rewrite = (args: List[Term]) => {
         if (applicable(args))
            GlobalChange(apply(args))
         else
            NoChange
   }
}


/** 
 *  counterpart to a [[RealizedOperator]] for the partial inverse
 */
abstract class InverseOperator(val head: GlobalName) extends UOMRule {
   /** takes the result of applying 'head' and returns the list of arguments */
   def unapply(l: OMLIT): Option[List[OMLIT]]
}