package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import objects._
import parser._

/**
 * A RealizedType couples a syntactic type (a Constant) with a semantic type (a Scala type as given by a [[SemanticType]]).
 */
trait RealizedType extends SemanticType with uom.UOMRule {
   private var _synType: Term = null
   private var _head: GlobalName = null
   /** Due to various constraints including limitations of the Scala type system,
    *  the most convenient solution is to initialize _synType null.
    *  
    *  Therefore, instances of RealizedType must set them by calling this method exactly once.
    *  MMT does that automatically when using a Scala object as a realization.
    */
   def init(synType: Term) {
      if (_synType == null) {
         _synType = synType
         synType.head match {
            case Some(p: GlobalName) => _head = p
            case _ => throw ImplementationError("syntactic type must have head")
         }
      } else
         throw ImplementationError("syntactic type already set")
   }
   /** convenience variant for atomic types */
   def init(synType: GlobalName) {init(OMS(synType))}
   /** the syntactic type */
   def synType = _synType 
   /** the head operator of the syntactic type */ 
   def head = _head
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