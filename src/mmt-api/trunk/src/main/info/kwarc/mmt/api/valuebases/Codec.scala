package info.kwarc.mmt.api.valuebases

import info.kwarc.mmt.api._
import objects._
import uom._

case object CodecNotApplicable extends java.lang.Throwable

/**
 * encodes/decodes terms of a certain type
 * @param exp the id of this codec
 * @param tp the type that is coded
 */
abstract class Codec[Code](val exp: Term, val tp: Term) {
   def encode(t: Term): Code
   def decode(c: Code): Term
}

abstract class AtomicCodec[Rep,Code](id: GlobalName, tp: Term, semType: SemanticType with RepresentationType[Rep]) extends Codec[Code](OMS(id), tp) {
   val rt = new RepresentedRealizedType(tp, semType)
   def encodeRep(r: Rep): Code
   def encode(t: Term): Code = t match {
      case rt(u) => encodeRep(u)
      case _ => throw CodecNotApplicable
   }

   def decodeRep(c: Code): Rep
   def decode(c: Code): Term = rt(decodeRep(c))
}

/**
 * encodes/decodes terms whose type is of the form T = OMA(OMS(tp), pars)
 * @param id the id of the codec operator
 * @param tp the type operator
 */
abstract class CodecOperator[Code](val id: GlobalName, val tp: GlobalName) {

   /** positions in the argument list (starting from 1) in OMA(list, ...) that are type arguments */ 
   val typeParameterPositions : List[Int]
   
   /**
    * @param cs one codec for each type parameter; pre: cs.length == this.typeParameterPositions.length
    * @return a codec for OMA(OMS(tp), cs.map(_.tp)) 
    */
   def apply(cs: Codec[Code]*) : Codec[Code]
}

/** lifts an embedding between representation types to a functor on codecs */
abstract class Embedding[From,To] {
   def embed(f: From): To
   def extract(t: To): Option[From]
   
   def apply(c: Codec[From]): Codec[To] = new Codec[To](c.exp, c.tp) {
      def encode(t: Term) = embed(c.encode(t))
      def decode(rT: To) = extract(rT) match {
         case Some(rF) => c.decode(rF)
         case None => throw CodecNotApplicable
      }
   }
}