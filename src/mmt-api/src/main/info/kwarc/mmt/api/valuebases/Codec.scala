package info.kwarc.mmt.api.valuebases

import info.kwarc.mmt.api._
import objects._
import uom._
import utils._

case object CodecNotApplicable extends java.lang.Throwable

/**
 * encodes/decodes terms of a certain type
 * @param exp the id of this codec
 * @param tp the type that is coded
 */
abstract class Codec[Code](val exp: Term, val tp: Term) {
   val codeType: ConcreteType
   def encode(t: Term): Code
   def decode(c: Code): Term
   /** data must be validated even if decoding succeeds, e.g., check prime-ness of integer-encoded prime numbers */
   // TODO check if codec operators should override this
   def strictDecode(c: Code): Term = decode(c)
}

/**
 * encodes/decodes terms whose type is of the form T = OMA(OMS(tp), pars)
 * @param id the id of the codec operator
 * @param tp the type operator
 */
abstract class CodecOperator[Code, C <: Codec[Code]](val id: GlobalName, val tp: GlobalName) {

   /** positions in the argument list (starting from 1) in OMA(list, ...) that are type arguments */
   val typeParameterPositions : List[Int]

   /**
    * @param cs one codec for each type parameter; pre: cs.length == this.typeParameterPositions.length
    * @return a codec for OMA(OMS(tp), cs.map(_.tp))
    */
   def apply(cs: C*) : C
}

/**
 * lifts an embedding between code types to a functor on codecs
 * 
 * This is particularly useful to use [[AtomicStringCodec]] to code literals as strings and then lift the strings into the desired code type.  
 */
abstract class Embedding[From,To](c: Codec[From]) extends Codec[To](c.exp, c.tp) {
  /** the embedding function */
  def embed(f: From): To
  /** partial inverse of embed */ 
  def extract(t: To): Option[From]

  def encode(t: Term) = embed(c.encode(t))
  def decode(rT: To) = extract(rT) match {
     case Some(rF) => c.decode(rF)
     case None => throw CodecNotApplicable
  }
}

/**
 * encodes/decodes literals
 *  
 * the abstract methods must handle the toString/fromString for type Rep
 * 
 * @tparam Rep the underlying type of the literals
 * @tparam Code the target type of the codecs
 * @param id the id of the codec
 * @param rt the realized type
 */
abstract class LiteralsCodec[Rep,Code](id: GlobalName, rt: RepresentedRealizedType[Rep]) extends Codec[Code](OMS(id), rt.synType) {
   def encodeRep(r: Rep): Code
   def encode(t: Term): Code = t match {
      case rt(u) => encodeRep(u)
      case _ => throw CodecNotApplicable
   }

   def decodeRep(c: Code): Rep
   def decode(c: Code): Term = rt(decodeRep(c))
}

/** turns a realized type into the corresponding codec using the toString/fromString functions of the semantic type */ 
class LiteralsAsStringsCodec[Rep](id: GlobalName, rt: RepresentedRealizedType[Rep]) extends Codec[String](OMS(id), rt.synType) {
   val codeType = StringType
   def encode(t: Term) = rt.unapply(t) map rt.semType.toString getOrElse(throw CodecNotApplicable)
   def decode(c: String) = try {rt.parse(c)} catch {case p: ParseError => throw CodecNotApplicable}
}