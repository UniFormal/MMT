package info.kwarc.mmt.api.valuebases

import info.kwarc.mmt.api._
import objects._
import uom._

/**
 * codes a list
 */
// useless in practice because we always use LF lists; generalized to LF lists in [[info.kwarc.mmt.odk.codecs.ListCodec]]
abstract class ListCodec[Code](id: GlobalName, list: GlobalName, nil: GlobalName, cons: GlobalName)
   extends CodecOperator[Code, Codec[Code]](id, list) {self =>

   val typeParameterPositions = List(1)

   /** merge a list of codes into a code */
   def aggregate(cs: List[Code]): Code
   /** split a list-code into its elements */
   def separate(c: Code): List[Code]

   def destruct(tm: Term): List[Term] = tm match {
      case OMS(nil) => Nil
      case OMA(OMS(cons), List(hd, tl)) => hd :: destruct(tl)
   }
   def construct(tms: List[Term]): Term = tms.foldRight[Term](OMS(nil)){case (sofar, next) => cons(next, sofar)}

   /**
    * @param cs one codec for each type parameter; pre: cs.length == this.numberOfTypeParameters
    * @return a codec for OMA(OMS(tp), cs.head.tp)
    */
   def apply(cs: Codec[Code]*) = {
      val codec = cs.head
      new Codec[Code](id(codec.exp), tp(codec.tp)) {
         val codeType = utils.ListType(codec.codeType)
         def encode(t: Term) = self.aggregate(self.destruct(t) map codec.encode)
         def decode(c: Code) = self.construct(self.separate(c) map codec.decode)
      }
   }
}