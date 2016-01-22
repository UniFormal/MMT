package info.kwarc.mmt.api.valuebases

import info.kwarc.mmt.api._
import objects._
import uom._

/**
 * codes a list
 */
abstract class ListCodec[Code](id: GlobalName, list: GlobalName, nil: GlobalName, cons: GlobalName)
   extends CodecOperator[Code](id, list) {self =>
   
   val numberOfTypeParameters = 1
   
   def aggregate(cs: List[Code]): Code
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
      if (cs.length != numberOfTypeParameters)
         throw CodecNotApplicable
      val codec = cs.head
      new Codec[Code](id(codec.exp), tp(codec.tp)) {
         def encode(t: Term) = self.aggregate(self.destruct(t) map codec.encode)
         def decode(c: Code) = self.construct(self.separate(c) map codec.decode)
      }
   }
}

class AtomicStringCodec[Rep](id: GlobalName, tp: Term, semType: Atomic[Rep])
   extends AtomicCodec[Rep,String](id, tp, semType) {
   def encodeRep(r: Rep) = semType.toString(r)
   def decodeRep(c: String) = semType.fromString(c)
}

