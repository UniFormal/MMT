package info.kwarc.mmt.odk.codecs

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.objects.{OMA, OMS, Term}
import info.kwarc.mmt.api.valuebases.{CodecNotApplicable, Codec, CodecOperator}
import info.kwarc.mmt.lf.{ApplySpine, Apply}

/**
  * codes a list
  */
abstract class ListCodec[Code](id: GlobalName, list: GlobalName, nil: GlobalName, cons: GlobalName)
  extends CodecOperator[Code](id, list) {self =>

  val numberOfOtherParameters = 1
  val numberOfTypeParameters = 1

  def aggregate(cs: List[Code]): Code
  def separate(c: Code): List[Code]

  def destruct(tm: Term): List[Term] = tm match {
    case Apply(OMS(nil), _) => Nil
    case ApplySpine(OMS(cons), List(_, hd, tl)) => hd :: destruct(tl)
  }
  def construct(elemTp: Term, tms: List[Term]): Term = {
    tms.foldLeft[Term](Apply(OMS(nil),elemTp)) {
      case (sofar, next) => ApplySpine(OMS(cons), elemTp, next, sofar)
    }
  }

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
      def decode(c: Code) = self.construct(codec.tp, self.separate(c) map codec.decode)
    }
  }
}
