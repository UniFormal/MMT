package info.kwarc.mmt.odk.codecs

import info.kwarc.mmt.api.GlobalName
import info.kwarc.mmt.api.objects.{Term, OMA, OMS}
import info.kwarc.mmt.api.utils.ConcreteType
import info.kwarc.mmt.api.valuebases.{CodecOperator, Codec, CodecNotApplicable}
import info.kwarc.mmt.lf.{ApplySpine, Apply}

/**
  * like the one in valuebases, but for LF-based lists
  */
abstract class ListCodec[Code](id: GlobalName, list: GlobalName, nil: GlobalName, cons: GlobalName)
  extends CodecOperator[Code,Codec[Code]](id, list) {self =>

  val typeParameterPositions = List(1)

  def resultCodeType(ct: ConcreteType): ConcreteType
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
    val codec = cs.head
    new Codec[Code](id(codec.exp), tp(codec.tp)) {
      val codeType = resultCodeType(codec.codeType)
      def encode(t: Term) = self.aggregate(self.destruct(t) map codec.encode)
      def decode(c: Code) = self.construct(codec.tp, self.separate(c) map codec.decode)
    }
  }
}
