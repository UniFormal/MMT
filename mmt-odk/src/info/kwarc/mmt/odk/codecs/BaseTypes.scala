package info.kwarc.mmt.odk.codecs

import info.kwarc.mmt.api.objects.{OMS, Term}
import info.kwarc.mmt.api.uom.{RealizedType, StandardInt, StandardNat, StandardString}
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.valuebases._
import info.kwarc.mmt.lf.{Apply, ApplySpine}
import info.kwarc.mmt.odk._

object TMInt extends AtomicCodec[BigInt,JSON](Codecs.standardInt, OMS(Math.int), StandardInt) {
  def encodeRep(i: BigInt): JSON = {
    if (i.isValidInt)
      JSONInt(i.toInt)
    else
      JSONString(i.toString)
  }
  def decodeRep(j: JSON): BigInt = j match {
    case JSONInt(i) => BigInt(i)
    case JSONString(s) => BigInt(s)
    case _ => throw CodecNotApplicable
  }
}

object TMNat extends AtomicCodec[BigInt,JSON](Codecs.standardNat, OMS(Math.nat), StandardNat) {
  def encodeRep(i: BigInt): JSON = {
    require(i>=0)
    if (i.isValidInt)
      JSONInt(i.toInt)
    else
      JSONString(i.toString)
  }
  def decodeRep(j: JSON): BigInt = j match {
    case JSONInt(i) if i>= 0 => BigInt(i)
    case JSONString(s) => BigInt(s)
    case _ => throw CodecNotApplicable
  }
}

object TMPos extends AtomicCodec[BigInt,JSON](Codecs.standardPos, OMS(Math.pos), StandardNat) {
  def encodeRep(i: BigInt): JSON = {
    require(i>=0)
    if (i.isValidInt)
      JSONInt(i.toInt)
    else
      JSONString(i.toString)
  }
  def decodeRep(j: JSON): BigInt = j match {
    case JSONInt(i) if i>= 0 => BigInt(i)
    case JSONString(s) => BigInt(s)
    case _ => throw CodecNotApplicable
  }
}

object TMString extends AtomicCodec[String,JSON](Codecs.standardString, OMS(Math.string), StandardString) {
  def encodeRep(s: String) = JSONString(s)
  def decodeRep(j: JSON) = j match {
    case JSONString(s) => s
    case _ => throw CodecNotApplicable
  }
}

object BoolAsInt extends Codec[JSON](OMS(Codecs.boolAsInt), OMS(Math.bool)) {
  def encode(t: Term) = t match {
    case OMS(Math.tt) => JSONInt(1)
    case OMS(Math.ff) => JSONInt(0)
  }
  def decode(j: JSON) = j match {
    case JSONInt(1) => OMS(Math.tt)
    case JSONInt(0) => OMS(Math.ff)
    case _ => throw CodecNotApplicable
  }
}

object StandardBool extends Codec[JSON](OMS(Codecs.standardBool), OMS(Math.bool)) {
  def encode(t: Term) = t match {
    case OMS(Math.tt) => JSONBoolean(true)
    case OMS(Math.ff) => JSONBoolean(false)
  }
  def decode(j: JSON) = j match {
    case JSONBoolean(b) => if (b) OMS(Math.tt) else OMS(Math.ff)
    case _ => throw CodecNotApplicable
  }
}

object TMList extends ListCodec[JSON](Codecs.standardList, Math.list, Math.nil, Math.cons) {
  def aggregate(cs: List[JSON]): JSON = JSONArray(cs:_*)
  def separate(j: JSON): List[JSON] = j match {
    case JSONArray(js@_*) => js.toList
    case _ => throw CodecNotApplicable
  }
}

object StandardVector extends CodecOperator[JSON](Codecs.standardVector, Math.vector) {self =>
  val typeParameterPositions : List[Int] = List(1)

  def aggregate(cs: List[JSON]): JSON = JSONArray(cs:_*)
  def separate(j: JSON): List[JSON] = j match {
    case JSONArray(js@_*) => js.toList
    case _ => throw CodecNotApplicable
  }

  def destruct(tm: Term): List[Term] = tm match {
    case Apply(OMS(Math.zerovec), _) => Nil
    case ApplySpine(OMS(Math.vectorprepend), List(_, _, hd, tl)) => hd :: destruct(tl)
  }
  def construct(elemTp: Term, tms: List[Term]): Term = {
    tms.foldLeft[Term](Apply(OMS(Math.zerovec),elemTp)) {
      case (sofar, next) => ApplySpine(OMS(Math.vectorprepend), elemTp, NatLiterals(destruct(sofar).length), next, sofar)
    }
  }

  def apply(cs: Codec[JSON]*) = {
    val codec = cs.head
    new Codec[JSON](id(codec.exp), tp(codec.tp)) {
      def encode(t: Term) = self.aggregate(self.destruct(t) map codec.encode)
      def decode(c: JSON) = self.construct(codec.tp, self.separate(c) map codec.decode)
    }
  }

}


object StandardMatrix extends CodecOperator[JSON](Codecs.standardMatrix, Math.matrix) {self =>

  val typeParameterPositions : List[Int] = List(1)

  def aggregate(cs: List[List[JSON]]): JSON = JSONArray(cs.map(l => JSONArray(l:_*)):_*)
  def separate(j: JSON): List[List[JSON]] = j match {
    case JSONArray(js@_*) =>
      js.map({
        case JSONArray(in@_*) => in.toList
        case _ => throw CodecNotApplicable
      }).toList
    case _ => throw CodecNotApplicable
  }

  def destruct(tm: Term): List[List[Term]] = StandardVector.destruct(tm).map(StandardVector.destruct)
  def construct(elemTp: Term, tms: List[List[Term]]): Term = {
    val n = tms.length
    val m = if (n > 1) {
      if (tms.tail.forall(_.length == tms.head.length)) tms.head.length else throw CodecNotApplicable
    } else tms.length
    StandardVector.construct(
      ApplySpine(OMS(Math.vector), elemTp, NatLiterals.apply(m)),
      tms.map(StandardVector.construct(elemTp, _)))
  }
  /*
  def apply(cs: Codec[JSON]*) = {
    val codec = cs.head
    new Codec[JSON](id(codec.exp), tp(codec.tp)) {
      def encode(t: Term) : JSON = self.aggregate(self.destruct(t).map(_.map(codec.encode)))
      def decode(c: JSON) : Term = self.construct(codec.tp, self.separate(c).map(_.map(codec.decode)))
    }
  }
  */
  def apply(cs : Codec[JSON]*) = StandardVector(StandardVector(cs.head))
}
