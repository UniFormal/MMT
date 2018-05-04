package info.kwarc.mmt.odk.codecs

import info.kwarc.mmt.api._
import objects._
import uom._
import utils._
import valuebases._
import info.kwarc.mmt.lf.{Apply, ApplySpine}
import info.kwarc.mmt.mitm.MitM
import info.kwarc.mmt.odk._

trait BigIntAsJSON {
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


object TMInt extends LiteralsCodec[BigInt,JSON](Codecs.standardInt, IntegerLiterals) with BigIntAsJSON
object TMNat extends LiteralsCodec[BigInt,JSON](Codecs.standardNat, NatLiterals) with BigIntAsJSON
object TMPos extends LiteralsCodec[BigInt,JSON](Codecs.standardPos, PosLiterals) with BigIntAsJSON

object TMString extends EmbedStringToJSON(new LiteralsAsStringsCodec(Codecs.standardString, StringLiterals))

object BoolAsString extends EmbedStringToJSON(new LiteralsAsStringsCodec(Codecs.boolAsString, MitM.BoolLit))

object BoolAsInt extends LiteralsCodec[java.lang.Boolean,JSON](Codecs.boolAsInt, MitM.BoolLit) {
  def encodeRep(b: java.lang.Boolean) = if (b) JSONInt(1) else JSONInt(0)
  def decodeRep(j: JSON) = j match {
    case JSONInt(1) => true
    case JSONInt(0) => false
    case _ => throw CodecNotApplicable
  }
}

object StandardBool extends LiteralsCodec[java.lang.Boolean,JSON](Codecs.standardBool, MitM.BoolLit) {
  def encodeRep(b: java.lang.Boolean) = JSONBoolean(b)
  def decodeRep(j: JSON) = j match {
    case JSONBoolean(b) => b
    case _ => throw CodecNotApplicable
  }
}

object TMList extends ListCodec[JSON](Codecs.standardList, MitM.list, MitM.nil, MitM.cons) {
  def aggregate(cs: List[JSON]): JSON = JSONArray(cs:_*)
  def separate(j: JSON): List[JSON] = j match {
    case JSONArray(js@_*) => js.toList
    case _ => throw CodecNotApplicable
  }
}

object StandardVector extends CodecOperator[JSON](Codecs.standardVector, MitM.vector) {self =>
  val typeParameterPositions : List[Int] = List(1)

  def aggregate(cs: List[JSON]): JSON = JSONArray(cs:_*)
  def separate(j: JSON): List[JSON] = j match {
    case JSONArray(js@_*) => js.toList
    case _ => throw CodecNotApplicable
  }

  def destruct(tm: Term): List[Term] = tm match {
    case Apply(OMS(MitM.zerovec), _) => Nil
    case ApplySpine(OMS(MitM.vectorprepend), List(_, _, hd, tl)) => hd :: destruct(tl)
  }
  def construct(elemTp: Term, tms: List[Term]): Term = {
    tms.foldLeft[Term](Apply(OMS(MitM.zerovec),elemTp)) {
      case (sofar, next) =>
        ApplySpine(OMS(MitM.vectorprepend), elemTp, NatLiterals.of(BigInt(destruct(sofar).length)), next, sofar)
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


object StandardMatrix extends CodecOperator[JSON](Codecs.standardMatrix, MitM.matrix) {self =>

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
      ApplySpine(OMS(MitM.vector), elemTp, NatLiterals.of(m)),
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
