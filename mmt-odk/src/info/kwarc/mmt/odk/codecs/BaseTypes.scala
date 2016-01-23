package info.kwarc.mmt.odk.codecs

import info.kwarc.mmt.api.objects.{Term, OMS}
import info.kwarc.mmt.api.uom.{StandardString, StandardInt, RealizedType}
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.valuebases._
import info.kwarc.mmt.lf.Apply
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