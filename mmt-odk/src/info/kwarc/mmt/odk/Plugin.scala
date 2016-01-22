package info.kwarc.mmt.odk

import info.kwarc.mmt.api._
import frontend._
import objects._
import uom._
import valuebases._
import utils._

import info.kwarc.mmt.lf._

class Plugin extends frontend.Plugin {
  val theory = Typesystem.path
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    // content enhancers
    controller.extman.addExtension(new LMFDB.Plugin)
  }
}

object ODK {
   val path = DPath(URI("http","www.opendreamkit.org"))
}

object Typesystem {
  val path = ODK.path ? "TypeSystem"

  val int = path ? "int"
  val string = path ? "string"
  val list = path ? "list"
  val nil = path ? "nil"
  val cons = path ? "cons"
}

object Codecs {
   val path = ODK.path ? "Codecs"

   val int = path ? "int"
   val string = path ? "string"
   val list = path ? "list"
}

object TMInt extends AtomicCodec[BigInt,JSON](Codecs.int, OMS(Typesystem.int), StandardInt) {
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

object TMString extends AtomicCodec[String,JSON](Codecs.string, OMS(Typesystem.string), StandardString) {
  def encodeRep(s: String) = JSONString(s)
  def decodeRep(j: JSON) = j match {
     case JSONString(s) => s
     case _ => throw CodecNotApplicable
  }
}

object TMList extends ListCodec[JSON](Codecs.list, Typesystem.list, Typesystem.nil, Typesystem.cons) {
  def aggregate(cs: List[JSON]): JSON = JSONArray(cs:_*)
  def separate(j: JSON): List[JSON] = j match {
     case JSONArray(js@_*) => js.toList
     case _ => throw CodecNotApplicable
  }
}