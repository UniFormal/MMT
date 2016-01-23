package info.kwarc.mmt.odk

import info.kwarc.mmt.api._
import frontend._
import objects._
import uom._
import valuebases._
import utils._

import info.kwarc.mmt.lf._

class Plugin extends frontend.Plugin {
  val theory = Math.path
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    // content enhancers
    controller.extman.addExtension(new LMFDB.Plugin)
  }
}

object ODK {
   val path = DPath(URI("http","www.opendreamkit.org"))
}

object Math {
  val path = ODK.path ? "Math"

  val tm = path ? "tm"
  val bool = path ? "bool"
  val tt = path ? "true"
  val ff = path ? "false"
  val int = path ? "int"
  val nat = path ? "nat"
  val string = path ? "string"
  val list = path ? "list"
  val nil = path ? "nil"
  val cons = path ? "cons"
}

object IntegerLiterals extends RealizedType(Apply(OMS(Math.tm),OMS(Math.int)),StandardInt)
object NatLiterals extends RealizedType(Apply(OMS(Math.tm),OMS(Math.nat)),StandardNat)
object StringLiterals extends RealizedType(Apply(OMS(Math.tm),OMS(Math.string)),StandardString)
