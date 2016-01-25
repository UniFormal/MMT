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

  val typesystem = ODK.path ? "Typesystem"
  val logic = ODK.path ? "Logic"
  val equality = ODK.path ? "Equality"
  val natliterals = ODK.path ? "NatLiterals"
  val intliterals = ODK.path ? "IntLiterals"
  val stringliterals = ODK.path ? "StringLiterals"
  val listliterals = ODK.path ? "ListLiterals"
  val vectorliterals = ODK.path ? "VectorLiterals"

  val tm = typesystem ? "tm"
  val bool = logic ? "bool"
  val tt = logic ? "true"
  val ff = logic ? "false"
  val int = intliterals ? "int"
  val nat = natliterals ? "nat"
  val succ = natliterals ? "nat_succ"
  val string = stringliterals ? "string"
  val list = listliterals ? "list"
  val nil = listliterals ? "nil"
  val cons = listliterals ? "cons"
  val vector = vectorliterals ? "vector"
  val zerovec = vectorliterals ? "zerovec"
  val vectorprepend = vectorliterals ? "vector_prepend"
}

object IntegerLiterals extends RealizedType(Apply(OMS(Math.tm),OMS(Math.int)),StandardInt)
object NatLiterals extends RealizedType(Apply(OMS(Math.tm),OMS(Math.nat)),StandardNat)
object NatSucc extends RealizedOperator(Math.succ) {
  val argTypes = List(NatLiterals)
  val retType = NatLiterals

  def apply(args: List[Term]): OMLIT = args match {
    case List(NatLiterals(u : BigInt)) => NatLiterals.apply(u + 1)
    case _ => throw new Exception("Put a helpful error message here")
  }
}
object NatSuccInverse extends InverseOperator(Math.succ) {
  def unapply(l: OMLIT): Option[List[OMLIT]] = l match {
    case NatLiterals(u : BigInt) if u>0 => Some(List(NatLiterals(u-1)))
    case _ => None
  }
}

object StringLiterals extends RealizedType(Apply(OMS(Math.tm),OMS(Math.string)),StandardString)
