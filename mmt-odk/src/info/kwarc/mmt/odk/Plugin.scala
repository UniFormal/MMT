package info.kwarc.mmt.odk

import info.kwarc.mmt.api._
import frontend._
import info.kwarc.mmt.odk.GAP
import objects._
import uom._
import valuebases._
import utils._

import info.kwarc.mmt.lf._

class Plugin extends frontend.Plugin {
  val theory = Math.path
  val dependencies = List("info.kwarc.mmt.lf.Plugin")
  override def start(args: List[String]) {
    controller.extman.addExtension(new LMFDB.Plugin)
    controller.extman.addExtension(new GAP.Plugin)
  }
}

object ODK {
   val path = DPath(URI("http","www.opendreamkit.org"))
}

object Math {
  val path = ODK.path ? "Math"

  val typesystem = ODK.path ? "Types"
  val logic = ODK.path ? "Logic"
  val natliterals = ODK.path ? "Nat"
  val intliterals = ODK.path ? "Int"
  val strings = ODK.path ? "Strings"
  val lists = ODK.path ? "Lists"
  val vectors = ODK.path ? "Vectors"
  val matrices = ODK.path ? "Matrices"

  val tm = typesystem ? "tm"
  val bool = logic ? "bool"
  val tt = logic ? "true"
  val ff = logic ? "false"
  val int = intliterals ? "int"
  val nat = natliterals ? "nat"
  val succ = natliterals ? "nat_succ"
  val string = strings ? "string"
  val list = lists ? "list"
  val nil = lists ? "nil"
  val cons = lists ? "cons"
  val vector = vectors ? "vector"
  val zerovec = vectors ? "zerovec"
  val vectorprepend = vectors ? "vector_prepend"
  val matrix = matrices ? "matrix"
  val matrixconst = matrices ? "matrix_const"
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
