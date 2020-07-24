package info.kwarc.mmt.frameit.archives.Foundation

import info.kwarc.mmt.api._
import parser.{LexFunction, LexParseExtension, ParseFunction}
import objects._
import uom._
import utils._

object BooleanLiterals extends RepresentedRealizedType(OMS(Math.bool),StandardBool)
object StringLiterals extends RepresentedRealizedType(OMS(Math.string),StandardString)
/*
object NumberLiterals extends RealizedType(OMS(Math.real),StandardDouble) {
  override def head: GlobalName = Math.real

  override def of(u: Any): OMLIT = u match {
    case i : BigInt if i > 0 => PosLiterals.of(i)
    case i : BigInt if i >= 0 => NatLiterals.of(i)
    case i : BigInt => IntegerLiterals.of(i)
    case r : Double if r.isValidInt => of(BigInt(r.toInt))
    case r : Double => RealLiterals.of(r)
    case i : Int => of(BigInt(i))
    case _ =>
      throw ParseError("invalid literal value for type " + synType + ": " + u)
  }
  val rtps = List(this,RealLiterals,IntegerLiterals,NatLiterals,PosLiterals)
  val semtps = List(StandardDouble,StandardInt,StandardNat,StandardPositive)
  val syntps = rtps.map(_.synType).tail

  override def unapply(t: Term) : Option[Any] = t match {
    case OMLIT(v, rt) if rtps contains rt => Some(v)
    case UnknownOMLIT(v,st) if syntps contains st =>
      Some(semtps(syntps.indexOf(st)).fromString(v))
    case _ => None
  }

  override def parse(s: String) = of(RealLiterals.parse(s).value)

  val lf = new LexFunction {

    override def unapply(s: String): String = {
      val stp = semtps.collectFirst{case tp if tp.lex.get.unapply(s) != "" => tp.lex.get.unapply(s)}
      stp.getOrElse("")
    }

    override def applicable(s: String, i: Int): Boolean = semtps.exists(_.lex.get.applicable(s,i))

    override def apply(s: String, i: Int): (String, String) = {
      val stp = semtps.find(_.lex.get.applicable(s,i))
      stp.get.lex.get.apply(s,i)
    }
  }
  val top = this
  val pf = new ParseFunction {
    override def unapply(t: Term): String =top.unapply(t).toString
    override def apply(text: String): Term = of(StandardDouble.fromString(text))
  }

  override def lexerExtension: Option[LexParseExtension] = Some(new LexParseExtension(lf,pf))
}
*/
object RealLiterals extends RepresentedRealizedType(OMS(Math.real),StandardDouble) {
 override def priority: Int = 2
}
object IntegerLiterals extends RepresentedRealizedType(OMS(Math.int),StandardInt) {
 override def priority: Int = -1
}
object PosLiterals extends RepresentedRealizedType(OMS(Math.pos),StandardPositive) {
 override def priority: Int = 1
}

object NatSucc extends RealizedOperator(
  Math.succ,
  SynOpType(MitM.under, List(OMS(Math.nat)),OMS(Math.pos)),
  Arithmetic.Succ,
  StandardNat =>: StandardPositive
)

// FR: this is added automatically because Arithmetic.Succ is used above
/*
object NatSuccInverse extends InverseOperator(Math.succ) {
  def unapply(l: OMLIT): Option[List[OMLIT]] = l match {
    case IntegerLiterals(u : BigInt) if u>0 => Some(List(IntegerLiterals.of(u-1)))
    case _ => None
  }
}
*/

class Unary(rt : RealizedType, path : GlobalName, op : Any => Any, prio : Int = 0) extends RealizedOperator(
  path, SynOpType(MitM.under, List(rt.synType),rt.synType), SemanticOperator.Unary(rt.semType,rt.semType)(op),rt.semType =>: rt.semType
) {
  override def priority: Int = super.priority + prio
}
class Binary(rt : RealizedType, path : GlobalName, op : (Any,Any) => Any, prio : Int = 0) extends RealizedOperator(
  path, SynOpType(MitM.under, List(rt.synType,rt.synType),rt.synType), SemanticOperator.Binary(rt.semType,rt.semType, rt.semType)(op),
  rt.semType =>: rt.semType =>: rt.semType
) {
  override def priority: Int = super.priority + prio
}
class BinaryPred(rt : RealizedType, path : GlobalName, op : (Any,Any) => Boolean, prio : Int = 0) extends RealizedOperator(
  path, SynOpType(MitM.under, List(rt.synType,rt.synType),OMS(Math.bool)),
  SemanticOperator.Binary(rt.semType,rt.semType,StandardBool)(op),
  rt.semType =>: rt.semType =>: StandardBool
)

object PosPlus extends Binary(PosLiterals,Math.posplus,(i,j) => i.asInstanceOf[BigInt] + j.asInstanceOf[BigInt],4)
// object NatPlus extends Binary(NatLiterals,Math.natplus,(i,j) => i.asInstanceOf[BigInt] + j.asInstanceOf[BigInt],3)
object IntPlus extends Binary(IntegerLiterals,Math.intplus,(i,j) => i.asInstanceOf[BigInt] + j.asInstanceOf[BigInt],2)
object RealPlus extends Binary(RealLiterals,Math.realplus,(i,j) => i.asInstanceOf[Double] + j.asInstanceOf[Double],1)

object PosTimes extends Binary(PosLiterals,Math.postimes,(i,j) => i.asInstanceOf[BigInt] * j.asInstanceOf[BigInt],4)
// object NatTimes extends Binary(NatLiterals,Math.nattimes,(i,j) => i.asInstanceOf[BigInt] * j.asInstanceOf[BigInt],3)
object IntTimes extends Binary(IntegerLiterals,Math.inttimes,(i,j) => i.asInstanceOf[BigInt] * j.asInstanceOf[BigInt],2)
object RealTimes extends Binary(RealLiterals,Math.realtimes,(i,j) => i.asInstanceOf[Double] * j.asInstanceOf[Double],1)

// object NatLeq extends BinaryPred(RealLiterals,Math.natleq,(i,j) => i.asInstanceOf[BigInt] <= j.asInstanceOf[BigInt],3)
object IntLeq extends BinaryPred(RealLiterals,Math.intleq,(i,j) => i.asInstanceOf[BigInt] <= j.asInstanceOf[BigInt],2)
object RealLeq extends BinaryPred(RealLiterals,Math.realleq,(i,j) => i.asInstanceOf[Double] <= j.asInstanceOf[Double],1)


object RealMinus extends Unary(RealLiterals,Math.realminus,i => 0 - i.asInstanceOf[Double],1)
object IntMinus extends Unary(RealLiterals,Math.intminus,i => 0 - i.asInstanceOf[BigInt],2) /* RealizedOperator(
  Math.realminus,
  SynOpType(List(OMS(Math.real)),OMS(Math.real)),
  SemanticOperator.Unary(StandardDouble,StandardDouble)(i => 0 - i.asInstanceOf[Double]),
  StandardDouble =>: StandardDouble
) */
object RealSqrt extends Unary(RealLiterals,Math.sqrt,i => scala.math.sqrt(i.asInstanceOf[Double]))