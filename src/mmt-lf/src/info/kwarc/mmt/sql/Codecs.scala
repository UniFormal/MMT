package info.kwarc.mmt.sql

import info.kwarc.mmt.api._
import objects._
import valuebases._
import uom._

import info.kwarc.mmt.lf._

import SQLSyntax._


case class CommutingProperty(context: Context, inCodecs: List[Term], outCodec: Term, mathOp: GlobalName, mathParams: List[Term], dbOp: SQLSyntax.FunOrOp) {
  def arity = mathParams.length + inCodecs.length
}

trait CodecRuleCallback {
  def buildCodec(t: Term): Codec[Value]
}

class SQLCoder(rules: RuleSet) extends CodecRuleCallback {
  private val codecRules = rules.get(classOf[CodecRule])
  def buildCodec(t: Term) = {
    codecRules.find(_.applicable(t)) match {
      case Some(cr) =>
        cr(this)(t)
      case None =>
        throw NoCodecFound(t)
    }
  }
}

case class NoCodecFound(t: Term) extends Throwable

abstract class CodecRule(val head: GlobalName, arity: Int) extends Rule {
  def applicable(tm: Term) = tm match {
    case ApplyGeneral(this.head, args) => args == arity
    case _ => false
  }
  def apply(coder: CodecRuleCallback)(tm: Term): Codec[Value]
}

/*
abstract class IdentCodec[U](h: GlobalName, rt: RepresentedRealizedType[U], codeType: utils.ConcreteType) extends CodecRule(h, 0) {self =>
  val dbtype = codeType match {
    case utils.StringType => StringType
    case utils.BooleanType => BoolType
    case utils.IntType => IntType
    case utils.FloatType => FloatType
  }
  object codec extends Codec[Value](OMS(head), rt.synType) {
     val codeType = self.codeType
     def encode(t: Term) = t match {
       case rt(u) => Value(dbtype, u)
       case _ => throw CodecNotApplicable
     }
     def decode(v: Value) = {
       if (SQLSyntax.Value.typeOf(v) == dbtype)
         rt(v.value.asInstanceOf[U])
       else
         throw CodecNotApplicable
     }
  }
  def apply(coder: CodecRuleCallback)(tm: Term) = codec
}

//object IntIdentCodec extends IdentCodec(Codecs.IntIdent, new RepresentedRealizedType(OMS(MathData.int), StandardInt), SQLSyntax.IntType)
object BoolIdentCodec extends IdentCodec(Codecs.BoolIdent, new RepresentedRealizedType(OMS(MathData.bool), StandardBool), utils.BooleanType)
object StringIdentCodec extends IdentCodec(Codecs.StringIdent, new RepresentedRealizedType(OMS(MathData.string), StandardString), utils.StringType)
object UUIDIdentCodec extends IdentCodec(Codecs.UUIDIdent, new RepresentedRealizedType(OMS(MathData.uuid), UUIDLiteral), utils.StringType)
*/

class GeneralListAsArrayCodec(ls: GlobalName, nil: GlobalName, cons: GlobalName) extends CodecRule(Codecs._path ? "ListsAsArray", 3) {
  def collectElems(t: Term): List[Term] = t match {
    case Apply(OMS(this.nil),_) => Nil
    case ApplySpine(OMS(this.cons), List(_, hd, tl)) => hd :: collectElems(tl)
    case _ => throw CodecNotApplicable
  }
  def apply(coder: CodecRuleCallback)(tm: Term) = {
    val ApplySpine(_, List(elemTpMa,elemTpDb,cod)) = tm
    val elemCodec = coder.buildCodec(cod).asInstanceOf[Codec[Value]]
    new Codec[Value](tm, Apply(OMS(ls), elemTpMa)) {
      val codeType = utils.ListType(elemCodec.codeType)
      def encode(t: Term) = {
        val elems = collectElems(t)
        val elemsC = elems map elemCodec.encode
        val dbtype = SQLBridge.termToType(elemTpDb)
        type U = dbtype.underlying
        val values = elemsC map {e => e.value.asInstanceOf[U]}
        ArrayVal(dbtype.itself, values.asInstanceOf[List[U]])
      }
      def decode(e: Value) = e match {
        case ArrayVal(bt, elems) =>
          val elemsT = elems map {e => elemCodec.decode(Value(bt, e))}
          elemsT.foldRight[Term](OMS(nil)) {case (next, sofar) => ApplySpine(OMS(cons), elemTpMa, next, sofar)}
        case _ => throw CodecNotApplicable
      }
    }
  }
}

