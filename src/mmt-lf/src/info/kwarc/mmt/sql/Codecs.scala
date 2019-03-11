package info.kwarc.mmt.sql

import info.kwarc.mmt.api._
import objects._
import valuebases._

import info.kwarc.mmt.lf._

abstract class SQLCodec(exp: Term, val dbtype: SQLSyntax.Type[_], tp: Term) extends Codec[SQLSyntax.Expr](exp, tp)
abstract class SQLCodecOperator(id: GlobalName, tp: GlobalName) extends CodecOperator[SQLSyntax.Expr,SQLCodec](id, tp)

case class CommutingProperty(context: Context, inCodecs: List[Term], outCodec: Term, mathOp: GlobalName, mathParams: List[Term], dbOp: SQLSyntax.FunOrOp) {
  def arity = mathParams.length + inCodecs.length
}
