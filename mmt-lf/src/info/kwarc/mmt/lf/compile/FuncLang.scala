package info.kwarc.mmt.lf.compile

case class SyntaxError(msg: String) extends java.lang.Throwable(msg)

sealed abstract class Type
case object KindOfTypes extends Type
case class BuiltinType(e: EXP) extends Type
case class FunctionalType(args: List[EXP], ret: EXP) extends Type
case object ErrorType extends Type

case class Context(vars: List[(String,Type)]) {
   def apply(n: String): Option[Type] = vars.reverse find {_._1 == n} map {_._2}
   def ++(con: Context) = Context(vars ::: con.vars)
}

/** a simple functional language represented as transformation functions from declarations and expressions to strings */
abstract class FuncLang[A] {
   def exp(e: EXP) : A
   def decl(d: DECL) : A
   def prog(ds: List[DECL]) : List[A] = ds map decl
}