package info.kwarc.mmt.api.utils

/** a simple API for generating Javascript expressions */
object Javascript {
  abstract class Expression {
    def toJS: String
    def apply(args: Expression*) = Apply(this, args:_*)
  }
  case class Identifier(name: String) extends Expression {
    def toJS = name
    def at(key: Expression) = FieldAccess(this, key)
  }
  
  abstract class JSClass(obj: Expression) {
    def FieldName(name: String) = FieldAccess(obj, Identifier(name))
  }
  case class Variable(name: String) extends Expression {
    def toJS = name
  }
  
  case object JSNull extends Expression {
    def toJS = "null"
  }
  case class JSInt(i: Int) extends Expression {
    def toJS = i.toString
  }
  case class JSBool(b: Boolean) extends Expression {
    def toJS = b.toString
  }
  case class JSString(s: String) extends Expression {
    def toJS = "\"" + StandardStringEscaping(s) + "\""
  }
  
  case class Array(entries: Expression*) extends Expression {
    def toJS = entries.map(_.toJS).mkString("[",",","]")
  }
  case class ArrayClass(array: Expression) extends JSClass(array) {
    val map = FieldName("map")
  }
  
  case class JSObject(entries: (Expression,Expression)*) extends Expression {
    def toJS = entries.map{case (k,v) => k.toJS+":"+v.toJS}.mkString("{",",","}")
  }
  /** accessing field in an array or object */
  case class FieldAccess(obj: Expression, field: Expression) extends Expression {
    def toJS = obj.toJS + "[" + field.toJS + "]"
  }
  case class Function(params: String*)(body: Expression) extends Expression {
    def toJS = s"function(${params.mkString(",")}){${body.toJS}}"
  }
  
  case class Apply(fun: Expression, args: Expression*) extends Expression {
    def toJS = s"${fun.toJS}(${args.map(_.toJS).mkString(", ")})"
  }
  val Alert = Identifier("alert")
  val Log = Identifier("console.log")
  
  implicit def fromInt(i: Int) = JSInt(i)
  implicit def fromBool(b: Boolean) = JSBool(b)
  implicit def fromString(s: String) = JSString(s)
  
  implicit def fromList(l: List[Expression]) = Array(l:_*)
  implicit def toList(a: Array) = a.entries.toList
  
  implicit def fromFunction(fun: Expression => Expression): Expression = {
    val name = "x"  // TODO generate unique name
    Function(name)(fun(Identifier(name)))
  }
  
  case class JSSeq(exprs: Expression*) extends Expression {
    def toJS = exprs.map(_.toJS).mkString("; ")
  }
  case class JSWhile(cond: Expression, body: Expression) extends Expression {
    def toJS = s"while (${cond.toJS}) {${body.toJS}}"
  }
  
}

import Javascript._
object MMTJavascript {
  val showGraph = Javascript.Identifier("me.showGraph")

}