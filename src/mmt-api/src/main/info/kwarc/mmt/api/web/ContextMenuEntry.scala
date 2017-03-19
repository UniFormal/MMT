package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import frontend._
import tiscaf.{HLet, HReqData}
import utils._

case class ContextMenuEntry(label: String, function: Javascript.Expression)

trait ContextMenuProvider extends Extension {
  def getEntries(path: Path): List[ContextMenuEntry]
}

class ContextMenuAggregator extends ServerExtension("menu") {
  def apply(httppath: List[String], query: String, body: Body, session: Session, req: HReqData): HLet = {
     val path = Path.parse(query)
     val entries = controller.extman.get(classOf[ContextMenuProvider]).flatMap(_.getEntries(path))
     val json = JSONObject(entries.map(e => (JSONString(e.label), JSONString(e.function.toJS))))
     Server.JsonResponse(json)
  }
}

/** a simple API for generating Javascript expressions */
object Javascript {
  abstract class Expression {
    def toJS: String
  }
  case class Identifier(name: String) extends Expression {
    def toJS = name
    def apply(args: Expression*) = Apply(name, args:_*)
    def at(key: Expression) = FieldAccess(this, key)
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
  case class JSObject(entries: (Expression,Expression)*) extends Expression {
    def toJS = entries.map{case (k,v) => k.toJS+":"+v.toJS}.mkString("{",",","}")
  }
  /** accessing field in an array or object */
  case class FieldAccess(obj: Expression, field: Expression) extends Expression {
    def toJS = obj.toJS + "[" + field.toJS + "]"
  }
  
  case class Function(params: String*)(body: Expression*) {
    def toJS = s"function(${params.mkString(",")}){${body.mkString("",";",";")}}"
  }
  case class Apply(name: String, args: Expression*) extends Expression {
    def toJS = s"$name(${args.map(_.toJS).mkString(", ")})"
  }
  
  implicit def fromInt(i: Int) = JSInt(i)
  implicit def fromBool(b: Boolean) = JSBool(b)
  implicit def fromString(s: String) = JSString(s)
  
  implicit def fromList(l: List[Expression]) = Array(l:_*)
  implicit def toList(a: Array) = a.entries.toList
}

object MMTJavascript {
  val showGraph = Javascript.Identifier("me.showGraph") 
}