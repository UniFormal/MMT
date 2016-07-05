package info.kwarc.mmt.api.web

import info.kwarc.mmt.api._
import frontend._
import utils._

case class ContextMenuEntry(label: String, function: Javascript.Expression)

trait ContextMenuProvider extends Extension {
  def getEntries(path: Path): List[ContextMenuEntry]
}

class ContextMenuAggregator extends ServerExtension("menu") {
  def apply(httppath: List[String], query: String, body: Body, session: Session) = {
     val path = Path.parse(query)
     val entries = controller.extman.get(classOf[ContextMenuProvider]).flatMap(_.getEntries(path))
     val json = JSONObject(entries.map(e => (JSONString(e.label), JSONString(e.function.toJS))))
     Server.JsonResponse(json)
  }
}

object Javascript {
  abstract class Expression {
    def toJS: String
  }
  case class Identifier(name: String) extends Expression {
    def toJS = name
    def apply(args: Expression*) = Apply(name, args:_*)
  }
  case class Function(params: String*)(body: Expression*) {
    def toJS = s"function(${params.mkString(",")}){${body.mkString("",";",";")}}"
  }
  case class Apply(name: String, args: Expression*) extends Expression {
    def toJS = s"$name(${args.map(_.toJS).mkString(", ")})"
  }
  case object JSNull extends Expression {
    def toJS = "null"
  }
  case class JSInt(i: Int) extends Expression {
    def toJS = i.toString
  }
  case class JSString(s: String) extends Expression {
    def toJS = "\"" + s + "\""
  }
  
  implicit def fromInt(i: Int) = JSInt(i)
  implicit def fromString(s: String) = JSString(s)
}

object MMTJavascript {
  val showGraph = Javascript.Identifier("me.showGraph") 
}