package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import presentation._

trait SemiFormalObject extends Content {
   def freeVars : List[LocalName]
   def governingPath = None
   def role = Role_value
}

case class Text(format: String, obj: String) extends SemiFormalObject {
   def components = List(StringLiteral(obj))
   def toNode = <om:text format={format}>{scala.xml.PCData(obj)}</om:text>
   override def toString = "\"" + obj + "\""
   def freeVars : List[LocalName] = Nil
}
case class XMLNode(obj: scala.xml.Node) extends SemiFormalObject {
   def components = List(XMLLiteral(obj))
   def toNode = <om:node>{obj}</om:node>
   override def toString = obj.toString
   def freeVars : List[LocalName] = Nil
}
case class Formal(obj: Term) extends SemiFormalObject {
   def components = List(obj)
   def toNode = obj.toNode
   override def toString = obj.toString
   def freeVars : List[LocalName] = obj.freeVars_
}

trait SemiFormalObjectList {
   val tokens: List[SemiFormalObject]
   def components = tokens
   override def toString = tokens.map(_.toString).mkString("", " ", "")
   def toNodeID(pos : Position) = <om:OMSF>{tokens.map(_.toNode)}</om:OMSF>
}