package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils.Sourceable
import presentation._

trait SemiFormalObject extends Content with Sourceable {
   def freeVars : List[LocalName]
   def governingPath = None
   def toCMLQVars(implicit qvars: Context) : scala.xml.Node
}

case class Text(format: String, obj: String) extends SemiFormalObject {
   def toNode = <om:text format={format}>{scala.xml.PCData(obj)}</om:text>
   override def toString = "\"" + obj + "\""
   def freeVars : List[LocalName] = Nil
   def toCMLQVars(implicit qvars: Context) = <mtext format={format}>{scala.xml.PCData(obj)}</mtext>
   def toSourceString: String = s"Text(${Sourceable(format)}, ${Sourceable(obj)})"
}
case class XMLNode(obj: scala.xml.Node) extends SemiFormalObject {
   def toNode = <om:node>{obj}</om:node>
   override def toString = obj.toString
   def freeVars : List[LocalName] = Nil
   def toCMLQVars(implicit qvars: Context) = obj
   def toSourceString: String = s"XMLNode(${Sourceable(obj)})"
}
case class Formal(obj: Term) extends SemiFormalObject {
   def toNode = obj.toNode
   override def toString = obj.toString
   def freeVars : List[LocalName] = obj.freeVars_
   def toCMLQVars(implicit qvars: Context) = obj.toCMLQVars
   def toSourceString: String = s"Formal(${Sourceable(obj)})"
}

trait SemiFormalObjectList {
   val tokens: List[SemiFormalObject]
   override def toString = tokens.map(_.toString).mkString("", " ", "")
   def toNode = <om:OMSF>{tokens.map(_.toNode)}</om:OMSF>
}