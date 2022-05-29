package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import presentation._

trait SemiFormalObject extends Content {
   def freeVars : List[LocalName]
   def boundVars : List[LocalName]
   def paths : List[Path]
   def governingPath = None
   @deprecated("use ContentMathMLPresenter instead")
   def toCMLQVars(implicit qvars: Context) : scala.xml.Node =  ContentMathMLPresenter.applyContext(this)(MathMLContext.forContent(qvars, None))
}

case class Text(format: String, obj: String) extends SemiFormalObject {
   def toNode = <om:text format={format}>{scala.xml.PCData(obj)}</om:text>
   override def toString = "\"" + obj + "\""
   def freeVars : List[LocalName] = Nil
   def boundVars : List[LocalName] = Nil
   def paths = Nil
}
case class XMLNode(obj: scala.xml.Node) extends SemiFormalObject {
   def toNode = <om:node>{obj}</om:node>
   override def toString = obj.toString
   def freeVars : List[LocalName] = Nil
   def boundVars : List[LocalName] = Nil
   def paths = Nil
}
case class Formal(obj: Term) extends SemiFormalObject {
   def toNode = obj.toNode
   override def toString = obj.toString
   def freeVars : List[LocalName] = obj.freeVars_
   def boundVars : List[LocalName] = obj.boundVars_
   def paths = obj.paths_
}

trait SemiFormalObjectList {
   val tokens: List[SemiFormalObject]
   override def toString = tokens.map(_.toString).mkString("", " ", "")
   def toNode = <om:OMSF>{tokens.map(_.toNode)}</om:OMSF>
}
