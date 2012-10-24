package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import presentation._

abstract class SemiFormalObject extends Content {
   def freeVars : List[LocalName]
}

case class Text(format: String, obj: String) extends SemiFormalObject {
   def toNode = scala.xml.Text(obj)
   override def toString = obj
   def freeVars : List[LocalName] = Nil
}
case class XMLNode(obj: scala.xml.Node) extends SemiFormalObject {
   def toNode = obj
   override def toString = obj.toString
   def freeVars : List[LocalName] = Nil
}
case class Formal(obj: Term) extends SemiFormalObject {
   def toNode = obj.toNode
   override def toString = obj.toString
   def freeVars : List[LocalName] = obj.freeVars_
}