package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import presentation._

abstract class SemiFormalObject extends Content

case class Text(format: String, obj: String) extends SemiFormalObject {
   def toNode = scala.xml.Text(obj)
   override def toString = obj
}
case class XMLNode(obj: scala.xml.Node) extends SemiFormalObject {
   def toNode = obj
   override def toString = obj.toString
}
case class Formal(obj: Term) extends SemiFormalObject {
   def toNode = obj.toNode
   override def toString = obj.toString
}