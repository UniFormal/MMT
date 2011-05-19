package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import presentation._

abstract class SemiFormalObject extends Content

case class Source(format: String, obj: String) extends SemiFormalObject {
   def toNode = scala.xml.Text(obj)
   def presentation(lpar: LocalParams) = IsLiteral(StringLiteral(obj))
}
case class Presentation(format: String, obj: scala.xml.Node) extends SemiFormalObject {
   def toNode = obj
   def presentation(lpar: LocalParams) = IsLiteral(XMLLiteral(obj))
}
case class Formal(obj: Term) extends SemiFormalObject {
   def toNode = obj.toNode
   def presentation(lpar: LocalParams) = obj.presentation(lpar)
}