package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._

/** information provided by a content expression about how it is to be presented */
abstract class PresentationData
/** a content expression provides a key that yields a notation, which is rendered in the context of some components */
case class ByNotation(key : NotationKey, components : List[Content], lpar : LocalParams) extends PresentationData
/** a content expressions provides a literal rendering, used, e.g., for names */
case class IsLiteral(l : Literal) extends PresentationData

/** some literal values that IsLiteral may use */
abstract class Literal extends info.kwarc.mmt.api.Content {
   def presentation(lpar : LocalParams) = IsLiteral(this)
}

case class StringLiteral(s : String) extends Literal {
   def toNode = scala.xml.Text(s)
}
case class XMLLiteral(val toNode : scala.xml.Node) extends Literal
case class ValueLiteral(v : AnyVal) extends Literal {
   def toNode = scala.xml.Text(v.toString)
}
/** a special literal with a function similar to None, it produces no output if rendered, and notations can test for it*/
case object Omitted extends Literal {
   def toNode = scala.xml.Text("")
}