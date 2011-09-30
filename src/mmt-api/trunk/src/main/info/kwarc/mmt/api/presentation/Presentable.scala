package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._

/** information provided by a content expression about how it is to be presented */
abstract class PresentationData
/** a content expression provides a key that yields a notation, which is rendered in the context of some components */
case class ByNotation(key : NotationKey, components : ContentComponents, lpar : LocalParams) extends PresentationData
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

case class OPath(parent: Path, component: String)

case class ContentComponents(comps : List[Content], names: List[(String, Int)] = Nil, owner: Option[Path] = None) {
   def apply(i: Int) : Content = apply(NumberedIndex(i))
   def apply(s: String) : Content = apply(NamedIndex(s))
   def apply(i: CIndex): Content = resolve(i).map(x => comps(x)).getOrElse(throw GetError("undefined component: " + i))
   def resolve(i: CIndex) : Option[Int] = i match {
      case NumberedIndex(n) => Some(n)
      case NamedIndex(s) => names.find(_._1 == s).map(_._2)
   }
   def getObjectPath(i:Int) : Option[OPath] = owner flatMap {p => 
      names.find(_._2 == i).map(x => OPath(p, x._1))
   }
   def length = comps.length
}

abstract class CIndex
case class NumberedIndex(i: Int) extends CIndex
case class NamedIndex(s: String) extends CIndex