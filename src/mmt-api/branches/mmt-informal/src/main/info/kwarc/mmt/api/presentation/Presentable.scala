package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import objects._

/** information provided by a content expression about how it is to be presented */
abstract class PresentationData
/** a content expression provides a key that yields a notation, which is rendered in the context of some components */
case class ByNotation(key : NotationKey, components : ContentComponents, lpar : LocalParams) extends PresentationData

/** instances of Literal are not so much content items themselves but the
 *  terminal symbols that are produced when rendering content items
 *  Thus, they are the leafs of the concrete syntax tree. */
abstract class Literal extends Content {
   /** Nil */
   def components = Nil
   /** None */
   def governingPath = None
   /** role is null */
   def role = null
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

/** Wrapper around components that permits named access of components */
case class ContentComponents(comps : List[Content], names: List[(String, Int)] = Nil, owner: Option[Path] = None, obj: Option[Obj] = None) {
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