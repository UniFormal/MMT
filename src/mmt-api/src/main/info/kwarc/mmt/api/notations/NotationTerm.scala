package info.kwarc.mmt.api.notations

import info.kwarc.mmt.api._
import objects._
import uom._
import parser._

object NotationType {
  def path = utils.mmt.mmtcd ? "notation"
  def term = OMS(path)
}

object NotationLexFunction extends AsymmetricEscapeLexer("nt\"", "\"")

object NotationSemanticType extends Atomic[TextNotation] {
   def asString = "Notations"
   val cls = classOf[TextNotation]
   def fromString(s: String) = TextNotation.parse(s, NamespaceMap.empty)
   override def toString(u: Any) = unapply(u).get.toText
   override def lex = Some(NotationLexFunction)
}

object NotationRealizedType extends RepresentedRealizedType(NotationType.term, NotationSemanticType)
