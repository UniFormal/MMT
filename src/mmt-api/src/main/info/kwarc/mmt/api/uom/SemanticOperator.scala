package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import parser._

/** a Scala-level function between [[SemanticType]]s to be used in a [[RealizedOperator]] */
abstract class SemanticOperator(val tp: SemOpType) extends SemanticObject {
  
  private var types = List(tp)
  def getTypes = types
  
  lazy val arity = tp.arity

  protected def alsoHasType(t: SemOpType) {
    if (tp.arity != arity)
      throw ImplementationError("illegal arity")
    else
      types ::= t
  }
  
  def apply(args: List[Any]): Any
}

object SemanticOperator {
  /** abbreviation for nullary operators */
  case class Value(t: SemanticType)(value: Any) extends SemanticOperator(t) {
    def apply(x:List[Any]) = value
  }
  /** abbreviation for unary operators */
  case class Unary(from: SemanticType, to: SemanticType)(val map: Any => Any) extends SemanticOperator(from =>: to) {
    def apply(x: List[Any]) = map(x(0))
    /** diagram-order composition */
    def compose(that: Unary) = {
      if (to == that.from)
        Unary(from, that.to)(x => this.map(that.map(x)))
      else
        throw ImplementationError("not composable")
    }
  }
  
  /** abbreviation for binary operators */
  case class Binary(from1: SemanticType, from2: SemanticType, to: SemanticType)(f: (Any,Any) => Any) extends SemanticOperator(from1 =>: from2 =>: to) {
    def apply(x: List[Any]) = f(x(0),x(1))
  }
  
  implicit def fromTerm(t: objects.Term): SynOpType = SynOpType(Nil, t)
  implicit def fromSemType(s: SemanticType): SemOpType = SemOpType(Nil,s)
}