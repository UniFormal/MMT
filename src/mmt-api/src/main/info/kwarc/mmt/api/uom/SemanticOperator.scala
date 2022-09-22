package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import parser._

/** a Scala-level value to be used in a [[RealizedValue]] */
case class SemanticValue(tp: SemanticType, value: Any) extends SemanticObject

/** a Scala-level function between [[SemanticType]]s to be used in a [[RealizedOperator]] */
abstract class SemanticOperator(val tp: SemOpType) extends SemanticObject {
  private var types = List(tp)
  def getTypes = types
  lazy val arity = tp.arity
  protected def alsoHasType(t: SemOpType): Unit = {
      types ::= t
  }
  /** basic type checking */
  override def init: Unit = {
    if (types.exists(_.arity != arity))
      throw ImplementationError("types of semantic operator do not have the same arity")
  }
  /**
   * the implementation of the operator
   * pre: args.length == arity
   */
  def apply(args: List[Any]): Any
}

/** adds a Scala-style unapply method */
trait Invertible {
  /**
   * the implementation of the inverse
   *
   * @param args arguments of the function (known or unknown)
   * @param result result of function application
   * @return true if result == this(args) can be uniquely solved, and all unknown arguments were filled in; false if there is no solution; None if inconclusive
   *
   * pre: args.length == arity
   */
  def invert(args: List[UnapplyArg], result: Any): Option[Boolean]
}

/** used to pass the arguments to the unapply method of a [[SemanticOperator]] */
sealed abstract class UnapplyArg
case class KnownArg(value: Any, pos: Int) extends UnapplyArg
/** an unknown argument that unapply must solve by calling solve */
class UnknownArg(val tp: SemanticType, val pos: Int) extends UnapplyArg {
  private var value: Option[Any] = None
  def solve(a: Any) = {
    if (tp.valid(a)) {
      value = Some(tp.normalform(a))
      true
    } else
      false
  }
  def getSolution = value
}

object SemanticOperator {  
  /** abbreviation for unary operators */
  class Unary(val from: SemanticType, val to: SemanticType, val map: Any => Any) extends SemanticOperator(from =>: to) {
    def apply(x: List[Any]) = map(x(0))
    /** diagram-order composition */
    def compose(that: Unary) = {
      if (to == that.from)
        Unary(from, that.to)(x => this.map(that.map(x)))
      else
        throw ImplementationError("not composable")
    }
  }
  object Unary {
    def apply(f: SemanticType, t: SemanticType)(m: Any => Any): Unary = new Unary(f,t,m)
  }
  class InvertibleUnary(f: SemanticType, t: SemanticType, m: Any => Any, val imap: Any => Option[Any]) extends Unary(f,t,m) with Invertible {
    def invert(args: List[UnapplyArg], res: Any) = imap(res) match {
      case Some(a) => args.head match {
        case KnownArg(v,_) => Some(a == v)
        case u: UnknownArg => Some(u.solve(a))
      }
      case None => Some(false)
    }
  }

  /** abbreviation for binary operators */
  class Binary(val from1: SemanticType, val from2: SemanticType, val to: SemanticType, map: (Any,Any) => Any) extends SemanticOperator(from1 =>: from2 =>: to) {
    def apply(x: List[Any]) = map(x(0),x(1))
  }
  object Binary {
    def apply(f1: SemanticType, f2: SemanticType, t: SemanticType)(m: (Any,Any) => Any): Binary = new Binary(f1,f2,t,m)
  }
  abstract class InvertibleBinary(f1: SemanticType, f2: SemanticType, t: SemanticType, m: (Any,Any) => Any) extends Binary(f1,f2,t,m) with Invertible {
    def invertLeft(right: Any, res: Any): Option[Any]
    def invertRight(right: Any, res: Any): Option[Any]
    def invert(args: List[UnapplyArg], res: Any) = args match {
      case List(KnownArg(x,_),KnownArg(y,_)) => Some(m(x,y) == res)
      case List(KnownArg(x,_),u: UnknownArg) => invertRight(x,res) map {y => u.solve(y)}
      case List(u: UnknownArg,KnownArg(y,_)) => invertLeft(y, res) map {x => u.solve(x)}
      case List(u: UnknownArg,v: UnknownArg) => None
    }
  }
  trait Commutative {self: InvertibleBinary =>
    def invertRight(x: Any, r: Any) = invertLeft(x,r)
  }
}
