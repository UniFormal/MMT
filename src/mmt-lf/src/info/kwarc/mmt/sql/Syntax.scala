package info.kwarc.mmt.sql

import info.kwarc.mmt.api._
import objects._

/**
  *  @param path the MMT name of the table
  *  @param columns sequence of all columns
  *  @param collections sequence of all collections
  */
case class Table(path: MPath, columns: Seq[Column]) {
  /** db name of the table, underscore style */
  def name = path.name.toString
  /** retrieve all columns that are collections */
  def collections = columns.filter(_.collection.isDefined)
}

/**
  *  @param path the MMT name of the column
  *  @param mathType the mathematical type of the column
  *  @param codec the codec expression for en/de-coding functions between them
  *  @param the database type
  *  @param isNullable can be null in the database
  *  @param isPrimaryKey key field (annotated in schema)
  *  @param opaque no meaningful operations on column except for (in)equality  (annotated in schema)
  *  @param isDisplayedByDefault (annotated in schema) whether the column gets displayed in the default view of the result set
  *  later we could add: displayName, description
  */
case class Column(path: GlobalName, mathType: Term, codec: Term, dbtype: SQLSyntax.Type[_],
    isNullable: Boolean, isPrimaryKey: Boolean, opaque: Boolean, isDisplayedByDefault: Boolean,
    collection: Option[CollectionInfo]) {
  /** the db name of the column */
  def name = path.name.toString
}

/**
  *  @param id collection id (only needed for the interface)
  *  @param index column holding the collection index
  *  @param metadata a record object for name, authors, url, other information about the collection; can be JSON
  */
case class CollectionInfo(id: String, index: Column, metadata: Any)

/**
 * 
 */
case class Filter(condition: SQLSyntax.Expr) {
  def toSQL = condition.toString
  def context = condition.columnRefs
}

/** inductive type for SQL queries */
object SQLSyntax {
  
  sealed abstract class Type[U] {
    type underlying = U
    def itself = this.asInstanceOf[Type[underlying]]
  }
  sealed abstract class BaseType[U](s: String) extends Type[U] {
    override def toString = s
    def apply(u: U) = Value(this, u)
  }
  case object IntType extends BaseType[Int]("Int")
  case object BoolType extends BaseType[java.lang.Boolean]("Boolean")
  case object StringType extends BaseType[String]("String")
  case object UUIDType extends BaseType[java.util.UUID]("UUID")
  case object JSONType extends BaseType[utils.JSON]("JSON")
  
  case class ArrayType[U](entryType: Type[U]) extends Type[List[U]] {
    override def toString = s"List[$entryType]"
  }
  
  /** expressions as used in WHERE clause */
  sealed abstract class Expr {
    def columnRefs: List[String]
  }
  sealed abstract class Value(val value: Any) extends Expr {
    override def toString = value.toString
    def columnRefs = Nil
  }
  
  object Value {
    def apply[U](t: Type[U], u: U): Value = t match {
      case IntType => IntVal(u)
      case BoolType => BoolVal(u)
      case StringType => StringVal(u)
      case UUIDType => UUIDVal(u)
      case JSONType => JSONVal(u)
      case ArrayType(tp) => ArrayVal(tp, u)
    }
    def typeOf(v: Value) = v match {
      case v: IntVal => IntType
      case v: BoolVal => BoolType
      case v: StringVal => StringType
      case v: UUIDVal => UUIDType
      case v: JSONVal => JSONType
      case v: ArrayVal[_] => ArrayType(v.tp)
    }
  }
  
  case class IntVal(v: Int) extends Value(v)
  case class StringVal(s: String) extends Value(s)
  case class BoolVal(b: Boolean) extends Value(b)
  case class UUIDVal(u: java.util.UUID) extends Value(u)
  case class JSONVal(j: utils.JSON) extends Value(j)
  
  case class ArrayVal[U](tp: Type[U], elems: List[U]) extends Value(elems)
  
  case class ColumnRef(name: String) extends Expr {
    override def toString = name // TODO escaping
    def columnRefs = List(name)
  }
  case class App(fun: FunOrOp, args: List[Expr]) extends Expr {
    override def toString = {
      fun match {
        case Fun(n) =>
          val argsS = if (args.isEmpty) "" else args.mkString("(", ",", ")")
          n + argsS
        case PostfixOp(n) =>
          val a = args(0)
          s"($a $n)"
        case PrefixOp(n) =>
          val a = args(0)
          s"($n $a)"
        case InfixOp(n) =>
          val (l,r) = (args(0), args(1))
          s"($l $n $r)"
      }
    }
    def columnRefs = args.flatMap(_.columnRefs)
  }

  sealed abstract class FunOrOp {
    def apply(args: Expr*) = App(this, args.toList)
  }

  sealed abstract class UnaryOp extends FunOrOp
  case class PrefixOp(name: String) extends UnaryOp
  case class PostfixOp(name: String) extends UnaryOp
  case class InfixOp(name: String) extends FunOrOp
  case class Fun(name: String) extends FunOrOp
}
