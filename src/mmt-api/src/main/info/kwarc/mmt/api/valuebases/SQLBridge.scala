package info.kwarc.mmt.api.valuebases

import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._
import objects._

/**
  *  @param name table name, underscore style
  *  @param columns sequence of all columns
  *  @param collections sequence of all collections
  */
case class Table(name: String, columns: Seq[Column], collections: Seq[Collection])

/**
 *  @param name column name, underscore style
 *  @param dbType one of "Boolean", "Int", "String", "UUID" - for now: it does not scale well to lists
 *  @param isNullable can be null in the database
 *  @param isPrimaryKey key field (annotated in schema)
 *  @param opaque no meaningful operations on column except for (in)equality  (annotated in schema)
 *  @param isDisplayedByDefault (annotated in schema) whether the column gets displayed in the default view of the result set
 */
case class Column(name: String, dbType: String, isNullable: Boolean, isPrimaryKey: Boolean, opaque: Boolean, isDisplayedByDefault: Boolean)

/**
  *  @param id collection id (only needed for the interface)
  *  @param index column holding the collection index
  *  @param metadata a record object for name, authors, url, other information about the collection; can be JSON
  */
case class Collection(id: String, index: Column, metadata: Any)

case class Filter(context: List[String], condition: SQLSyntax.Expr) {
  def toSQL = condition.toString
}

object SQLSyntax {
  abstract class Expr
  
  abstract class Value(value: Any) extends Expr {
    override def toString = value.toString
  }
  case class IntVal(v: BigInt) extends Value(v)
  case class StringVal(s: String) extends Value(s)
  case class SQLBoolean(b: Boolean) extends Value(b)
  
  case class ColumnRef(name: String) extends Expr {
    override def toString = name // TODO escaping
  }
  case class App(fun: String, args: List[Expr]) extends Expr {
    override def toString = {
      val argsS = if (args.isEmpty) "" else args.mkString("(", ",", ")")
      fun+argsS
    }
  }
  case class InfixOp(op: String, left: Expr, right: Expr) extends Expr {
    override def toString = s"($left $op $right)"
  }
  case class PrefixOp(op: String, arg: Expr) extends Expr {
    override def toString = s"($op $arg)"
  }
  case class PostfixOp(op: String, arg: Expr) extends Expr {
    override def toString = s"($arg $op)"
  } 
}

class SQLBridge(controller: Controller) {
   def theoryToTable(t: Theory): Table = {
     val cols = t.getConstants map {
       case c: Constant => constantToColumn(c)
     }
     Table()
   }
   def constantToColumn(t: Constant): Column = {
     ???
   }
   def termToFilter(context: Context, t: Term): Filter = {
     // determine type and codec for each subterm, then translate according to commutativity annotations 
     ???
   }
}