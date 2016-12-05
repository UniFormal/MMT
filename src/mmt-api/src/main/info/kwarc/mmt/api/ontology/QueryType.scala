package info.kwarc.mmt.api.ontology

import scala.language.implicitConversions

/** QueryBaseType = Path | Obj | XML | String */
sealed abstract class QueryBaseType(val name : String)

case object PathType extends QueryBaseType("path")
case object ObjType extends QueryBaseType("object")
case object XMLType extends QueryBaseType("xml")
case object StringType extends QueryBaseType("string")

object QueryBaseType {
  def parse(s : String) = List(PathType, ObjType, XMLType, StringType).filter(_.name == s).head
}

/**
  * The different types of return Values of Queries
  */
sealed abstract class QueryType {}

/** a single tuple of base types */
case class TupleQuery(tp: List[QueryBaseType]) extends QueryType {
  def lift : SetTupleQuery = SetTupleQuery(tp)
}

/** a set of tuples */
case class SetTupleQuery(tp: List[QueryBaseType]) extends QueryType {
  def unlift : TupleQuery = TupleQuery(tp)
}

/** a single basetype */
object ElementQuery {
  def apply(b : QueryBaseType): TupleQuery = TupleQuery(List(b))
  def unapply(arg: QueryType): Option[QueryBaseType] = arg match {
    case TupleQuery(b::Nil) => Some(b)
    case _ => None
  }
}

/** a set of single base types */
object SetElementQuery {
  def apply(b : QueryBaseType): SetTupleQuery = SetTupleQuery(List(b))
  def unapply(t: QueryType): Option[QueryBaseType] = t match {
    case SetTupleQuery(List(b)) => Some(b)
    case _ => None
  }
}

/** implicit conversions from / to Query types */
object QueryTypeConversion {
  implicit def qtFromList(bs: List[QueryBaseType]): TupleQuery = TupleQuery(bs)
  implicit def qtFromBase(b: QueryBaseType): TupleQuery = TupleQuery(List(b))
  implicit def tFromBase(b: BaseType): List[BaseType] = List(b)
}
