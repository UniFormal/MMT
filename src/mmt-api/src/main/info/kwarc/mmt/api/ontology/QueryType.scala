package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._

/** QueryBaseType = Path | Obj | XML | String */
sealed abstract class QueryBaseType(val name: String, val path: GlobalName) {
  def toTerm = OMID(path)
}

case object PathType extends QueryBaseType("path", QueryType.pth)

case object ObjType extends QueryBaseType("object", QueryType.obj)

case object XMLType extends QueryBaseType("xml", QueryType.xml)

case object StringType extends QueryBaseType("string", QueryType.str)


/**
  * The different types of return Values of Queries
  */
sealed abstract class QueryType

/** a single tuple of base types */
case class ElementQuery(tp: List[QueryBaseType]) extends QueryType {
  def lift: SetQuery = SetQuery(tp)
}

object ElementQuery {
   def apply(b: QueryBaseType): ElementQuery = ElementQuery(List(b))
}

/** a set of tuples */
case class SetQuery(tp: List[QueryBaseType]) extends QueryType {
  def unlift: ElementQuery = ElementQuery(tp)
}

object SetQuery {
  def apply(b: QueryBaseType): SetQuery = SetQuery(List(b))
}

/** a single basetype */
object ElementQuery1 {
  def apply(b: QueryBaseType): ElementQuery = ElementQuery(List(b))

  def unapply(arg: QueryType): Option[QueryBaseType] = arg match {
    case ElementQuery(b :: Nil) => Some(b)
    case _ => None
  }
}

/** a set of single base types */
object SetQuery1 {
  def apply(b: QueryBaseType): SetQuery = SetQuery(List(b))

  def unapply(t: QueryType): Option[QueryBaseType] = t match {
    case SetQuery(List(b)) => Some(b)
    case _ => None
  }
}


object QueryType {
  private val allBaseTypes = List(PathType, ObjType, XMLType, StringType)

  def parse(s: String): QueryBaseType = allBaseTypes.filter(_.name == s).head

  // the remainder converts between query types and MMT terms; ultimately all queries should be terms

  private val theoryPath = utils.mmt.mmtbase ? "QMT"
  val pth: GlobalName = theoryPath ? "Path"
  val obj: GlobalName = theoryPath ? "Object"
  val xml: GlobalName = theoryPath ? "XML"
  val str: GlobalName = theoryPath ? "String"
  val element: GlobalName = theoryPath ? "ElementQuery"
  val set: GlobalName = theoryPath ? "SetQuery"

  private def baseTypefromTerm(s: Term): QueryBaseType = s match {
    case OMID(x) => allBaseTypes.find(_.path == x).getOrElse(throw ParseError("not a well-formed query type"))
    case _ => throw ParseError("not a well-formed query type")
  }
  def fromTerm(t: Term): QueryType = t match {
    case OMA(OMID(`element`), args) => ElementQuery(args map baseTypefromTerm)
    case OMA(OMID(`set`), args) => SetQuery(args map baseTypefromTerm)
    case _ => throw ParseError("not a well-formed query type")
  }

  def toTerm(tp: QueryType): Term = tp match {
    case ElementQuery(args) => OMA(OMID(element), args.map(_.toTerm))
    case SetQuery(args) => OMA(OMID(set), args.map(_.toTerm))
    case _ => throw ParseError("not a well-formed query type")
  }
}

/** implicit conversions from / to Query types */
object QueryTypeConversion {
  implicit def qtFromList(bs: List[QueryBaseType]): ElementQuery = ElementQuery(bs)

  implicit def qtFromBase(b: QueryBaseType): ElementQuery = ElementQuery(List(b))

  implicit def tFromBase(b: BaseType): List[BaseType] = List(b)
}
