package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api.objects.{OMA, OMID, Term}
import info.kwarc.mmt.api.{GlobalName, NamespaceMap, Path}

import scala.language.implicitConversions

/** QueryBaseType = Path | Obj | XML | String */
sealed abstract class QueryBaseType(val name: String, val path: GlobalName)

case object PathType extends QueryBaseType("path", QueryBaseType.pth)

case object ObjType extends QueryBaseType("object", QueryBaseType.obj)

case object XMLType extends QueryBaseType("xml", QueryBaseType.xml)

case object StringType extends QueryBaseType("string", QueryBaseType.str)

object QueryBaseType {
  private val all = List(PathType, ObjType, XMLType, StringType)

  /** parses a [[String]] into a [[info.kwarc.mmt.api.ontology.QueryBaseType]] */
  def parse(s: String): QueryBaseType = all.filter(_.name == s).head

  /** parses a [[Term]] into a [[info.kwarc.mmt.api.ontology.QueryBaseType]] */
  def parse(s: Term): QueryBaseType = s match {
    case OMID(x) => all.filter(_.path == x).head
  }

  /** generates a term from a [[info.kwarc.mmt.api.ontology.QueryBaseType]] */
  def generate(qb: QueryBaseType): Term = OMID(qb.path)

  /** parses a list of query Base types from a string **/
  def parseList(s: Term): List[QueryBaseType] = s match {
    case OMID(`nil`) => Nil
    case OMA(OMID(`cons`), h :: t :: Nil) => parse(h) :: parseList(t)
  }

  /** generates a term from a list of BaseTypes */
  def generate(l: List[QueryBaseType]): Term = l match {
    case Nil => OMID(nil)
    case h :: t => OMA(OMID(`cons`), generate(h) :: generate(t) :: Nil)
  }

  private val theoryPath = Path.parseM("http://cds.omdoc.org/urtheories?BaseTypes", NamespaceMap.empty)

  val pth: GlobalName = theoryPath ? "Path"
  val obj: GlobalName = theoryPath ? "Object"
  val xml: GlobalName = theoryPath ? "XML"
  val str: GlobalName = theoryPath ? "String"

  val nil: GlobalName = theoryPath ? "emptyBaseTypeList"
  val cons: GlobalName = theoryPath ? "consBaseTypeList"
}

/**
  * The different types of return Values of Queries
  */
sealed abstract class QueryType {}

/** a single tuple of base types */
case class TupleQuery(tp: List[QueryBaseType]) extends QueryType {
  def lift: SetTupleQuery = SetTupleQuery(tp)
}

/** a set of tuples */
case class SetTupleQuery(tp: List[QueryBaseType]) extends QueryType {
  def unlift: TupleQuery = TupleQuery(tp)
}

/** a single basetype */
object ElementQuery {
  def apply(b: QueryBaseType): TupleQuery = TupleQuery(List(b))

  def unapply(arg: QueryType): Option[QueryBaseType] = arg match {
    case TupleQuery(b :: Nil) => Some(b)
    case _ => None
  }
}

/** a set of single base types */
object SetElementQuery {
  def apply(b: QueryBaseType): SetTupleQuery = SetTupleQuery(List(b))

  def unapply(t: QueryType): Option[QueryBaseType] = t match {
    case SetTupleQuery(List(b)) => Some(b)
    case _ => None
  }
}


object QueryType {
  /** parses a term into a query type */
  def parse(t: Term): QueryType = t match {
    case OMA(OMID(`tuple`), h :: Nil) => TupleQuery(QueryBaseType.parseList(h))
    case OMA(OMID(`element`), h :: Nil) => ElementQuery(QueryBaseType.parse(h))
    case OMA(OMID(`setTuple`), h :: Nil) => SetTupleQuery(QueryBaseType.parseList(h))
    case OMA(OMID(`setElement`), h :: Nil) => SetElementQuery(QueryBaseType.parse(h))
  }

  /** generates a term from a querytype */
  def generate(tp: QueryType): Term = tp match {
    case ElementQuery(e) => OMA(OMID(element), QueryBaseType.generate(e) :: Nil)
    case TupleQuery(l) => OMA(OMID(tuple), QueryBaseType.generate(l) :: Nil)

    case SetElementQuery(e) => OMA(OMID(setElement), QueryBaseType.generate(e) :: Nil)
    case SetTupleQuery(l) => OMA(OMID(setTuple), QueryBaseType.generate(l) :: Nil)
  }

  private val theoryPath = Path.parseM("http://cds.omdoc.org/urtheories?QueryTypes", NamespaceMap.empty)

  val tuple: GlobalName = theoryPath ? "TupleQuery"
  val element: GlobalName = theoryPath ? "ElementQuery"

  val setTuple: GlobalName = theoryPath ? "SetTupleQuery"
  val setElement: GlobalName = theoryPath ? "SetElementQuery"

}

/** implicit conversions from / to Query types */
object QueryTypeConversion {
  implicit def qtFromList(bs: List[QueryBaseType]): TupleQuery = TupleQuery(bs)

  implicit def qtFromBase(b: QueryBaseType): TupleQuery = TupleQuery(List(b))

  implicit def tFromBase(b: BaseType): List[BaseType] = List(b)
}
