package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api.{NamespaceMap, ParseError, Path}
import info.kwarc.mmt.api.objects.Obj
import info.kwarc.mmt.api.utils.xml

import scala.collection.mutable.HashSet
import scala.xml.Node

import scala.language.implicitConversions


/** a trait for all concrete data types that can be returned by queries; atomic types are paths and objects */
trait BaseType
case class XMLValue(node: scala.xml.Node) extends BaseType
case class StringValue(string: String) extends BaseType

object BaseType {
  /**
    * Parses a single literal value
    * @param n
    * @return
    */
  def parse(n: Node): BaseType = n match {
    case <object>{o}</object> =>
      Obj.parseTerm(o, NamespaceMap.empty)
    case <xml>{x}</xml> =>
      XMLValue(x)
    case <string>{s}</string> =>
      StringValue(s.text)
    case <uri/> =>
      Path.parse(xml.attr(n, "path"))
    case _ =>
      throw ParseError("Unknown literal type: Please use <object />, <xml />, <string /> or <uri />")
  }
}

/** Wrapper type for the result of a query */
sealed abstract class QueryResult {
  def toNode : scala.xml.Node
}

/**
  * The results of an Element Query: A Tuple of [BaseType]s.
  * @param t Tuple of BaseType Results
  */
case class ElemResult(t: List[BaseType]) extends QueryResult {
  def toNode : scala.xml.Node = <result>{t map {
    case p: Path => <uri path={p.toPath}/>
    case o: Obj => <object xmlns:om="http://www.openmath.org/OpenMath">{o.toNode}</object>
    case x: XMLValue => <xml>{x.node}</xml>
    case s: StringValue => <string>{s.string}</string>
  }}</result>
}

/**
  * The result of a Set Query; A set of ElementResults.
  * @param s Set of ElemResults
  */
case class SetResult(s : HashSet[ElemResult]) extends QueryResult {
  def toNode : scala.xml.Node =
    <results size={s.size.toString}>{s.map(_.toNode)}</results>
}

object QueryResultConversion {
  implicit def SetResult2HashSet(s : SetResult): HashSet[ElemResult] = s.s
  implicit def ElemResult2List(e : ElemResult): List[BaseType] = e.t
}