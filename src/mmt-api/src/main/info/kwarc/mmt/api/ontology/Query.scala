package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import frontend.Controller
import objects._
import documents._
import parser.{ParsingUnit, SourceRef}
import utils._

import scala.xml.Node

/** the expressions of the query language */
sealed abstract class Query

/** isolated sub-query of a query. Carries an optional hint on how to evaluate the query */
case class I(q: Query, hint: Option[String]) extends Query

/** take a subset of the query */
case class Slice(q : Query, from: Option[Int], to: Option[Int]) extends Query

/** takes a specific element from a querySet (negative indexes work also) */
case class Element(q : Query, index: Int) extends Query

/** bound variable represented by a [[info.kwarc.mmt.api.LocalName]] */
case class Bound(varname: LocalName) extends Query

/** component of all elements inside a query */
case class Component(of: Query, component: ComponentKey) extends Query

/** subobject of an object at a certain position */
case class SubObject(of: Query, position: Position) extends Query

/** the set of all elements related to a certain path by a certain relation */
case class Related(to: Query, by: RelationExp) extends Query

/** literal */
case class Literal[T <: BaseType](literal: T) extends Query

/** set of literals */
case class Literals[T <: BaseType](literals: T*) extends Query

/** let binder */
case class Let(varname: LocalName, value: Query, in: Query) extends Query

/** singleton sets */
case class Singleton(e: Query) extends Query

/** the set of all URIs of a certain concept in the MMT ontology */
case class Paths(tp: Unary) extends Query

/** dependency closure of a path */
case class Closure(of: Query) extends Query

/** union of sets */
case class Union(left: Query, right: Query) extends Query

/** union of a collection of sets indexed by a set */
case class BigUnion(domain: Query, varname: LocalName, of: Query) extends Query

/** apply a term to an existing set */
case class Mapping(domain : Query, varname: LocalName, function : Term) extends Query

/** intersection of sets */
case class Intersection(left: Query, right: Query) extends Query

/** difference of sets */
case class Difference(of: Query, without: Query) extends Query

/** comprehension subset of a set */
case class Comprehension(domain: Query, varname: LocalName, pred: Prop) extends Query

/** tupling */
case class Tuple(components: List[Query]) extends Query

/** projection
  * @param index the component to project out (starting from 1) */
case class Projection(of: Query, index: Int) extends Query

/** query that applies an atomic function */
case class QueryFunctionApply(function: QueryFunctionExtension, argument: Query, params: List[String]) extends Query

object Query {
  /**
    * parses a query from a string
    * @param s
    * @param controller
    */
  def parse(s : String, context: Context, controller: Controller): Query = {
    val pu = ParsingUnit(SourceRef.anonymous(s), context ++ Context(QMTPaths.QMT), s, InterpretationInstructionContext())
    val term = controller.objectParser(pu)(ErrorThrower).toTerm
    parse(term)(controller.extman.get(classOf[QueryFunctionExtension]), controller.relman)
  }

  /**
    * Parses a Term representing a Query into a Query object
    * @param t Term representing the query
    * @param queryFunctions List of query functions to parse
    * @param relManager
    * @return
    */
  def parse(t : Term)(implicit queryFunctions: List[QueryFunctionExtension], relManager: RelationalManager) : Query = t match {
    /** the isolated query with an optional hint */
    // TODO: String Literals
    case OMA(OMID(QMTQuery.I), str(name) :: q :: Nil) =>
      I(parse(q), Some(name))

    /** slicing a query result */
    // TODO: Integer literals
    case OMA(OMID(QMTQuery.Slice), str(s) :: str(e) :: q :: Nil) =>
      Slice(parse(q), Some(s.toInt), Some(e.toInt))
    case OMA(OMID(QMTQuery.SliceFrom), str(s) :: q :: Nil) =>
      Slice(parse(q), Some(s.toInt), None)
    case OMA(OMID(QMTQuery.SliceUntil), str(e) :: q :: Nil) =>
      Slice(parse(q), None, Some(e.toInt))

    /** picking a specific element form a Query */
    // TODO: Integer literals
    case OMA(OMID(QMTQuery.Element), str(s) :: q :: Nil) =>
      Element(parse(q), s.toInt)

    /** a bound variable */
    case OMV(name) =>
      Bound(name)

    /** a component of a given query */
    // TODO: No Labels?
    case OMA(OMID(QMTQuery.Component), str(n) :: q :: Nil) =>
      Component(parse(q), ComponentKey.parse(n))

    /** a subobject at a given position */
    // TODO: No Labels?
    case OMA(OMID(QMTQuery.SubObject), str(p) :: q :: Nil) =>
        SubObject(parse(q), Position.parse(p))

    /** related to a specific query by a given relation */
    case OMA(OMID(QMTQuery.Related), q :: by :: Nil) =>
      Related(parse(q), RelationExp.parse(by))

    /** a single literal */
    // TODO: Other kinds of literals
    case OMA(OMID(QMTQuery.Literal), OMID(pth) :: Nil) =>
        Literal(pth)

    /** a set of literals */
    case OMA(OMID(QMTQuery.Literals), lst) =>
      Literals(lst.map(_.asInstanceOf[OMID].path) : _*)

    /** let binder */
    case OMBINDC( OMID(QMTQuery.Let), Context(VarDecl(name, _, _, _, _)), List(bound, query)) =>
        Let(name, parse(bound), parse(query))

    /** a single object */
    case OMA(OMID(QMTQuery.Singleton), q :: Nil) =>
        Singleton(parse(q))

    /** a unary path */
    // TODO: No Labels?
    case OMA(OMID(QMTQuery.Paths), unary :: Nil) =>
      Paths(Unary.parse(unary))

    /** closure */
    case OMA(OMID(QMTQuery.Closure), of :: Nil) =>
      Closure(parse(of))

    /** Union */
    case OMA(OMID(QMTQuery.Union), l :: r :: Nil) =>
        Union(parse(l), parse(r))

    /** Intersection */
    case OMA(OMID(QMTQuery.Intersection), l :: r :: Nil) =>
      Intersection(parse(l), parse(r))

    /** Difference */
    case OMA(OMID(QMTQuery.Difference), l :: r :: Nil) =>
      Difference(parse(l), parse(r))

    /** BigUnion binder */
    case OMBINDC( OMID(QMTQuery.BigUnion), Context(VarDecl(name, _, _, _, _)), List(domain, query)) =>
      BigUnion(parse(domain), name, parse(query))

    /** Mapping binder */
    case OMBINDC( OMID(QMTQuery.Mapping), Context(VarDecl(name, _, _, _, _)), List(domain, function)) =>
      Mapping(parse(domain), name, function)

    /** Comprehension binder */
    case OMBINDC( OMID(QMTQuery.Comprehension), Context(VarDecl(name, _, _, _, _)), List(domain, pred)) =>
      Comprehension(parse(domain), name, Prop.parse(pred))

    /** Tuple */
    case OMA(OMID(QMTQuery.Tuple), components) =>
      Tuple(components.map(c => parse(c)))

    /** Projection */
    //TODO: Integer literals
    case OMA(OMID(QMTQuery.Projection), str(s) :: q :: Nil) =>
      Projection(parse(q), s.toInt)

    /** QueryFunction */
    // TODO: Parameters
      // BUG? queryextensions are only needed for checking, not for parsing?
    case OMA(OMID(QMTQuery.QueryFunctionApply), str(name) :: q :: args) =>
      val fun = queryFunctions.find(_.name == name).getOrElse {
        throw ParseError("illegal function: " + name)
      }
      QueryFunctionApply(fun, parse(q), Nil)

    case _ =>
      throw ParseError("illegal query expression: " + t)
  }

  /**
    * parses a query; infer must be called to sure well-formedness
    * @param n Node to parse
    * @param queryFunctions List of QueryFunctions to use
    * @param relManager RelationalManager to use
    * @return
    */
  def parse(n: Node)(implicit queryFunctions: List[QueryFunctionExtension], relManager: RelationalManager): Query = n match {

    /** the isolated query with an optional hint */
    case <i>{q}</i> =>
      val h = xml.attr(n, "hint", "")
      I(parse(q), if(h != "") Some(h) else None)

    /** slicing a query result */
    case <slice>{q}</slice> =>
      val from = Option(xml.attr(n, "from", "").toInt)
      val to = Option(xml.attr(n, "to", "").toInt)
      Slice(parse(q), from, to)

    /** picking a specific element form a Query */
    case <element>{q}</element> =>
      Element(parse(q), xml.attr(n, "index").toInt)

    /** we have the "index" for backward compatibility */
    case <bound/> =>
      Bound(xml.attrL(n, "index"))

    case <component>{o}</component> =>
      Component(parse(o), ComponentKey.parse(xml.attr(n, "index")))

    case <subobject>{o}</subobject> =>
      SubObject(parse(o), Position.parse(xml.attr(n, "position")))

    case <related>{to}{by}</related> =>
      Related(parse(to), RelationExp.parse(by))

    case <literal>{l}</literal> =>
      Literal(BaseType.parse(l))

    case <literals>{ls@_*}</literals> =>
      Literals(ls.map(BaseType.parse):_*)

    case <let>{v}{in}</let> =>
      Let(xml.attrL(n, "name"), parse(v), parse(in))

    case <singleton>{e}</singleton> =>
      Singleton(parse(e))

    case <uris/> =>
      Paths(relManager.parseUnary(xml.attr(n, "concept")))

    case <closure>{of}</closure> =>
      Closure(parse(of))

    case <union>{l}{r}</union> =>
      Union(parse(l), parse(r))

    case <bigunion>{d}{s}</bigunion> =>
      val dom = xml.attr(n, "type") match {
        case "path" => parse(d)
        case "object" => parse(d)
      }
      BigUnion(dom, xml.attrL(n, "name"), parse(s))

    case <mapping>{d}{s}</mapping> =>
      val dom = parse(d)
      Mapping(dom, xml.attrL(n, "name"), Obj.parseTerm(s, NamespaceMap.empty))

    case <intersection>{l}{r}</intersection> =>
      Intersection(parse(l), parse(r))

    case <difference>{l}{r}</difference> =>
      Difference(parse(l), parse(r))

    case <comprehension>{d}{f}</comprehension> =>
      Comprehension(parse(d), xml.attrL(n, "name"), Prop.parse(f))

    case <tuple>{t@_*}</tuple> =>
      Tuple(t.toList.map(parse))

    case <projection>{t}</projection> =>
      val i = xml.attrInt(n, "index", ParseError)
      Projection(parse(t), i)

    case <function>{a}</function> =>
      val name = xml.attr(n, "name")
      val params = stringToList(xml.attr(n, "param"))
      val arg = parse(a)
      val fun = queryFunctions.find(_.name == name).getOrElse {
        throw ParseError("illegal function: " + name)
      }
      QueryFunctionApply(fun, arg, params)

    case _ =>
      throw ParseError("illegal query expression: " + n)
  }

  /** helper object to match any string-like literal */
  private[ontology] object str {
    def apply(s: String): Term = OML(LocalName(s), None, None, None, None)
    def unapply(tm: Term): Option[String] = tm match {
      // an OML, as parsed from surface syntax
      case OML(s, _, _, _, _) => Some(s.toPath)

      // a known OMLIT with a string value
      // HACK HACK HACK this might match something that isn't actually a string
      case OMLIT(v: String, _) => Some(v)

      // an UnknownOMLIT
      // HACK HACK HACK this might match something that isn't actually a string
      case UnknownOMLIT(v: String, _) => Some(v)
      case _ => None
    }
  }
}
