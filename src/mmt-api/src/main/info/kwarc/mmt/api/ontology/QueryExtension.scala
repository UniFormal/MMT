package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import frontend._
import objects._
import presentation.Presenter

import scala.language.implicitConversions
import QueryResultConversion._
import QueryTypeConversion._
import info.kwarc.mmt.api.libraries.LookupWithNotFoundHandler

import scala.collection.mutable.HashSet

/** Shared class for extentsions that are related to queries */
abstract class QueryExtension(val name: String) extends Extension {
  // must be lazy because controller is initially null
  protected lazy val extman: ExtensionManager = controller.extman

  protected lazy val lup: LookupWithNotFoundHandler = controller.globalLookup
}


/** A QueryFunctionExtension provides the syntax and and semantics for an atomic function of the QMT query language
  *
  * All functions are unary from one BaseType to another. All are actually families of functions, parametrized by an MPath.
  *
  * @param name the name of the functions (names must be globally unique)
  * @param in   the input type
  * @param out  output type
  */
abstract class QueryFunctionExtension(override val name: String, val in: QueryType, val out: QueryType) extends QueryExtension(name) {
  protected def mpath(params: List[String]): MPath = params match {
    case p :: Nil => Path.parseM(p, NamespaceMap.empty)
    case Nil => throw ParseError("paramater expected")
    case _ => throw ParseError("exactly one paramater expected")
  }

  /** the semantics of this function
    *
    * @param argument evaluation of the argument
    * @param params   the MPath this family of functions is parametrized by
    */
  def evaluate(argument: BaseType, params: List[String]): List[BaseType]
}

/**
  * A QueryEvaluationExtension is used by I() clauses to evaluate a part of the query
  *
  * @param name
  */
abstract class QueryEvaluationExtension(override val name: String) extends QueryExtension(name) {
  def evaluate(q: Query, e: QueryEvaluator)(implicit substiution: QueryEvaluator.QuerySubstitution): HashSet[List[BaseType]]
}

import parser._

/** parsing of strings into objects */
//TODO take format argument, currently always "mmt"
class Parse extends QueryFunctionExtension("parse", StringType, ObjType) {
  def evaluate(argument: BaseType, params: List[String]) = {
    val mp = mpath(params)
    argument match {
      case StringValue(s) =>
        val pu = ParsingUnit(SourceRef.anonymous(s), Context(mp), s, NamespaceMap.empty)
        controller.objectParser(pu)(ErrorThrower).toTerm
      case _ => throw ImplementationError("evaluation of ill-typed query")
    }
  }
}

/** type inference relative to a foundation */
class Infer extends QueryFunctionExtension("infer", ObjType, ObjType) {
  def evaluate(argument: BaseType, params: List[String]): List[BaseType] = {
    val mp = mpath(params)
    val found = extman.getFoundation(mp).getOrElse(throw GetError("no applicable type inference engine defined"))
    argument match {
      case OMBIND(QueryEvaluator.free, cont, obj) =>
        found.inference(obj, cont)(lup)
      case t: Term => found.inference(t, Context())(lup)
      case o: Obj => throw GetError("object exists but is not a term: " + o)
      case _ => throw ImplementationError("evaluation of ill-typed query")
    }
  }
}


/** type reconstruction of objects */
class Analyze extends QueryFunctionExtension("analyze", ObjType, List(ObjType, ObjType)) {
  def evaluate(argument: BaseType, params: List[String]): List[BaseType] = {
    val mp = mpath(params)
    argument match {
      case t: Term =>
        val stack = Stack(mp)
        val (tR, tpR) = checking.Solver.check(controller, stack, t).left.toOption.getOrElse {
          throw InvalidObject(t, "term was parsed but did not type-check")
        }
        List(tR, tpR)
      case o: Obj => o // ill-typed
      case _ => throw ImplementationError("evaluation of ill-typed query")
    }
  }
}

/** simplification of objects */
class Simplify extends QueryFunctionExtension("simplify", ObjType, ObjType) {
  def evaluate(argument: BaseType, params: List[String]): List[BaseType] = {
    val mp = mpath(params)
    argument match {
      case OMBIND(QueryEvaluator.free, cont, body) =>
        controller.simplifier(body, Context(mp) ++ cont)
      case o: Obj =>
        controller.simplifier(o, Context(mp))
      case _ => throw ImplementationError("evaluation of ill-typed query")
    }
  }
}

/** XML rendering of objects */
class Present extends QueryFunctionExtension("present", ObjType, XMLType) {
  def evaluate(argument: BaseType, params: List[String]): List[BaseType] = {
    if (params.length != 1) throw ParseError("excatly one parameter expected")
    argument match {
      case o: Obj =>
        val rb = new presentation.StringBuilder
        val pr = extman.get(classOf[Presenter], params(0)).getOrElse(throw ParseError("unknown format"))
        pr(o, None)(rb)
        XMLValue(scala.xml.XML.loadString(rb.get))
      case _ => throw ImplementationError("evaluation of ill-typed query")
    }
  }
}

/** XML rendering of declarations */
class PresentDecl extends QueryFunctionExtension("presentDecl", PathType, XMLType) {
  def evaluate(argument: BaseType, params: List[String]): List[BaseType] = {
    if (params.length != 1) throw ParseError("excatly one parameter expected")
    argument match {
      case p: Path =>
        val rb = new presentation.StringBuilder
        val e = controller.get(p)
        val pr = extman.get(classOf[Presenter], params(0)).getOrElse(throw ParseError("unknown format"))
        pr(e)(rb)
        XMLValue(scala.xml.XML.loadString(rb.get))
      case _ => throw ImplementationError("evaluation of ill-typed query")
    }
  }
}
