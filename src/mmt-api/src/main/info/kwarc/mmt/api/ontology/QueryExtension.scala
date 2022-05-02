package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import frontend._
import objects._
import documents._
import presentation.Presenter
import QueryResultConversion._
import QueryTypeConversion._
import info.kwarc.mmt.api.libraries.LookupWithNotFoundHandler
import info.kwarc.mmt.api.uom.SimplificationUnit

import scala.collection.mutable.HashSet

/** Extension that allow for the evaluation of (a subset of) queries */
abstract class QueryExtension(val name: String) extends Extension {
  // must be lazy because controller is initially null
  protected lazy val extman: ExtensionManager = controller.extman

  protected lazy val lup: LookupWithNotFoundHandler = controller.globalLookup

  /**
    * Evaluates a single Query
    *
    * @param q           Query to evaluate
    * @param e           A QueryEvaluator to use for recursive queries
    * @param substitution Substiution (Context) to apply QueryEvaluation in
    * @return
    */
  def evaluate(q: Query, e: QueryEvaluator)(implicit substitution: QueryEvaluator.QuerySubstitution): HashSet[List[BaseType]]
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


  def evaluate(q: Query, e: QueryEvaluator)(implicit substiution: QueryEvaluator.QuerySubstitution): HashSet[List[BaseType]] = {
    val (subquery, params) = q match {case QueryFunctionApply(f, a, p) if f.name == name => (a, p)}

    /** evaluate for i.head is enough by precondition */
    e.evalSet(subquery).map(i => evaluate(i.head, params))
  }
}

import parser._

/** parsing of strings into objects */
//TODO take format argument, currently always "mmt"
class Parse extends QueryFunctionExtension("parse", StringType, ObjType) {
  def evaluate(argument: BaseType, params: List[String]) = {
    val mp = mpath(params)
    argument match {
      case StringValue(s) =>
        val pu = ParsingUnit(SourceRef.anonymous(s), Context(mp), s, InterpretationInstructionContext())
        controller.objectParser(pu)(ErrorThrower).toTerm
      case _ => throw ImplementationError("evaluation of ill-typed query")
    }
  }
}

/** type inference relative to a theory */
class Infer extends QueryFunctionExtension("infer", ObjType, ObjType) {
  def evaluate(argument: BaseType, params: List[String]): List[BaseType] = {
    val mp = mpath(params)
    val outerCon = Context(mp)
    val (context, term) = argument match {
      case OMBIND(QueryEvaluator.free, cont, obj) => (outerCon ++ cont, obj)
      case t: Term => (outerCon, t)
      case o: Obj => throw LocalError("object has no type: " + o)
      case _ => throw ImplementationError("evaluation of ill-typed query")
    }
    checking.Solver.infer(controller, context, term, None).getOrElse {throw LocalError("error during type inference")}
  }
}


/** type reconstruction of objects */
class Analyze extends QueryFunctionExtension("analyze", ObjType, List(ObjType, ObjType)) {
  def evaluate(argument: BaseType, params: List[String]): List[BaseType] = {
    val mp = mpath(params)
    argument match {
      case t: Term =>
        val stack = Stack(mp)
        val (tR, tpR) = checking.Solver.check(controller, stack, t).left.getOrElse {
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
        controller.simplifier(body, SimplificationUnit(Context(mp) ++ cont, false, true))
      case o: Obj =>
        controller.simplifier(o, SimplificationUnit(Context(mp), false, true))
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
        val pr = extman.get(classOf[Presenter], params.head).getOrElse(throw ParseError("unknown format"))
        pr(e)(rb)
        XMLValue(scala.xml.XML.loadString(rb.get))
      case _ => throw ImplementationError("evaluation of ill-typed query")
    }
  }
}
