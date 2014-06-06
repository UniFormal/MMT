package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import frontend._
import objects._

import QueryTypeConversion._

/** A QueryExtension provides the syntax and and semantics for an atomic function of the QMT query language
 *  
 *  All functions are unary from one BaseType to another. All are actually families of functions, parametrized by an MPath.
 *  
 *  @param name the name of the functions (names must be globally unique)
 *  @param in the input type
 *  @param out output type
 */
abstract class QueryExtension(val name: String, val in: QueryType, val out: QueryType) extends Extension {
   protected lazy val extman = controller.extman // must be lazy because controller is initially null
   protected lazy val lup = controller.globalLookup
   protected def mpath(params: List[String]) = params match {
      case p::Nil => Path.parseM(p, utils.mmt.mmtbase)
      case Nil => throw ParseError("paramater expected")
      case _ => throw ParseError("exactly one paramater expected")
   } 
   /** the semantics of this function
    *  @param the evaluation of the argument
    *  @param param the MPath this family of functions is parametrized by
    */
   def evaluate(argument: BaseType, params: List[String]): List[BaseType]
}

/** parsing of strings into objects */
class Parse extends QueryExtension("parse", StringType, ObjType) {
   def evaluate(argument: BaseType, params: List[String]) = {
      val mp = mpath(params)
      argument match { 
         case StringValue(s) =>
           val pu = parser.ParsingUnit(parser.SourceRef.anonymous(s), OMMOD(mp), Context(), s) 
           controller.textParser(pu)(ErrorThrower)
         case _ => throw ImplementationError("evaluation of ill-typed query")
      }
   }
}

/** type inference relative to a foundation */
class Infer extends QueryExtension("infer", ObjType, ObjType) {
   def evaluate(argument: BaseType, params: List[String]) = {
      val mp = mpath(params)
      val found = extman.getFoundation(mp).getOrElse(throw GetError("no applicable type inference engine defined"))
      argument match { 
         case OMBIND(Evaluator.free, cont, obj) =>
           found.inference(obj, cont)(lup)
         case t: Term => found.inference(t, Context())(lup)
         case o: Obj => throw GetError("object exists but is not a term: " + o)
         case _ => throw ImplementationError("evaluation of ill-typed query")
      }
   }
}


/** type reconstruction of objects */
class Analyze extends QueryExtension("analyze", ObjType, List(ObjType,ObjType)) {
   def evaluate(argument: BaseType, params: List[String]) = {
      val mp = mpath(params)
      argument match { 
         case t: Term =>
            val stack = Stack(OMMOD(mp), Context())
            val (tR, tpR) = checking.Solver.check(controller, stack, t).getOrElse {
               throw InvalidObject(t, "term was parsed but did not type-check")
            }
            List(tR,tpR)
         case o: Obj => o // ill-typed
         case _ => throw ImplementationError("evaluation of ill-typed query")
      }
   }
}

/** simplification of objects */
class Simplify extends QueryExtension("simplify", ObjType, ObjType) {
   def evaluate(argument: BaseType, params: List[String]) = {
      val mp = mpath(params)
      argument match { 
         case OMBIND(Evaluator.free, cont, body) =>
            controller.simplifier(body, OMMOD(mp), cont)
         case o: Obj =>
            controller.simplifier(o, OMMOD(mp), Context())
         case _ => throw ImplementationError("evaluation of ill-typed query")
      }
   }
}

/** XML rendering of objects */
class Present extends QueryExtension("present", ObjType, XMLType) {
   def evaluate(argument: BaseType, params: List[String]) = {
      if (params.length != 1) throw ParseError("excatly one parameter expected")
      argument match {
        case o : Obj =>
           val rb = new presentation.StringBuilder
           val pr = extman.getPresenter(params(0)).getOrElse(throw ParseError("unknown format"))
           pr(o, None)(rb)
           XMLValue(scala.xml.XML.loadString(rb.get))
        case _ => throw ImplementationError("evaluation of ill-typed query")
     }
   }
}

/** XML rendering of declarations */
class PresentDecl extends QueryExtension("presentDecl", PathType, XMLType) {
   def evaluate(argument: BaseType, params: List[String]) = {
      if (params.length != 1) throw ParseError("excatly one parameter expected")
      argument match {
        case p: Path => 
           val rb = new presentation.StringBuilder
           val e = controller.get(p)
           val pr = extman.getPresenter(params(0)).getOrElse(throw ParseError("unknown format"))
           pr(e)(rb)
           XMLValue(scala.xml.XML.loadString(rb.get))
        case _ => throw ImplementationError("evaluation of ill-typed query")
     }
   }
}
