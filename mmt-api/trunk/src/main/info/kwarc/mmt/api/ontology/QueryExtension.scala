package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import frontend._
import objects._

/** A QueryExtension provides the syntax and and semantics for an atomic function of the QMT query language
 *  
 *  All functions are unary from one BaseType to another. All are actually families of functions, parametrized by an MPath.
 *  
 *  @param name the name of the functions (names must be globally unique)
 *  @param in the input type
 *  @param out output type
 */
abstract class QueryExtension(val name: String, val in: QueryBaseType, val out: QueryBaseType) extends Extension {
   protected lazy val extman = controller.extman // must be lazy because controller is initially null
   protected lazy val lup = controller.globalLookup
   /** the semantics of this function
    *  @param the evaluation of the argument
    *  @param param the MPath this family of functions is parametrized by
    */
   def evaluate(argument: BaseType, param: MPath): BaseType
}

/** parsing of strings into objects */
class Parse extends QueryExtension("parse", StringType, ObjType) {
   def evaluate(argument: BaseType, param: MPath) = {
      argument match { 
         case StringValue(s) =>
           val pu = parser.ParsingUnit(parser.SourceRef.anonymous(s), OMMOD(param), Context(), s) 
           controller.termParser(pu)
         case _ => throw ImplementationError("evaluation of ill-typed query")
      }
   }
}

/** type inference relative to a foundation */
class Infer extends QueryExtension("infer", ObjType, ObjType) {
   def evaluate(argument: BaseType, param: MPath) = {
      val found = extman.getFoundation(param).getOrElse(throw GetError("no applicable type inference engine defined"))
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
class Analyze extends QueryExtension("analyze", ObjType, ObjType) {
   def evaluate(argument: BaseType, param: MPath) = {
      argument match { 
         case t: Term =>
            val stack = Stack(OMMOD(param), Context())
            val (tR, tpR) = Solver.check(controller, stack, t).getOrElse {
               throw InvalidObject(t, "term was parsed but did not type-check")
            }
            tR
         case o: Obj => o
         case _ => throw ImplementationError("evaluation of ill-typed query")
      }
   }
}

/** simplification of objects */
class Simplify extends QueryExtension("simplify", ObjType, ObjType) {
   def evaluate(argument: BaseType, param: MPath) = {
      argument match { 
         case OMBIND(Evaluator.free, cont, body) =>
            controller.uom.simplify(body, OMMOD(param), cont)
         case t: Term =>
            controller.uom.simplify(t, OMMOD(param))
         case c: Context =>
            controller.uom.simplifyContext(c, OMMOD(param))
         case o: Obj =>
            o
         case _ => throw ImplementationError("evaluation of ill-typed query")
      }
   }
}

/** XML rendering of objects */
class Present extends QueryExtension("present", ObjType, XMLType) {
   def evaluate(argument: BaseType, param: MPath) = {
      argument match {
        case o : Obj =>
           val rb = new presentation.XMLBuilder
           (new presentation.StyleBasedPresenter(controller,param)).apply(o, rb)
           XMLValue(rb.get)
        case _ => throw ImplementationError("evaluation of ill-typed query")
     }
   }
}

/** XML rendering of declarations */
class PresentDecl extends QueryExtension("presentDecl", PathType, XMLType) {
   def evaluate(argument: BaseType, param: MPath) = {
      argument match {
        case p: Path => 
           val rb = new presentation.XMLBuilder
           val e = controller.get(p)
           (new presentation.StyleBasedPresenter(controller,param)).apply(e, rb)
           XMLValue(rb.get)
        case _ => throw ImplementationError("evaluation of ill-typed query")
     }
   }
}
