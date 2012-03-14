package info.kwarc.mmt.lf

/** a type (family) with constructors; possible type arguments are erased */
case class Category(name: String, constructors: List[Constructor])
/** a reference to a type (family); possible type arguments are erased */
case class CatRef(target: String) {
   override def toString = target
}

/** constructor for a Category */ 
abstract class Constructor
/** a connective declared in a logic with some argument types
 * we assume that the LF types of all connectives are of the form a1 -> ... an -> c where ai, c are atomic
 */
case class Connective(name: String, arguments: List[CatRef]) extends Constructor
/** a binder with some argument types and a typed bound variable
 * we assume that the LF types of all binders are of the form a1 -> ... an -> (b -> s) -> c where ai, b, s, c are atomic
 */
case class Binder(name: String, arguments: List[CatRef], bound: CatRef, scope: CatRef) extends Constructor
/** a (Connective-like) constructor declared in a pattern body with some argument types */
case class ConstantSymbol(pattern: String, name: String, arguments: List[CatRef]) extends Constructor
/** a bound variable */
case object VariableSymbol extends Constructor

/** a pragmatic declaration with some argument types */
case class Declaration(pattern: String, arguments: List[CatRef])

/** a logic syntax consists of some categories and declarations */
case class LogicSyntax(cats: List[Category], decls: List[Declaration])

/** generates a LogicSyntax for a given LF theory */
class CodeGenerator {
  
}

/** an example backend that transforms a LogicSyntax L into the SML code that represents and validates L-theories */
object SMLEmitter {
   def argsToString(args: List[CatRef]) = args.mkString(" of ", " * ", "")
   def apply(log: LogicSyntax) {
      // the categories
      val cats = log.cats map {case Category(c, cons) =>
         val cases = cons map {
            case Connective(n, args) => n + argsToString(args)
            case Binder(n, args, bound, scope) => n + argsToString(args) + " * string * " + scope
            case ConstantSymbol(p, n, args) => p + "_" + n + argsToString(args)
            case VariableSymbol => c + "_var of string"
         }
         // all constructors as ... | c of a1 * ... an | ... 
         c + " = " + cases.mkString("","\n | ", ".") 
      }
      // all categories as datatype ... and ... and 
      print(cats.mkString("datatype ", "\n\nand ", ""))
      
      // the declarations
      val decls = log.decls map {case Declaration(p, args) =>
        // datatype p = p of a1 ... an
         "datatype " + p + " = " + p + argsToString(args)
      }
      // all declaration types
      print(decls.mkString("", "\n\n", ""))
      // the labeled union type of all declaration types
      val ofdecls = log.decls map {case Declaration(p, _) =>
         p + "_decl of " + p
      }
      print("datatype decl = " + ofdecls.mkString("", " | ", ""))

      // the type of signatures
      print("type sign = decl list\n")
   }
}