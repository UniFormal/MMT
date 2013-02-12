/**
 * concrete syntax
 * 
 * created by Florian Rabe
 */
package info.kwarc.mmt.lf.compile

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
/** also works as a constructor for formulas, i.e. true : bool, false : bool, has no args in that case */
case class Connective(name: String, arguments: List[CatRef]) extends Constructor
/** a binder with some argument types and a typed bound variable
 * we assume that the LF types of all binders are of the form {x:a} (b x -> s) -> c or (b -> s) -> c where a, b, s, c are atomic
 */
case class Binder(name: String, argument: Option[CatRef], bound: CatRef, scope: CatRef) extends Constructor
/** a (Connective-like) constructor declared in a pattern body with some argument types */
case class ConstantSymbol(pattern: String, name: String, arguments: List[CatRef]) extends Constructor
/** a bound variable */
case object VariableSymbol extends Constructor

/** a pragmatic declaration with some argument types */
case class Declaration(pattern: String, arguments: List[CatRef])

/** a logic syntax consists of some categories (one of which is the category of formulas) and declarations */
case class LogicSyntax(cats: List[Category], form: CatRef, decls: List[Declaration])

/** generates a LogicSyntax for a given LF theory */
class CodeGenerator {
  
}