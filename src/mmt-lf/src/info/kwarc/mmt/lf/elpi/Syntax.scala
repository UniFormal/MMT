package info.kwarc.mmt.lf.elpi

import info.kwarc.mmt.api._
import utils._

/**
 * implementation of the syntax of the ELPI system by Claudio Sacerdoti Coen and others
 *
 * In concrete syntax, clauses are written with inverted implication and implicit quantification of upper case variables.
 * Upper-case variables are instantiable while lower-case ones are not. (In queries: existential; in assumptions: universal)
 */
object ELPI {

  /** programs */
  case class Program(decls: Decl*) {
    def toELPI : String = decls.map(_.toELPI + "\n\n").mkString
  }

  /** toplevel declarations */
  abstract class Decl {
    def toELPI: String
  }

  case class Data(name: LocalName, tp: Expr, isKind: Boolean) extends Decl {
    def keyword : String = if (isKind) "kind" else "type"
    def toELPI  : String = keyword + " " + name + " " + tp.toELPI(bracket = false) + "."
  }

  case class Rule(rule: Expr) extends Decl {
    def toELPI : String = {
      val ruleS = rule match {
        case Forall(_, scope) =>
          // skip all leading Forall's
          Rule(scope).toELPI
        case Impl(left, right) =>
          // switch to toplevel notation of implication
          right.toELPI(bracket = false) + " :- " + left.toELPI(bracket = false) + "."
        case _ =>
          rule.toELPI(bracket = false) + "."
      }
      ruleS
    }
  }
  
  case class Accumulate(file: File) extends Decl {
    def toELPI = s"""accumulate "$file"."""
  }
  
  case class Comment(s: String) extends Decl {
    def toELPI : String = "/* " + s + " */"
  }
  
  /** expressions (including rules as a special case) */
  abstract class Expr {
    def apply(args: Expr*)           : Expr = if (args.isEmpty) this else Apply(this, args:_*)
    def apply(args: List[LocalName]) : Expr = if (args.isEmpty) this else Apply(this, args.map(ELPI.Variable):_*)
    
    def toELPI(bracket: Boolean = true): String
    override def toString  : String = toELPI()
    
    def isAtomic : Boolean = this match {
      case a: Application => a.fun.isInstanceOf[Variable] && !a.args.last.isInstanceOf[Lambda]
      case _: Variable => true
      case _ => false
    }
  }
  
  /** helper class to optionally wrap brackets around a string */
  case class Bracket(bracket: Boolean) {
    def apply(s: String) : String = {
      val (opB, clB) = if (bracket) ("(", ")") else ("","")
      opB + s + clB
    }
  }

  case class Variable(name: LocalName) extends Expr {
    def toELPI(bracket: Boolean = true) : String = name.toString
  }

  case class Constant(name : LocalName) extends Expr {
    def toELPI(bracket: Boolean = true) : String = name.toString.toUpperCase
  }

  case class Integer(value: Int) extends Expr {
    def toELPI(bracket: Boolean = true) : String = value.toString
  }

  case class Lambda(name: LocalName, scope: Expr) extends Expr {
    def toELPI(bracket: Boolean = true) : String = {
      Bracket(bracket)(s"$name \\ ${scope.toELPI(bracket = false)}")
    }
  }
  object Lambda {
    def apply(names: List[LocalName], scope: Expr): Expr = {
      names match {
        case Nil => scope
        case hd::tl => Lambda(hd, apply(tl, scope))
      }
    }
  }
  
  
  abstract class Application(val fun: Expr, val args: Expr*) extends Expr {
   /* precedence rules: applications binds more strongly than lambda, except for last argument
       f (x \ x) (x \ x)  can be written as f (x \ x) x \ x
   */
   def toELPI(bracket: Boolean = true) : String = {
      val argsI = args.toList.init.map(_.toELPI())
      val last = args.last
      // val bracketLast = !args.last.isInstanceOf[Lambda]
      val bracketLast = true     // actually, the above seems to cause problems if penultimate arg is a variable
      val argsL = last.toELPI(bracketLast)
      val argsE = (argsI ::: List(argsL)).mkString(" ")
      
      Bracket(bracket)(fun.toELPI() + " " + argsE)
    }
  }
  case class Apply(f: Expr, as: Expr*) extends Application(f, as:_*)
  
  /** built-in constants */
  abstract class BuiltInConstant(name: String) extends Variable(LocalName(name))
  object True  extends BuiltInConstant(name = "true")
  object False extends BuiltInConstant(name = "false")

  /** special case for the application of unary operators */
  case class UnaryApply(operator: UnOp, arg: Expr) extends Application(Variable(LocalName(operator.name)), arg) {
    override def toELPI(bracket: Boolean = true) : String = {
      val o = operator.name
      val unbracketed = if (operator.prefix) { s"$o $arg" } else { s"$arg $o" }
      Bracket(bracket)(unbracketed)
    }
  }

  /** built-in unary operators */
  case class UnOp(name: String, prefix: Boolean) {
    def apply(e : Expr): UnaryApply = UnaryApply(this, e)
  }
  object Not extends UnOp(name = "not", prefix =  true)
  
  /** special case for the application of binary operators */
  case class BinaryApply(operator: BinOp, left: Expr, right: Expr) extends Application(Variable(LocalName(operator.name)), left, right) {
     override def toELPI(bracket: Boolean = true) : String = {
       val leftS = left.toELPI((! left.isAtomic) || ((operator.name == "=>" | operator.name == "->") && left.isInstanceOf[BinaryApply]))   // TODO: properly capture case "a, b => c"
       val rightS = right.toELPI(! right.isAtomic)
       Bracket(bracket)(s"$leftS ${operator.name} $rightS")
     }
  }
  
  /** built-in binary operators */
  case class BinOp(name: String) {
    def apply(left: Expr, right: Expr) : BinaryApply = BinaryApply(this, left, right)
    def unapply(e: Expr) : Option[(Expr, Expr)] = e match {
      case ee : Application =>
        if (ee.fun == Variable(LocalName(name)) && ee.args.length == 2) {
          val el = ee.args.toList
          Some((el.head, el.tail.head)) // Elements 0 and 1
        } else {
          // No Binary operations with more than two arguments.
          None
        }
      case _ => None
    }
  }
  object And extends BinOp(name = ","){
    def apply(args: List[Expr]): Expr = args match {
      case Nil => True
      case hd::Nil => hd
      case hd::tl => And(hd, And(tl))
    }
  }
  object Or          extends BinOp(name = ";")
  object Is          extends BinOp(name = "is")
  object Equal       extends BinOp(name = "=")
  object GreaterThan extends BinOp(name = ">")
  object Minus       extends BinOp(name = "-")
  object Impl        extends BinOp(name = "=>") {
    def apply(left: List[Expr], right: Expr): Expr = if (left.isEmpty) right else Impl(And(left), right)
  }
  object Arrow extends BinOp(name = "->")
  
  /** built-in binders */
  abstract class Binder(nameS: String) {
    val name : LocalName = LocalName(nameS)
    def apply(varname: LocalName, scope: Expr) : Apply = Apply(Variable(name), Lambda(varname, scope))
    def apply(context: List[LocalName], scope: Expr) : Expr = context match {
      case Nil => scope
      case hd::tl => apply(hd, apply(tl, scope))
    }
    
    def unapply(e: Expr) : Option[(LocalName, Expr)] = e match {
      case Apply(Variable(this.name), Lambda(varname, scope)) => Some((varname,scope))
      case _ => None
    }
    
  }
  
  object Forall extends Binder(nameS = "pi")
  object Exists extends Binder(nameS = "sigma")
}
