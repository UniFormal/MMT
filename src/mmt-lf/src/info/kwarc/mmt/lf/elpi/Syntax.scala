package info.kwarc.mmt.lf.elpi

import info.kwarc.mmt.api._
import utils._

/**
 * implementation of the syntax of the ELPI system by Claudio Sacerdoti Coen and others
 *  
 * In concrete syntax, clauses are written with inverted implication and implicit quantification of upper case variables.
 * 
 * Upper-case variables are instantiable while lower-case ones are not. (In queries: existential; in assumptions: universal)
 */
object ELPI {

  /** programs */
  case class Program(decls: Decl*) {
    def toELPI = decls.map(_.toELPI + "\n\n").mkString
  }

  /** toplevel declarations */
  abstract class Decl {
    def toELPI: String
  }

  case class Data(name: LocalName, tp: Expr, isKind: Boolean) extends Decl {
    def keyword = if (isKind) "kind" else "type"
    def toELPI = keyword + " " + name + " " + tp.toELPI(false) + "."
  }

  case class Rule(rule: Expr) extends Decl {
    def toELPI = {
      val ruleS = rule match {
        case Forall(x, scope) =>
          // skip all leading Forall's
          Rule(scope).toELPI
        case Impl(left, right) =>
          // switch to toplevel notation of implication
          right.toELPI(false) + " :- " + left.toELPI(false) + "."
        case _ =>
          rule.toELPI(false) + "." 
      }
      ruleS
    }
  }
  
  case class Accumulate(file: File) extends Decl {
    def toELPI = s"""accumulate "$file"."""
  }
  
  case class Comment(s: String) extends Decl {
    def toELPI = "/* " + s + " */"
  }
  
  /** expressions (including rules as a special case) */
  abstract class Expr {
    def apply(args: Expr*) = if (args.isEmpty) this else Apply(this, args:_*)
    def apply(args: List[LocalName]) = if (args.isEmpty) this else Apply(this, args.map(ELPI.Variable(_)):_*)
    
    def toELPI(bracket: Boolean = true): String
    override def toString = toELPI()
    
    def isAtomic = this match {
      case a: Application => a.fun.isInstanceOf[Variable] && !a.args.last.isInstanceOf[Lambda]
      case _: Variable => true
      case _ => false
    }
  }
  
  /** helper class to optionally wrap brackets around a string */
  case class Bracket(bracket: Boolean) {
    def apply(s: String) = {
      val (opB, clB) = if (bracket) ("(", ")") else ("","")
      opB + s + clB
    }
  }

  case class Variable(name: LocalName) extends Expr {
    def toELPI(bracket: Boolean = true) = name.toString 
  }

  case class Integer(value: Int) extends Expr {
    def toELPI(bracket: Boolean = true) = value.toString
  }
  
  case class Lambda(name: LocalName, scope: Expr) extends Expr {
    def toELPI(bracket: Boolean = true) = {
      Bracket(bracket)(s"$name \\ ${scope.toELPI(false)}")
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
   def toELPI(bracket: Boolean = true) = {
      val argsI = args.toList.init.map(_.toELPI(true))
      val last = args.last
      val bracketLast = !args.last.isInstanceOf[Lambda]
      val argsL = last.toELPI(bracketLast)
      val argsE = (argsI ::: List(argsL)).mkString(" ")
      
      Bracket(bracket)(fun.toELPI() + " " + argsE)
    }
  }
  case class Apply(f: Expr, as: Expr*) extends Application(f, as:_*)
  
  /** built-in constants */
  abstract class Constant(name: String) extends Variable(LocalName(name))
  object True extends Constant("true")
  object False extends Constant("false")

  /** special case for the application of unary operators */
  case class UnaryApply(operator: UnOp, arg: Expr) extends Application(Variable(LocalName(operator.name)), arg) {
    override def toELPI(bracket: Boolean = true) = {
      val o = operator.name 
      val unbracketed = if (operator.prefix)
        s"$o $arg"
      else
        s"$arg $o"
      Bracket(bracket)(unbracketed)
    }
  }

  /** built-in unary operators */
  case class UnOp(name: String, prefix: Boolean)
  object Not extends BinOp("not")
  
  /** special case for the application of binary operators */
  case class BinaryApply(operator: BinOp, left: Expr, right: Expr) extends Application(Variable(LocalName(operator.name)), left, right) {
     override def toELPI(bracket: Boolean = true) = {
       val leftS = left.toELPI(! left.isAtomic)
       val rightS = right.toELPI(! right.isAtomic)
       Bracket(bracket)(s"$leftS ${operator.name} $rightS")
     }
  }
  
  /** built-in binary operators */
  case class BinOp(name: String) {
    def apply(left: Expr, right: Expr) = BinaryApply(this, left, right)
    def unapply(e: Expr) = e match {
      case e: Application if e.fun == Variable(LocalName(name)) && e.args.length == 2 => Some((e.args(0),e.args(1)))
      case _ => None
    }
  }
  object And extends BinOp(","){
    def apply(args: List[Expr]): Expr = args match {
      case Nil => True
      case hd::Nil => hd
      case hd::tl => And(hd, And(tl))
    }
  }
  object Or extends BinOp(";")
  object Is extends BinOp("is")
  object GreaterThan extends BinOp(">")
  object Minus extends BinOp("-")
  object Impl extends BinOp("=>") {
    def apply(left: List[Expr], right: Expr): Expr = if (left.isEmpty) right else Impl(And(left), right)
  }
  object Arrow extends BinOp("->")
  
  /** built-in binders */
  abstract class Binder(nameS: String) {
    val name = LocalName(nameS)
    def apply(varname: LocalName, scope: Expr) = Apply(Variable(name), Lambda(varname, scope))
    def apply(context: List[LocalName], scope: Expr): Expr = context match {
      case Nil => scope
      case hd::tl => apply(hd, apply(tl, scope))
    }
    
    def unapply(e: Expr) = e match {
      case Apply(Variable(this.name), Lambda(varname, scope)) => Some((varname,scope))
      case _ => None
    }
    
  }
  
  object Forall extends Binder("pi")
  object Exists extends Binder("sigma")
}
