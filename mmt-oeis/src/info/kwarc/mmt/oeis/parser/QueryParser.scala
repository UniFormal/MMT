package info.kwarc.mmt.oeis.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, JavaTokenParsers}
import scala.collection.mutable
import scala.xml.Elem

/**
 * Created by enxhi on 4/3/15.
 */


case class QVar(expr : Expression) extends Expression{
  def present : String = expr.present
  def toNode(implicit theory : String) : Elem = <QVAR>{expr.toNode}</QVAR>
  def clear = this
  override def toString = "QVar(" + expr.toString +")"
}


class QueryParser extends JavaTokenParsers with PackratParsers {
  var variables = new mutable.HashSet[String]()
  var functions = new mutable.HashSet[String]()

  override val skipWhitespace = true


  def postProcess(expression : Expression) : Expression = expression match{
    case Func(name, args) if(variables.contains(name) && args.args.length == 1) => Mul(Var(name)::args.args.head::Nil)
    case Func(name, args) => Func(name, postProcess(args) match {case a : ArgList => a})
    case FuncR(name, args) => FuncR(name, postProcess(args) match {case a : ArgList => a})
    case ArgList(args) =>ArgList(args.map(x=>postProcess(x)))
    case Mul(args) =>{
      val mul =  args.map(x=>postProcess(x)).foldRight(Mul(Nil))(varToFunc)
      if(mul.expr.length == 1){
        mul.expr.head
      }else{
        mul
      }
    }
    case Sub(args) => Sub(args.map(x=>postProcess(x)))
    case Add(args) => Add(args.map(x=>postProcess(x)))
    case Div(args) => Div(args.map(x=>postProcess(x)))
    case Power(base,p) => Power(postProcess(base),postProcess(p))
    case Iters(name,from,to,on) => Iters(name, from match {case Some(a) => Some(postProcess(a)); case _ => None},
      to match {case Some(a) => Some(postProcess(a)); case _ => None}, postProcess(on))
    case Factorial(expr) => Factorial(postProcess(expr))
    case Equation(cmp, left,right) => Equation(cmp, postProcess(left), postProcess(right))
    case Neg(expr) => Neg(postProcess(expr))
    case QVar(expr) => QVar(postProcess(expr))
    case x => x
  }

  def varToFunc(left : Expression, right : Mul)  : Mul = (left,right) match{
    case (x: Var, y: Mul) if(functions.contains(x.name)) =>{
      val func = Func(x.name, ArgList(y.expr.head::Nil))
      Mul(func::y.expr.tail)
    }
    case (x:Expression, Mul(expr)) => Mul(x::expr)
    case _ => throw new Exception("Function wrongly applied to foldr!")
  }


  def transformSignedTerm(base : Expression, expr : Expression) : Expression = {
    (base, expr) match {
      case (Add(e1), Adder(e2)) => Add(e1 :+ e2)
      case (Add(e1), Subber(e2)) => Sub(Add(e1) :: e2 :: Nil)
      case (Sub(e1), Adder(e2)) =>  Add(Sub(e1) :: e2 :: Nil)
      case (Sub(e1), Subber(e2)) => Sub(e1 :+ e2)
//      case (Func(a,b), right) if a.equals("<=") => Func(a,ArgList(b.args.drop(1) :+ transformSignedTerm(b.args.last, right)))
      case (e1, Subber(e2)) => Sub(e1 :: e2 :: Nil)
      case (e1, Adder(e2)) => Add(e1 :: e2 :: Nil)
//      case (left, right) => Func("<=",ArgList(left :: right :: Nil))

    }
  }

  def addVar(variable : String, throwable : Boolean = true) = {
    if(functions.contains(variable)){
      if(throwable)
        throw new Exception("Inconsistent type at "+variable)
      else
        functions -= variable
    }

    variables += variable
  }

  def addFunc(function : String, throwable : Boolean = true) = {
    if(variables.contains(function)){
      if(throwable)
        throw new Exception("Inconsistent type at "+function)
      else
        variables -= function

    }

    functions += function
  }

  def applyFunctionsInOrder(exprs : List[Expression], funcs : List[(Expression,Expression)=>Expression]) : Expression = {
    (exprs, funcs) match{
      case (e1::e2::rst, f1::rf) => applyFunctionsInOrder(f1(e1,e2)::rst, rf)
      case (expr::Nil,Nil) => expr
      case _ => throw new Exception("Number of functions doesn't match!")
    }
  }
  //
  //  class Wrapper[E](div : List[E])
  //
  //  case class MultDivWrapper(div : List[((Expression,Expression)=>Expression)~Expression]) extends Wrapper(div){
  //    def unapply(divs : List[((Expression,Expression)=>Expression)~Expression]) : Option[List[((Expression,Expression)=>Expression)~Expression]] = {
  //      Some(divs)
  //    }
  //  }
  //
  //  case class DivWrapper(div : List[Expression]) extends Wrapper(div){
  //    def unapply(divs : List[Expression]) : Option[List[Expression]] = Some(divs)
  //  }

  lazy val reference : Regex = "A\\d{6}".r
  lazy val variable : PackratParser[String] = "\\w{1,4}\\b\\d{0,1}".r | "\\w+\\b(?=\\()(?!\\d+)(?=\\))".r
  lazy val number : Regex = """(\d+(\.\d+)?)""".r
  lazy val sum : PackratParser[String] = "SUM" | "Sum" | "sum" | "Product" | "product" | "PRODUCT" | "limit" | "Limit" | "lim"
  lazy val comparison : PackratParser[(Expression,Expression) => Expression] =
    ("~" | "<>" | "<=" | ">=" | "==" | ">" | "=" | "<" | "->" | ":=" | "=:") ^^{
      x => {(left: Expression, right : Expression) => Equation(x, left, right)}
  }

  lazy val plusminus: PackratParser[String] = "+" | "-"
  lazy val multdiv: PackratParser[(Expression,Expression) => Expression] = "*" ^^{
    _ => (x:Expression, y:Expression) => (x,y) match {
      case (x: Mul, y: Expression) => Mul(x.expr :+ y)
      case (x: Expression, y: Expression) => Mul(List(x, y))
    }
  } |
    "/" ^^ {
      _ => (x:Expression, y:Expression) => (x,y) match {
        case (x: Div, y: Expression) => Div(x.expr :+ y)
        case (x: Expression, y: Expression) => Div(List(x, y))
      }
    }

  lazy val function: PackratParser[String] = "floor" | "ceiling" | "ceil" | "sqrt" |"log" | "sinh" |"sin" | "cosh"| "cos" | "sqrt" | "tan" |
    "tg" | "ctg" | "deg" | "binomial" | "numerator" | "exp" | "phi" | "[A-Z]+[a-z_]*\\b(?!\\d)".r |
    "[a-z_]{3,}[a-zA-Z]*\\b(?!\\d)".r /*| "[a-z_]{1}\\b(?!\\d)".r*/

  lazy val constant: PackratParser[String] = "Pi" | "pi" | "infinity" | "infty"

  lazy val expression : PackratParser[Expression] =
    c_expression~rep(comparison~c_expression) ^^ {
      case expr~listexpr if (listexpr.length !=0) => {
        applyFunctionsInOrder( expr :: listexpr.map(_._2), listexpr.map(_._1) )
      }
      case expr~listexpr => expr
    }

  lazy val c_expression : PackratParser[Expression] = signed_term~rep(sum_op) ^^ {
    case (fctr : Expression)~(divs : List[Expression]) if divs.length !=0 => divs.foldLeft(fctr)(transformSignedTerm)
    case (fctr : Subber)~(divs) => Neg(fctr.expr)
    case (fctr : Adder)~(divs) => fctr.expr
    case (fctr)~(divs) => fctr
  }


  lazy val sum_op : PackratParser[Expression] =
    "?"~>sum_op_n ^^ {x => QVar(x)} |
      sum_op_n ^^ {x => x}

  lazy val sum_op_n : PackratParser[Expression] =
    plusminus~term~opt("!") ^^ {
      case "-"~(fctr : Expression)~Some("!") => Subber(Factorial(fctr))
      case "-"~(fctr : Expression)~None => Subber(fctr)
      case "+"~(fctr : Expression)~Some("!") => Adder(Factorial(fctr))
      case "+"~(fctr : Expression)~None => Adder(fctr)
    }

  lazy val signed_term : PackratParser[Expression] =
    "?"~>signed_term_n ^^ {x => QVar(x)} |
      signed_term_n ^^ {x => x}


  lazy val signed_term_n : PackratParser[Expression] =
    plusminus~term~opt("!") ^^ {
      case "-"~(fctr : Expression)~Some("!") => Subber(Factorial(fctr))
      case "-"~(fctr : Expression)~None => Subber(fctr)
      case "+"~(fctr : Expression)~Some("!") => Adder(Factorial(fctr))
      case "+"~(fctr : Expression)~None => Adder(fctr)
    }|
      term~opt("!") ^^ {
        case (a : Expression)~Some("!") => Factorial(a)
        case (a : Expression)~None => a
      }

  lazy val term : PackratParser[Expression] =
    number ~ unsigned_factor~opt(unsigned_factor) ^^ {
      case (no )~(expr : Expression)~Some(expr1 : Expression) => Mul(Num(no.toDouble)::expr::expr1::Nil)
      case (no )~(expr : Expression)~None => Mul(Num(no.toDouble)::expr::Nil)
    } |
      unsigned_factor~rep(lazy_multiply | multiply) ^^ {
        case (fctr : Expression)~(divs) if(divs.length !=0 ) =>
          applyFunctionsInOrder(fctr :: divs.collect({
            case x : (Any~Expression)  => x._2
            case x : Expression => x
          }), divs.collect({
            case x : (((Expression,Expression)=>Expression)~Expression) => x._1
            case x => {
              (x: Expression, y: Expression) => (x, y) match {
                case (x: Var, y: ArgList) => {
                  addFunc(x.name)
                  Func(x.name, y)
                }
                //In case there is var(var) consider the first var to be a function
                case (x: Var, y: Var) =>{
                  if(variables.contains(x.name))
                    Mul(x :: y :: Nil)
                  else {
                    addFunc(x.name)
                    Func(x.name, ArgList(List(y)))
                  }
                }
                case (x: Var, y: Num) =>{
                  addFunc(x.name)
                  Func(x.name, ArgList(List(y)))
                }
                case (x: Var, y: Expression) =>{
                  if(variables.contains(x.name))
                    Mul(x :: y :: Nil)
                  else
                    Func(x.name, ArgList(List(y)))
                }
                case (x: Mul, y: Expression) => Mul(x.expr :+ y)
                case (x: Expression, y: Expression) => Mul(List(x, y))
              }
            }
          }))
        case (fctr : Var)~(divs) => addVar(fctr.name); fctr
        case (fctr : Expression)~(divs) => fctr
      }

  lazy val multiply : PackratParser[((Expression,Expression)=>Expression)~Expression] =
    multdiv~signed_factor^^ {x => x}

  lazy val lazy_multiply : PackratParser[Expression] =
    unsigned_factor ^^ {x => x}

  lazy val signed_factor : PackratParser[Expression] =
    "?"~>signed_factor_n ^^ {x=>QVar(x)} |
      signed_factor_n ^^ {x=>x}

  lazy val signed_factor_n : PackratParser[Expression] =
    plusminus~factor~opt("!") ^^ {
      case "-"~(fctr : Expression)~Some("!") => Neg(Factorial(fctr))
      case "-"~(fctr : Expression)~None => Neg(fctr)
      case "+"~(fctr : Expression)~None=> fctr
      case "+"~(fctr : Expression)~Some("!") => Factorial(fctr)
    } |
      unsigned_factor ^^ {x=>x}

  lazy val unsigned_factor : PackratParser[Expression] =
    "?"~>unsigned_factor_n ^^ { x => QVar(x)} |
      unsigned_factor_n ^^ {x => x}

  lazy val unsigned_factor_n : PackratParser[Expression] =
    factor~opt("!") ^^ {
      case (a:Expression)~None => a
      case a ~ Some(b) => Factorial(a)
    }

  lazy val factor : PackratParser[Expression] =
    argument~opt("^"~signed_factor~opt(argument)) ^^ {
      case (a : Var)~Some("^"~(signed : Expression)~Some(arguments : ArgList)) =>{
        addFunc(a.name)
        Power(Func(a.name,arguments),signed)
      }
      case (a : ArgList)~Some("^"~(signed : Expression)~None) => Power(a.args.head, signed)
      case (a : Expression)~Some("^"~(signed : Expression)~None) => Power(a,signed)
      case (a : ArgList)~None if(a.args.length == 1) => a.args.head
      case (a : Expression)~None => a
    }

  //  lazy val factor_op : PackratParser[Expression] = "^"~signed_factor | ""

  lazy val argument : PackratParser[Expression] =
    not(sum)~>opt(function)~argument ^^ {
      case Some(a : String)~(b : ArgList)  =>{
        if(variables.contains(a) && b.args.length == 1){
          Mul(List(Var(a), b.args.head))
        }else {
          if(b.args.length > 1){
            addFunc(a)
          }

          Func(a, b)
        }
      }
      case Some(a : String)~(b : Expression) =>{
        if(variables.contains(a)){
          Mul(Var(a)::b::Nil)
        }else {
          Func(a, ArgList(b :: Nil))
        }
      }
      //no other chance - not checking on purpose because if there is a big magnetic field from sun that disturbs it i'll be the first to know
      case None~(b: ArgList) if(b.args.length == 1) => b.args.head
      case None~(b: Expression) => b
    }  |
      reference~argument ^^{
        case (ref : String)~(args : ArgList)  => FuncR(SeqReference(ref), args)
        case (ref : String)~(arg : Expression) => FuncR(SeqReference(ref), ArgList(arg::Nil))
      } |
      (sum)~(opt("_")~>"{"~>expression)~(".."~>expression<~"}")~expression ^^{
        case (iter : String)~(from )~(to : Expression)~(on : ArgList) => Iters(iter, Some(from), Some(to), on.args.head)
        case (iter : String)~(from )~(to : Expression)~(on : Expression) => Iters(iter, Some(from), Some(to), on)
      } |
      (sum )~(opt("_")~>"{"~>expression<~"}")~opt("^"~>"{"~>expression<~"}")~(expression) ^^{
        case (iter : String)~(from : Expression)~(e : Option[Expression])~(on : ArgList) =>
          Iters(iter,Some(from),e,on.args.head)
        case (iter : String)~(from : Expression)~(e : Option[Expression])~(on : Expression) =>
          Iters(iter,Some(from),e,on)
      } |
      (sum)~(opt("_")~>"("~>expression)~(".."~>expression<~("," | ";"))~(expression<~")") ^^{
        case (iter : String)~(from )~(to : Expression)~(on : Expression) => Iters(iter, Some(from), Some(to), on)
      } |
      (sum)~(opt("_")~>"{"~>expression<~("," | ";"))~(expression)~(".."~>expression<~"}") ^^{
        case (iter : String)~(on )~(from : Expression)~(to : Expression) => Iters(iter, Some(from), Some(to), on)
      } |
      (sum)~(opt("_")~>"{"~>expression<~("," | ";"))~(expression<~"}")~(expression) ^^{
        case (iter : String)~(from )~(to : Expression)~(on : Expression) => Iters(iter, Some(from), Some(to), on)
      } |
      (sum)~(opt("_")~>"("~>expression<~("," | ";"))~(expression)~(".."~>expression<~")") ^^{
        case (iter : String)~(on )~(from : Expression)~(to : Expression) => Iters(iter, Some(from), Some(to), on)
      } |
      (sum)~(opt("_")~>"("~>expression<~")") ^^{
        case (iter : String)~(on : Expression) => Iters(iter, None, None, on)
      } |
      "("~expression ~ rep("," ~> expression) <~opt(")") ^^ {
        case "("~(expr1 : Expression)~(expr : List[Expression]) => ArgList(expr1::expr)
      } |
      "["~expression ~ rep("," ~> expression) <~opt("]") ^^ {
        case "["~(expr1 : Expression)~(expr : List[Expression]) => ArgList(expr1::expr)
      } |
      "{"~expression ~ rep("," ~> expression) <~opt("}") ^^ {
        case "{"~(expr1 : Expression)~(expr : List[Expression]) => ArgList(expr1::expr)
      } |
      value ^^ {case x : Expression => x}

  lazy val value : PackratParser[Expression] =
      reference ^^ {x => SeqReference(x)}|
      constant ^^ {x => Constant(x)} |
      number ^^ {case x  => Num(x.toDouble)} |
      variable ^^ { case v : String =>{
//        addVar(v, false)
        Var(v)
      }} |
      "..." ^^ {x => ExtraSymbol("...")}



  private def initSet() : Unit = {
    variables = variables.empty
    //    variables += "x" //defined variables to be added here
    functions = functions.empty
  }

  private def parseEquation(equation : String): (ParseResult[Expression], ParseResult[Expression]) ={
    val (left, right) = equation.splitAt(equation.indexOf("="))
    initSet()
    parseOne(left) -> parseOne(right.substring(1))
  }

  private def parseOne(line : String): ParseResult[Expression] = {
    val failuresToIgnore = List(',','.','?','$','#','@','&')
    initSet()
    try {
      val res = parseAll(expression, line)
      res match {
        case Failure(msg, input) //if it is the last character and it is 'irrelevant' probably a case of not well filtered formula
          if (input !=null && line.length - input.pos.column == 0 && failuresToIgnore.contains(line.charAt(input.pos.column - 1))) => parseOne(line.drop(1))
        case a => a
      }
      //      res
    }catch{
      case a : Throwable => parseAll(expression,"A000002.") // very very bad, ugly.
    }
  }

  def parse(line : String) : Option[Expression] = {
    initSet()

    try {
      val parsed = parseAll(expression, line)
      parsed.successful match {
        case false => None
        case true => Some(postProcess(parsed.get))
      }
    }catch{
      case ex : Throwable => println(ex.toString); None
    }
  }


}

//object QueryParserRunner extends QueryParser{
//
//  def main(args : Array[String]): Unit = {
//    val test = "1 + a(x+2) + a"
//    println("input : "+ test)
//
//    println(parse(test))
//    println(variables)
//    println(functions)
//
//  }
//
//}