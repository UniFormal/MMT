package info.kwarc.mmt.oeis.parser

import java.io.{File, PrintWriter}

import info.kwarc.mmt.api.ImplementationError
import info.kwarc.mmt.oeis.processor._

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}

abstract class OeisParserLogger {
  def logSuccess(s : String) : Unit
  def logFail(s : String) : Unit
  def logException(s : String) : Unit
}

class FileLogger extends OeisParserLogger {
  val successFile = new PrintWriter(new File("../../mmt-oeis/suc.txt"))
  val failFile = new PrintWriter( new File("../../mmt-oeis/fail.txt"))
  val exceptionFile = new PrintWriter( new File("../../mmt-oeis/exception.txt"))

  def logSuccess(s : String) = successFile.write(s)
  def logFail(s : String) = failFile.write(s)
  def logException(s : String) = exceptionFile.write(s)
}

class ConsoleLogger extends OeisParserLogger {
  def logSuccess(s : String) = println("Success: " + s)
  def logFail(s : String) = println("Fail: " + s)
  def logException(s : String) = println("Exception: " + s)
}

class SilentLogger extends OeisParserLogger {
  def logSuccess(s : String) = {}
  def logFail(s : String) = {}
  def logException(s : String) = {}
}

class FormulaParser(val dictionary : Set[String]) extends JavaTokenParsers with PackratParsers {
  var calls : BigInt = 0
  var succeded : BigInt = 0
  var exceptions : BigInt = 0
  //base is mmt-api/trunk

  def logger = new SilentLogger() //switch to the other loggers when debugging

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit): Unit = {
    val print = new java.io.PrintWriter(f)
    try {
      op(print)
    } finally {
      print.close()
    }
  }

  var variables = new mutable.HashSet[String]()
  var functions = new mutable.HashSet[String]()

  override val skipWhitespace = true

  def postProcess(expression : Expression) : Expression = expression match{
    case Func(name, args) if variables.contains(name) && args.args.length == 1 => Mul(Var(name)::args.args.head::Nil)
    case Func(name, args) => Func(name, postProcess(args) match {case a : ArgList => a})
    case FuncR(name, args) => FuncR(name, postProcess(args) match {case a : ArgList => a})
    case ArgList(args) =>ArgList(args.map(x=>postProcess(x)))
    case Mul(args) =>
      val mul =  args.map(x=>postProcess(x)).foldRight(Mul(Nil))(varToFunc)
      if(mul.expr.length == 1){
        mul.expr.head
      }else{
        mul
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
    case x => x
  }

  def varToFunc(left : Expression, right : Mul)  : Mul = (left,right) match{
    case (x: Var, y: Mul) if functions.contains(x.name) =>
      if(y.expr.nonEmpty) {
        val func = Func(x.name, ArgList(y.expr.head :: Nil))
        Mul(func :: y.expr.tail)
      }else{
        Mul(x::Nil) //TODO: See readme, it fails because 'functions' contain things like n X n
      }
    case (x:Expression, Mul(expr)) => Mul(x::expr)
    case _ => throw new Exception("Applied to foldr")
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

  val exceptionCases: Regex = List("[A-Za-z]\\-th").mkString("|").r

  lazy val word: Regex = "[A-Za-z\\']+(?![\\(\\[\\{\\<\\=\\>])\\b".r
  lazy val delim: Regex = "([\t\n\\.\\?\\!\\:\\;\\-\\=\\#\\*\\,\\/]|(\\/\\/)|(\\\\\\\\))".r
  lazy val month = "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"

  /*Month day year*/
  lazy val date = month + "\\s{0,1}\\d{1,4}\\s{0,1}\\d{1,4}"
  lazy val obracket : PackratParser[String] = "(" | "{" | "["
  lazy val cbracket : PackratParser[String] = ")" | "}" | "]"

  lazy val sen_obracket : PackratParser[String] = ("(" | "{" | "[")<~not("-")  // avoid considering expressions of (-exp text ...) as a text
  lazy val sen_cbracket : PackratParser[String] = ")" | "}" | "]" /*<~not("""[\^\+\*\-\/\\]""".r)*/  // avoid considering expr part of (expr)^exponent as seperate from exponent

  def isInDictionary(word : String) : Boolean = {
    if(word.isEmpty){
      return false
    }
    dictionary.contains(word) || dictionary.contains(word.toLowerCase.substring(1) + word.drop(1))
  }

  lazy val sentence: PackratParser[Sentence] = {
    rep1(sentence_cont) ^^ { x => Sentence(x.flatten) }
  }

  /*Why not do: rep1(rep(words)~rep(formulas)) - BUG (i call) in Scala. It can be easily fixed
  * by memorizing that both rep-s inside rep1 failed to parse anything - so that rep1 doesn't
  * go into an infinite loop of parsing nothing.*/
  lazy val sentence_cont: PackratParser[List[Expression]] = {
    (rep1(words)~opt(formulas))^^ {
      case (wordm) ~ (Some(formulam : Expression)) => wordm.flatten :+ formulam
      case (wordm) ~ (None) =>  wordm.flatten
    } |
      (formulas~rep(words)) ^^ {
        case (formulam : Expression)~(wordm) => formulam :: wordm.flatten
      }
  }

  //lazy val dots : PackratParser[String] = "..." | ".."
  lazy val formulas: PackratParser[Expression] =
    expression ^^ {
      x =>
        val result = postProcess(x)
        initSet()
        result
    }

  lazy val words: PackratParser[List[Expression]] =
    exceptionCases ~ rep(delim) ^^ {
      case x~y =>
        Word(x) :: y.map(x => Delim(x))
    } |
      date.r ~ rep(delim) ^^ {
        case x ~ del =>
          Date(x.toString) :: del.map(x => Delim(x))
      } |
      word ~ rep(delim) ^? {
        case w1~(del) if isInDictionary(w1) || isInDictionary(w1 + del) =>
          Word(w1)::del.map(x => Delim(x))
      } |
      "[\\w\\.\\-]+".r~("@" |"(AT)")~"[\\w\\.-]+".r ~ rep(delim)^^{
        case name~sym~dom~dels => Email(name+sym+dom) :: Delim(dels.mkString("")) :: Nil
      } |
      //unicode|
      not(generating_function)~>"_{0,2}[A-Z\\.']+[A-Za-z'[^\\x00-\\x7F]\\.]+(?![\\(\\{])_{0,2}(?![\\[\\{\\(])\\b".r ~ rep(delim) ^^ /*human names (not functions)*/ {
        case x~del =>  Name(x) :: del.map(x => Delim(x))
      } | // IDK - french names or something of this sort d'Artagon'k
      sen_obracket~ sentence ~sen_cbracket ^? { // ex: (fraction continues) //TODO: (-1) //try expr if fail try words
        //      case ob~w~cb =>(Delim(ob) :: w) ::: (s :: Delim(cb) :: Nil)

        //avoiding expression like (x+3) to be taken as Delim(() x + 3  Delim())
        case ob~w~cb if w.parts.head.isInstanceOf[Line] => (Delim(ob) :: w.parts):+ Delim(cb)
      } |
      rep1(delim) ^^{
        x => x.map( y => Delim(y))
      }


  lazy val words_inbracket: PackratParser[List[Expression]] =
    exceptionCases ~ rep(delim) ^^ {
      case x~y =>
        Word(x) :: y.map(x => Delim(x))
    } |
      date.r ~ rep(delim) ^^ {
        case x ~ del =>
          Date(x.toString) :: del.map(x => Delim(x))
      } |
      word ~ rep(delim) ^? {
        case w1~(del) if isInDictionary(w1) || isInDictionary(w1 + del) =>
          Word(w1)::del.map(x => Delim(x))
      } |
      "[\\w\\.\\-]+".r~("@" | "(AT)")~"[\\w\\.-]+".r ~ rep(delim)^^{
        case name~sym~dom~dels => Email(name+sym+dom) :: Delim(dels.mkString("")) :: Nil
      } |
      //unicode|
      not(generating_function)~>"_*[A-Z\\.']+[A-Za-z'[^\\x00-\\x7F]\\.]+(?![\\(\\{])_*(?![\\[\\{\\(])\\b".r ~ rep(delim) ^^ /*human names (not functions)*/ {
        case x~del =>  Name(x) :: del.map(x => Delim(x))
      } |
      sen_obracket~words~sentence~sen_cbracket ^^ { // ex: (fraction continues) //TODO: (-1) //try expr if fail try words
        //        case ob~w~cb =>(Delim(ob) :: w) ::: (s :: Delim(cb) :: Nil)
        case ob~w~sen~cb => (Delim(ob) :: w) ::: (sen :: Delim(cb) :: Nil)
      } |
      rep1(delim) ^^{
        x => x.map( y => Delim(y))
      }


  lazy val infix_ops = mod | element

  lazy val generating_function_def = "(?i)GF".r
  lazy val generating_function = "(?i)[O]*G\\.f\\.:[ =]*".r | "(?i)[O]*G\\.f\\.[ =]*".r | "(?i)[O]*G\\.f[ =]*".r
  lazy val no_bracket_function =  not(number|infix_ops)~>("floor\\b".r | "ceiling\\b".r | "ceil\\b".r | "sqrt\\b".r | "log\\b".r | "sinh\\b".r |"sin\\b".r | "cosh\\b".r | "cos\\b".r |
    "tan\\b".r |"tg\\b".r | "ctg\\b".r | "deg\\b".r | "binomial\\b".r | "numerator\\b".r | "exp\\b".r | "phi\\b".r | "If\\b".r | "if\\b".r)

  // no mod (for modulo) because it is an infix operator so it shouldn't understand a mod b, as a*(mod(b))
  lazy val function: PackratParser[String] = no_bracket_function | not(number|infix_ops) ~> "[A-Za-z']+[A-Za-z_0-9']*(?=[\\(\\[\\{])\\b".r

  lazy val constant: PackratParser[String] = "Pi\\b".r | "pi\\b".r | "infinity\\b".r | "infty\\b".r | "Infinity\\b".r | "Infty\\b".r |
    "Inf\\b".r | "inf\\b".r

  lazy val dots : PackratParser[String] = "..." | ".."
  lazy val reference : Regex = "A\\d{6}".r
  lazy val variable : PackratParser[String] = not(infix_ops)~>"\\w+\\d{0,1}\\b".r ^^ {x=>x}/*| "\\w+\\d+(?=\\()(?=\\))\\b".r*/
  lazy val number : PackratParser[String] = """(\d+(\.\d+)?)""".r | """\d+(\.\d+)\b""".r
  lazy val sum : PackratParser[String] = "SUM" | "Sum" | "sum" | "Product" | "product" | "PRODUCT" | "limit" | "Limit" | "lim" | "prod" | "Prod"
  lazy val comparison : PackratParser[(Expression,Expression) => Expression] = ("~" | "<>" | "<=" | ">=" | "==" | ">" | "=" | "<" | "->" | ":=" | "=:") ^^{
    x => {(left: Expression, right : Expression) => Equation(x, left, right)}
  }

  lazy val divisible : PackratParser[(Expression,Expression) => Expression] =
    "(\\|)|(divides)".r ^^ {
      _ => (x:Expression, y:Expression) => (x,y) match {
        case (x: Expression, y: Expression) => Divisible(x, y)
      }
    }

  lazy val unary_plusminus : PackratParser[String] = "+" | "-"

  lazy val mod : PackratParser[String] = "mod\\b".r | "modulo\\b".r
  lazy val modulo : PackratParser[(Expression,Expression) => Expression] =
    mod ^^ {
      _ => (x:Expression, y:Expression) => (x,y) match {
        case (x: Expression, y: Expression) => Modulo(x,y)
      }
    }

  lazy val elem : PackratParser[String] = "element\\b".r | "in\\b".r
  lazy val element : PackratParser[(Expression, Expression) => Expression] =
    elem ^^ {
      _ => (x:Expression, y:Expression) => (x,y) match {
        case (x: Expression, y: Expression) => InSet(x,y)
      }
    }

  lazy val plusminus: PackratParser[(Expression,Expression) => Expression] =
    "+" ^^ {
      _ => (x:Expression, y:Expression) => (x,y) match {
        case (x: Add , y: Expression) => Add(x.expr :+ y)
        case (x: Expression, y: Expression) => Add(List(x, y))
      }
    } |
      "-" ^^ {
        _ => (x:Expression, y:Expression) => (x,y) match {
          case (x: Sub , y: Expression) => Sub(x.expr :+ y)
          case (x: Expression, y: Expression) => Sub(List(x, y))
        }
      }

  lazy val multdiv: PackratParser[(Expression,Expression) => Expression] =
    "(\\*)|(\\bX\\b)".r ^^{
      _ => (x:Expression, y:Expression) => (x,y) match {
        case (x: Mul , y: Expression) => Mul(x.expr :+ y)
        case (x: Expression, y: Expression) => Mul(List(x, y))
      }
    } |
      "/" ^^ {
        _ => (x:Expression, y:Expression) => (x,y) match {
          case (x: Div , y: Expression) => Div(x.expr :+ y)
          case (x: Expression, y: Expression) => Div(List(x, y))
        }
      }


  lazy val expression : PackratParser[Expression] =
    generating_function~c_expression ^^ {
      case gn~exp => GeneratingFunction(exp)
    } |
      (c_expression~rep(comparison~c_expression)) ^^ {
        case expr~listexpr if listexpr.length != 0 =>
          applyFunctionsInOrder( expr :: listexpr.map(_._2), listexpr.map(_._1) )
        case expr~listexpr => expr
      }


  lazy val expression_iters : PackratParser[Expression] =
    (c_expression_iters~rep(comparison~c_expression_iters)) ^^ {
      case expr~listexpr if listexpr.length != 0 =>
        applyFunctionsInOrder( expr :: listexpr.map(_._2), listexpr.map(_._1) )
      case expr~listexpr => expr
    }

  lazy val c_expression : PackratParser[Expression] = signed_term~rep(sum_op) ^^ {
    case (fctr : Expression)~(divs : List[((Expression,Expression)=>Expression,Expression)]) if divs.length !=0 =>
      val (funcs, exprs) = divs.unzip
      applyFunctionsInOrder(fctr :: exprs , funcs)

    case (fctr)~(divs) => fctr
  }

  lazy val c_expression_iters : PackratParser[Expression] = signed_term~rep(sum_op_iters) ^^ {
    case (fctr : Expression)~(divs : List[((Expression,Expression)=>Expression,Expression)]) if divs.length !=0 =>
      val (funcs, exprs) = divs.unzip
      applyFunctionsInOrder(fctr :: exprs , funcs)

    case (fctr)~(divs) => fctr
  }

  lazy val sum_op : PackratParser[((Expression,Expression)=>Expression,Expression)] =
    plusminus~term ^^ {
      case func~(fctr : Expression) => func->fctr
    } |
      divisible ~ term  ^^ {
        case func ~ fctr => func -> fctr
      } |
      modulo ~ term ^^ {
        case func ~ fctr => func -> fctr
      } |
      obracket ~> modulo ~ term <~ cbracket ^^ {
        case func ~ fctr => func -> fctr
      }

  lazy val sum_op_iters : PackratParser[((Expression,Expression)=>Expression,Expression)] =
    plusminus~term ^^ {
      case func~(fctr : Expression) => func->fctr
    } |
      divisible ~ term  ^^ {
        case func ~ fctr => func -> fctr
      } |
      element ~ term ^^ {
        case func ~ fctr => func -> fctr
      } |
      modulo ~ term ^^ {
        case func ~ fctr => func -> fctr
      } |
      obracket ~> modulo ~ term <~ cbracket ^^ {
        case func ~ fctr => func -> fctr
      }


  lazy val signed_term : PackratParser[Expression] =
    unary_plusminus~term ^^ {
      case "-"~(fctr : Expression) => Neg(fctr)
      case "+"~(fctr : Expression) => fctr
      case _ => throw ImplementationError("unsupported")
    }|
      term ^^ {
        case (a : Expression) => a
      }

  lazy val term : PackratParser[Expression] =
  //    term <~ "even\\b".r ^^ {
  //      x => Divisible(x, Num(2))
  //    } |
    term_no_fact/*~opt("!")*/ ^^ {
      //      case exp~Some("!") => Factorial(exp)
      case exp/*~None*/ => exp
    }


  lazy val term_no_fact : PackratParser[Expression] =
  //this will fail us on 2n*log for obvious reasons
  /*  number ~ (not(words)~>unsigned_factor)~opt(unsigned_factor) ^^ {
      case (no )~(expr : Expression)~Some(expr1 : Expression) => Mul(Num(no.toDouble)::expr::expr1::Nil)
      case (no )~(expr : Expression)~None => Mul(Num(no.toDouble)::expr::Nil)
    } |*/
    //TODO the type erasure warnings here are bugs and may not be ignored
    unsigned_factor~rep(multiply | lazy_multiply) ^^ {
      case (fctr : Expression)~(divs) if divs.nonEmpty =>
        applyFunctionsInOrder(fctr :: divs.collect({
          case x : (Any@unchecked~Expression@unchecked)  => x._2
          case x : Expression@unchecked => x
        }), funcs = divs.collect({
          case x: (((Expression, Expression) => Expression)@unchecked ~ Expression@unchecked) => x._1
          case x =>
            (x: Expression, y: Expression) => (x, y) match {
              case (x: Var, y: ArgList) => y match {
                //In case there is var(var) consider the first var to be a function
                case ArgList(List(Var(a))) =>
                  addFunc(x.name)
                  Func(x.name, y)
                case ArgList(List(Num(a))) =>
                  addFunc(x.name)
                  Func(x.name, y)
                case elsee =>
                  Func(x.name, y)
              }
              case (x: Mul, y: Expression) => Mul(x.expr :+ y)
              case (x: Expression, y: Expression) => Mul(List(x, y))
            }
        }))
      case (fctr : Var)~(divs) => addVar(fctr.name); fctr
      case (fctr : Expression)~(divs) => fctr
    }

  lazy val multiply : PackratParser[((Expression,Expression)=>Expression)~Expression] =
    multdiv~signed_factor ^^ {x => x}

  //add HERE what is not to be understood as lazy multiplication
  //TODO : Add context so that once inside the function the words_inbracket is not checked ( or something similar) because of exp(2 Pi t)
  lazy val lazy_multiply : PackratParser[Expression] =
    (not(dots)~not(words_inbracket)~not(mod))~>unsigned_factor ^^ {x => x}


  lazy val signed_factor : PackratParser[Expression] =
    unary_plusminus~unsigned_factor ^^ {
      case "-"~(fctr : Expression) => Neg(fctr)
      case "+"~(fctr : Expression) => fctr
      case _ => throw ImplementationError("unsupported")
    } |
      unsigned_factor ^^ {x=>x}

  lazy val suffix_functions : PackratParser[String] = "even\\b".r | "?"
  // acts as factor with suffixes
  lazy val unsigned_factor : PackratParser[Expression] =
    unsigned_factor ~ suffix_functions ^^ {
      case x ~ "even" => Divisible(x,Num(2))
      case x ~ "?" => QVar(x)
      case _ => throw ImplementationError("unsupported")
    } |
      factor~opt("!") ^^ {
        case (a:Expression)~ None => a
        case a ~ Some(b) => Factorial(a)
      }

  lazy val factor : PackratParser[Expression] =
    argument_suffix~opt("^"~signed_factor~opt(argument)) ^? {
      case (a : Var)~Some("^"~(signed : Expression)~Some(arguments : ArgList)) =>{
        Power(Func(a.name, arguments), signed)
      }
      case (a : ArgList)~Some("^"~(signed : Expression)~Some(arguments : ArgList))
        if a.args.length == 1 && arguments.args.length == 1 => { // TODO: Can drop the first a.args.length == 1, because we can have [-1,1,0]^2
        Power(a.args.head, Mul(signed :: arguments.args.head :: Nil))
      }
      case (a : ArgList)~Some("^"~(signed : Expression)~None) => Power(a.args.head, signed)
      case (a : ArgList)~None if a.args.length == 1 => a.args.head

      // Query parser
      case (ar : QVar)~Some("^"~signed~Some(arguments : ArgList))
        if(ar.expr match {
          case a: Var => true
          case a: ArgList if a.args.length == 1 && arguments.args.length == 1 => true
          case _ => false
        }) => {
        ar.expr match {
          case a: Var => Power(QVar(Func(a.name, arguments)), signed) // TODO: NOT QUITE RIGHT, the ? is on the function name
          case a: ArgList => Power(QVar(a.args.head), Mul(signed :: arguments.args.head :: Nil))
        }
      }
      case (ar : QVar)~Some("^"~(signed : Expression)~None)
        if(ar.expr match {
          case a: ArgList => true
          case _ => false
        }) =>
        ar.expr match {
          case a: ArgList => Power(QVar(a.args.head), signed)
        }
      case (ar : QVar)~Some("^"~(signed : Expression)~None) => Power(ar, signed)
      case (ar : QVar)~None
        if(ar.expr match {
          case a: ArgList if a.args.length == 1 => true
          case _ => false
        }) =>
        ar.expr match {
          case a: ArgList => QVar(a.args.head)
        }
      case (ar : QVar)~None => ar

      case (a : Expression)~Some("^"~(signed : Expression)~None) => Power(a,signed)
      case (a : Expression)~None => a
    } |
      argument_suffix~opt("^"~signed_factor) ^^ {
        case (ar : QVar)~Some("^"~(signed : Expression)) =>
          Power(ar, signed)
        case (a : Expression)~Some("^"~(signed : Expression)) =>
          Power(a,signed)
        case _ => throw ImplementationError("unsupported")

      }

  lazy val argument_suffix : PackratParser[Expression] =
    argument ~ suffix_functions ^^ {
      case (x : Expression) ~ "even" => Divisible(x, Num(2))
      case (x : Expression) ~ "?" => QVar(x)
      case _ => throw ImplementationError("unsupported")
    } |
      argument ^^ {x => x}

  lazy val argument : PackratParser[Expression] =
    sum ~(opt("_")~>obracket~>expression)~(dots~>expression<~cbracket)~expression ^^{
      case (iter : String)~(from )~(to : Expression)~(on : ArgList) => Iters(iter, Some(from), Some(to), on.args.head)
      case (iter : String)~(from )~(to : Expression)~(on : Expression) => Iters(iter, Some(from), Some(to), on)
    } |
      sum ~(opt("_")~>obracket~>expression)~(dots~>expression)~(","~>expression<~cbracket)^^{
        case (iter : String)~(from )~(to : Expression)~(on : ArgList) => Iters(iter, Some(from), Some(to), on.args.head)
        case (iter : String)~(from )~(to : Expression)~(on : Expression) => Iters(iter, Some(from), Some(to), on)
      } |
      sum ~(opt("_")~>obracket~>expression<~cbracket)~opt("^"~>obracket~>expression<~cbracket)~(opt(obracket)~>expression<~opt(cbracket)) ^^{
        case (iter : String)~(from : Expression)~(e : Option[Expression])~(on : ArgList) =>
          Iters(iter,Some(from),e,on.args.head)
        case (iter : String)~(from : Expression)~(e : Option[Expression])~(on : Expression) =>
          Iters(iter,Some(from),e,on)
      } |
      (sum)~(opt("_")~>obracket~>expression)~(dots~>expression<~("," | ";"))~(expression<~cbracket) ^^{
        case (iter : String)~(from )~(to : Expression)~(on : Expression) => Iters(iter, Some(from), Some(to), on)
      } |
      (sum)~(opt("_")~>obracket~>expression<~("," | ";" | ":") )~(expression)~(dots~>expression<~cbracket) ^^{
        case (iter : String)~(on )~(from : Expression)~(to : Expression) => Iters(iter, Some(from), Some(to), on)
      } |
      (sum)~(opt("_")~>"{"~>expression<~("," | ";" | ":"))~(expression<~"}")~(expression) ^^{
        case (iter : String)~(from )~(to : Expression)~(on : Expression) => Iters(iter, Some(from), Some(to), on)
      } |
      (sum)~(opt("_")~>"{"~>expression<~("," | ";" | ":"))~(expression<~"}") ^^{
        case (iter : String)~(from )~(on : Expression) => Iters(iter, Some(from), None, on)
      } |
      (sum)~(opt("_")~>(obracket~>expression_iters<~cbracket))~opt(obracket~>expression<~cbracket) ^^{
        case (iter : String)~(on : Expression)~Some(expr) => Iters(iter, Some(on), None, expr)
        case (iter : String)~(on : Expression)~None => Iters(iter, None, None, on)
      } |
      not(constant|reference)~> no_bracket_function ~(not("^" | obracket)~>term_no_fact) ^^ {
        case (a : String)~(b : ArgList) =>
          if(variables.contains(a) && b.args.length == 1){
            Mul(List(Var(a), b.args.head))
          }else {
            if(b.args.length > 1){
              addFunc(a)
            }
            Func(a, b)
          }
        case (a : String)~(b : Mul) =>
          if(variables.contains(a)){
            Mul(Var(a)::b.expr)
          }else {
            Func(a, ArgList(b :: Nil))
          }
        case (a : String)~(b : Expression) =>
          if(variables.contains(a)){
            Mul(Var(a)::b::Nil)
          }else {
            Func(a, ArgList(b :: Nil))
          }
      } |
      (not(constant|reference)~generating_function_def)~>(argument) ^^ {
        case (b : ArgList)  => GeneratingFunctionDef(b)
      } |
      not(constant|reference)~>opt(function)~(argument) ^^ {
        case Some(a : String)~(b : ArgList)  =>
          if(variables.contains(a) && b.args.length == 1){
            Mul(List(Var(a), b.args.head))
          }else {
            if(b.args.length > 1){
              addFunc(a)
            }
            Func(a, b)
          }
        case Some(a : String)~(b : Expression) =>
          if(variables.contains(a)){
            Mul(Var(a)::b::Nil)
          }else {
            Func(a, ArgList(b :: Nil))
          }
        case None~(b: ArgList) if b.args.length == 1 => b.args.head
        case None~(b: Expression) => b
      } |
      not(constant|reference)~>(function)<~(obracket~rep(",")~cbracket) ^^ {
        case (a : String) => Func(a, ArgList(Nil))
      } |
      obracket~cbracket ^^ {
        case left~right => ArgList(Nil)
      } |
      reference~argument ^^{
        case (ref : String)~(args : ArgList)  => FuncR(SeqReference(ref), args)
        case (ref : String)~(arg : Expression) => FuncR(SeqReference(ref), ArgList(arg::Nil))
      } |
      obracket~>expression ~ rep("," ~> expression) <~cbracket ^^ {
        case (expr1 : Expression)~(expr : List[Expression]) => ArgList(expr1::expr)
      } |
      "|"~>expression <~ "|" ^^ {
        case (expr : Expression) => Abs(expr)
      } |
      value ^^ { case x : Expression => x }

  lazy val value : PackratParser[Expression] =
    reference ^^ {x => SeqReference(x)}|
      constant ^^ {x => Constant(x)} |
      number ^^ {case x  => Num(x.toDouble)} |
      variable ^^ { case v : String =>
        Var(v)
      } |
      "..." ^^ {x => ExtraSymbol("...")}

  def initSet() : Unit = {
    variables = variables.empty
    functions = functions.empty
  }

  def parse(line : String, theory : String = "") : Option[Expression] = {
    if(line.isEmpty){
      None
    } else {
      initSet()
      calls += 1
      try {
        val parsed = parseAll(expression, line)
        parsed.successful match {
          case false => logger.logFail(theory + "\t" + line + "\n"); None
          case true => succeded += 1; logger.logSuccess(theory + "\t" + line + "\n"); Some(postProcess(parsed.get))
        }
      } catch {
        case ex: Throwable => exceptions += 1; None
      }
    }
  }

  def parseQuery(line : String, theory : String = "") : Option[String] = {
    if(line.isEmpty){
      None
    }else{
      initSet()
      try{
        val parsed = parseAll(expression, line)
        parsed.successful match {
          case false => None
          case true => Some(parsed.get.toNode(theory).toString())
        }
      } catch {
        case ex : Throwable => None
      }
    }
  }
}

object FormulaParserTest extends FormulaParser(Nil.toSet) {
  def main(args : Array[String]): Unit = {
    val test = "T(x)(x+3)"
    println("input : "+ test)
    println(parse(expression, test))

  }

}
