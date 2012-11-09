package info.kwarc.mmt.api.parser

import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.{LocalPath, DPath, MPath}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api._

/* Tokenizing Classes */

/**
 * trait for the separators (or delimiters) used during tokenization
 */
trait Separator

/**
 * Single separators are simple delimiters that cause a split during tokenization
 * @param s the value of the separator
 */
case class SingSep(s : String) extends Separator

/**
 * Pair separators are delimiters that must match with each other and delimit a sub-expression
 * They may be nested and tokenization does check for well-formedness with respect to pair separators matching and nesting
 * @param l
 * @param r
 */
case class PairSep(l : String, r : String) extends Separator

/* Parsing Classes */

case class TokenProperties(start : Int, end : Int)

sealed abstract class Token {
  val tkProps : TokenProperties
  def start = tkProps.start
  def end = tkProps.end
  def isArg : Boolean
  override def toString : String
}

case class StrTk(s : String, tkProps : TokenProperties) extends Token {
  def isArg = false
  override def toString = s
}

case class TermTk(t : Term, tkProps : TokenProperties) extends Token {
  def isArg = true
  override def toString = t.toString
}

case class ExpTk(sep : String, tks : List[Token], tkProps : TokenProperties) extends Token {
  def isArg = true
  override def toString = tks.mkString("(", " ", ")")
}

/**
 * Class for grammars
 */
class Grammar(name : String, declseps : List[Separator], var ops : List[Operator]) {

  var separators = declseps
  var operators = ops
  var strseps : List[String] = Nil
  var context = Context()
  def addContext(con : Context) = context = context ++ con
  def removeContext(con : Context) = context = Context(context.variables.dropRight(con.variables.length) : _*)

  def compile() {
    //val newSeps = operators.flatMap(x => x.notation.getStrMarkers).sortWith((x,y) => x.size > y.size).map(SingSep(_))
    separators = declseps //::: newSeps
    separators = separators.distinct.filterNot(_ == SingSep(""))
    strseps = separators.flatMap(x => x match {case SingSep(s) => List(s) case PairSep(l,r) => List(l,r)})
  }


  def init(n : Int) {
    compile()
  }


  /*
  def getOpByPrecedence : List[List[Operator]] = {
    val ops = operators.sortWith((x,y) => x.notation.precedence < y.notation.precedence)
    ops.foldLeft[List[List[Operator]]](Nil)((r,x) =>
      if (r.length == 0)
        List(List(x))
      else if (x.notation.precedence == r.head.head.notation.precedence)
        (x :: r.head) :: r.tail
      else List(x) :: r
    )
  }
  */

  def isDecl(sep : Separator) = sep match {
    case SingSep(" ") => true
    case _ => false
  }

  def isSep(s : String) = strseps.contains(s)

  def getMarkers(state : String) : List[String] = {
    separators.flatMap(x => x match {case SingSep(s) => List(s) case PairSep(l,r) => List(l,r)})
  }


}

object LFGrammar {
  private val name = "Simple LF Grammar"
  val thy = DPath(URI("http://cds.omdoc.org/foundations/lf/lf.omdoc")) ? "lf"

  private def context(l : List[Token]) : Context = l match {
    case StrTk(name,_) :: TermTk(tp,_) :: TermTk(df,_) :: Nil => Context(VarDecl(LocalName.parse(name),Some(tp), Some(df)))
    case StrTk(name,_) :: TermTk(tp,_) :: Nil => Context(VarDecl(LocalName.parse(name),Some(tp), None)) //TODO: FR changed OMV; parse may fail - OK?
    case StrTk(name,_) :: Nil => Context(VarDecl(LocalName.parse(name),None, None))

    case _ => throw ParseError("Invalid arguments for binder")
  }

	private val operators : List[Operator] = List(
		//new Operator(thy ? "type", Notation(List(Delimiter("type")),0)),
    //new Operator(thy ? "arrow", Notation(List(SeqArg(0, Delimiter("->"))), 3)),
		//new Operator(thy ? "lambda", Notation(List(Delimiter("["), StdArg(0), Delimiter("]")), 12, true)),
    //new Operator(thy ? "pi", Notation(List(Delimiter("{"), StdArg(0), Delimiter("}")), 12, true))
	  )

        /*     
  private val latexOperators : List[Operator] = List(
    new Operator(thy ? "type", Notation(List(StrMk("type")), NotationProperties(Precedence(0), AssocNone()))),
    new Operator(thy ? "arrow", Notation(List(ArgMk(0), StrMk("\\rightarrow"), ArgMk(1)), NotationProperties(Precedence(3), AssocRight()))),
    new Operator(thy ? "add", Notation(List(ArgMk(0), StrMk("+"),ArgMk(1)), NotationProperties(Precedence(5), AssocSeq()))),
    new Binder(thy ? "lambda", Notation(List(StrMk("\\lambda"), ArgMk(0), StrMk("\\cdot")), NotationProperties(Precedence(12), AssocLeft())), context, 0, BindRight()),
    new Binder(thy ? "lambda", Notation(List(StrMk("\\lambda"), ArgMk(0),StrMk("\\colon"),ArgMk(1), StrMk("\\cdot")), NotationProperties(Precedence(12), AssocLeft())), context,0, BindRight()),
    new Binder(thy ? "Pi", Notation(List(StrMk("{"), ArgMk(0), StrMk("}")), NotationProperties(Precedence(12), AssocLeft())), context,0, BindRight()))
*/

  private val markers : List[Separator]= List(
		  SingSep(" "),
      SingSep(":"),
//      SingSep("="),
      SingSep("!"),
      SingSep("/"),
      //SingSep("\\"),
      SingSep("["),
      SingSep("]"),
      SingSep("{"),
      SingSep("}"),
	  	PairSep("(", ")")
     )
		
  val grammar = new Grammar(name, markers, operators)



  private def makeOperator(c : Constant) : Operator = c.not match {
      case Some(not) => new Operator(c.path, not)
      case None => new Operator(c.path, None)
  }

  def grammar(ops : List[Constant]) = {
    new Grammar(name, markers, ops.map(makeOperator) ::: operators)
  }  
  
}