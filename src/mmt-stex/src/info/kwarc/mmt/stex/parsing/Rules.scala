package info.kwarc.mmt.stex.parsing

import scala.collection.mutable
import scala.tools.util.PathResolver.Environment
import scala.util.Try

class MacroApplication extends TeXTokenLike {
  var plain : Option[PlainMacro] = None
  var rule : Option[TeXRule] = None
  private[parsing] var children: List[TeXTokenLike] = Nil
  def startoffset = children.head.startoffset
  def endoffset: Int = children.last.endoffset

  override def iterate(f: TeXTokenLike => Unit): Unit = {
    f(this)
    children.foreach(_.iterate(f))
  }

  override def toString: String = children.mkString
  override def asPlain: String = children.map(_.asPlain).mkString
}

sealed trait Argument {
  def asname: String
  def asPlain : String
  def asList : List[TeXTokenLike]
}
case class Grouped(group: Group) extends Argument {
  def asname = TeXTokenLike.cleanstring(group.children.filterNot(_.isInstanceOf[Comment]).mkString.trim)

  override def toString: String = group.children.mkString
  override def asPlain: String = group.children.map(_.asPlain).mkString
  def asList = group.children
}
case class Single(tk: TeXTokenLike) extends Argument {
  def asname = TeXTokenLike.cleanstring(tk.toString)

  override def toString: String = tk.toString
  override def asPlain: String = tk.asPlain
  def asList = List(tk)
}
class OptionalArgument {
  var children : List[TeXTokenLike] = Nil
  override def toString = "[" + children.mkString + "]"
  def asPlain = "[" + children.map(_.asPlain) + "]"
  lazy val asname = children.mkString
  lazy val asKeyVals = {
    val m = mutable.HashMap.empty[List[TeXTokenLike],List[TeXTokenLike]]
    var ls : List[(List[TeXTokenLike],List[TeXTokenLike])] = List((Nil,Nil))
    var infirst = true
    children.foreach {
      case p@PlainText(",",_,_) if ls.head._1.nonEmpty =>
        ls ::= (Nil,Nil)
        infirst = true
      case p@PlainText("=",_,_) =>
        infirst = false
      case o if infirst => ls = (o :: ls.head._1,Nil) :: ls.tail
      case o => ls = (ls.head._1,o :: ls.head._2) :: ls.tail
    }
    ls.foreach{
      case (k,v) => m(k.reverse) = v.reverse
    }
    m
  }
  def get(key : String) = asKeyVals.find(t => TeXTokenLike.cleanstring(t._1.mkString.trim) == key).map(_._2)
  def getStr(key : String) = get(key).map {
    case List(g: Group) => TeXTokenLike.cleanstring(g.children.mkString.trim)
    case o => TeXTokenLike.cleanstring(o.mkString.trim)
  }
  def consume(key : String) = asKeyVals.find(t => TeXTokenLike.cleanstring(t._1.mkString.trim) == key).map{tks =>
    asKeyVals.remove(tks._1)
    tks._2
  }
  def consumeStr(key : String) = consume(key).map{
    case List(g:Group) => TeXTokenLike.cleanstring(g.children.mkString.trim)
    case o => TeXTokenLike.cleanstring(o.mkString.trim)
  }
  def drop(keys : String*) = keys.foreach(key => asKeyVals.keys.find(t => TeXTokenLike.cleanstring(t.mkString.trim) == key).foreach(asKeyVals.remove))
}

class SimpleMacroApplication(val starred:Boolean = false,val args:List[Argument] = Nil) extends MacroApplication {
  override def toString: String = plain.getOrElse("").toString + (if (starred) "*" else "") + args.map(a => "{" + a.toString + "}").mkString

  override def asPlain: String = plain.map(_.asPlain).getOrElse("") + (if (starred) "*" else "") + args.map(a => "{" + a.asPlain + "}").mkString
}

trait TeXRule {
  def name: String
}

class ParseState[A <: TeXTokenLike](val latex: LaTeXParser, val trigger: A) {
  var children: List[TeXTokenLike] = List(trigger)

  def readChar(c : Char) : Boolean = {
    val (b,ls) = latex.In.dropCharMaybe(c)
    ls.reverse.foreach(children ::= _)
    b
  }

  def readOptAgument : Option[OptionalArgument] = {
    latex.In.trim
    (try { latex.In.first } catch {
      case _ : java.util.NoSuchElementException =>
        return None
    }) match {
      case '[' =>
        children ::= latex.next()
        val ret = readOptI
        ret.foreach( children ::= _)
        if (latex.In.first == ']') {
          children ::= latex.next()
        }
        val oa = new OptionalArgument
        oa.children = ret.filterNot(_.isInstanceOf[Comment])
        Some(oa)
      case _ =>
        None
    }
  }
  private def readOptI : List[TeXTokenLike] = {
    val ret = latex.readNoRules{ latex.In.first == '[' || latex.In.first == ']'}
    if (latex.In.first == '[')
      ret ::: latex.next() :: {
        readOptI ::: latex.next() :: readOptI
      }
    else ret
  }

  def reparse(tks:List[TeXTokenLike]) = {
    children = children.filterNot(_.isInstanceOf[Comment])
    children.indexOfSlice(tks.reverse) match {
      case -1 =>
        children.indexWhere{
          case Group(ch,_,_) if ch.filterNot(_.isInstanceOf[Comment]) == tks => true
          case _ => false
        } match {
          case -1 =>
            ???
          case i =>
            latex.In.enqueueAll(PlainText("{", 0, 0) :: tks ::: PlainText("}", 0, 0) :: Nil)
            val ret = latex.readOne().asInstanceOf[Group]
            children = children.take(i) ::: ret :: children.drop(i + 1)
            ret.children
        }
      case i =>
        latex.In.enqueueAll(PlainText("{",0,0) :: tks ::: PlainText("}",0,0) :: Nil)
        val ret = latex.readOne().asInstanceOf[Group].children
        children = children.take(i) ::: ret.reverse ::: children.drop(i + tks.length)
        ret
    }
  }

  def readArgumentNoRules: Argument = {
    def next: TeXTokenLike = {
      latex.In.trim
      latex.readOneNoRules() match {
        case PlainText(str, startoffset, endoffset) if str.length > 1 =>
          latex.In.enqueue(PlainText(str.tail, startoffset + 1, endoffset))
          PlainText(str.take(1), startoffset, startoffset + 1)
        case c: Comment =>
          children ::= c
          next
        case o => o
      }
    }
    val nx = next
    children ::= nx
    nx match {
      case gr: Group => Grouped(gr)
      case tk => Single(tk)
    }
  }

  def readArgument: Argument = {
    def next : TeXTokenLike = {
      latex.In.trim
      latex.next() match {
        case PlainText(str, startoffset, endoffset) if str.length > 1 =>
          latex.In.enqueue(PlainText(str.tail, startoffset + 1, endoffset))
          PlainText(str.take(1), startoffset, startoffset + 1)
        case c : Comment =>
          children ::= c
          next
        case o => o
      }
    }
    val nx = next
    children ::= nx
    nx match {
      case gr: Group => Grouped(gr)
      case tk => Single(tk)
    }
  }
}

trait MacroRule extends TeXRule {

  private[parsing] def parse(trigger: PlainMacro)(implicit parser: LaTeXParser): Option[MacroApplication] = {
    val state = new ParseState(parser, trigger)
    val ret = try {
      apply(state)
    } catch {
      case e: LaTeXParseError =>
        trigger.errors ::= e
        parser.In.enqueueAll(state.children.reverse.tail)
        return None
    }
    ret.children = state.children.reverse
    ret.rule = Some(this)
    ret.plain = Some(trigger)
    Some(ret)
  }

  def name: String

  def apply(implicit parser: ParseState[PlainMacro]): MacroApplication
}

class Environment(begin:Begin.BeginMacro) extends MacroApplication {
  var end: Option[End.EndMacro] = None
  var header : List[TeXTokenLike] = Nil

  override def startoffset: Int = begin.startoffset

  override def endoffset: Int = end.getOrElse(children.lastOption.getOrElse(begin)).endoffset

  override def toString: String = begin.toString + children.mkString + end.getOrElse("").toString
}

trait EnvironmentRule extends TeXRule {
  val envname : String
  def name = "environment/" + envname
  def applyStart(implicit parser: ParseState[Begin.BeginMacro]) : Environment = new Environment(parser.trigger)
  def applyEnd(env:Environment)(implicit parser: ParseState[Begin.BeginMacro]) : Environment = {
    env.end match {
      case Some(e:End.EndMacro) if e.arg.asname == envname =>
      case Some(e: End.EndMacro) =>
        env.addError("Environment " + envname + " closed by " + e.arg.asname)
      case _ =>
        env.addError("Environment ended unexpectedly")
    }
    env
  }
}
case class UnknownEnvironmentRule(envname : String) extends EnvironmentRule

class RuleContainer {
  val rules = mutable.HashMap.empty[String,TeXRule]//:List[TeXRule] = Nil
  var letters = ""
}
object RuleContainer {
  def from(rc:RuleContainer) : RuleContainer = {
    val r = new RuleContainer
    r.letters = rc.letters
    r
  }
}

object End extends MacroRule {
  val name = "end"

  case class EndMacro(arg: Argument) extends SimpleMacroApplication(args = List(arg)) {
    def name = arg.asname
  }

  def apply(implicit parser: ParseState[PlainMacro]): EndMacro = {
    EndMacro(parser.readArgument)
  }
}

object Begin extends MacroRule {
  val name = "begin"

  override def parse(trigger: PlainMacro)(implicit parser: LaTeXParser): Option[Environment] = {
    val state = new ParseState[PlainMacro](parser, trigger)
    val ret = try {
      apply(state)
    } catch {
      case e: LaTeXParseError =>
        trigger.errors ::= e
        parser.In.enqueueAll(state.children.reverse.tail)
        return None
    }
    ret.children = state.children.reverse
    ret.rule = Some(this)
    ret.plain = Some(trigger)
    val rl = parser.getRule("environment/" + ret.name) match {
      case Some(rl: EnvironmentRule) => rl
      case _ => UnknownEnvironmentRule(ret.name)
    }
    val nstate = new ParseState(parser, ret)
    parser.inGroup {
      val env = rl.applyStart(nstate)
      env.header = nstate.children.reverse
      val next = try {
        parser.readTop {
          parser.In.startsWith("\\end")
        }
      } catch {
        case le: LaTeXParseError if !rl.isInstanceOf[UnknownEnvironmentRule] =>
          env.addError("Environment ended unexpectedly")
          le.tokens
        case le: LaTeXParseError =>
          le.tokens
      }
      env.children = env.children ::: next
      Try(parser.doMacro(parser.readMacro)).toOption match {
        case Some(e: End.EndMacro) => env.end = Some(e)
        case _ if !rl.isInstanceOf[UnknownEnvironmentRule] =>
          env.addError("\\end{" + ret.name + "} expected")
        case _ =>
      }
      Some(rl.applyEnd(env)(nstate))
    }
  }

  case class BeginMacro(arg: Argument) extends SimpleMacroApplication(args = List(arg)) {
    def name = arg.asname
  }

  def apply(implicit parser: ParseState[PlainMacro]): BeginMacro = {
    BeginMacro(parser.readArgument)
  }
}
trait VerbatimLikeRule extends TeXRule {
  def readVerb(finish:String*)(implicit parser: LaTeXParser): PlainText = {
    var ret : List[TeXTokenLike] = Nil
    val start = parser.In.offset
    val str = parser.In.stringUntil(finish.exists(parser.In.startsWith))
    PlainText(str,start,parser.In.offset)
  }
}
case class VerbatimEnvRule(envname:String,args:String = "") extends EnvironmentRule with VerbatimLikeRule with Skip{
  override def applyStart(implicit parser: ParseState[Begin.BeginMacro]): Environment = {
    val env = super.applyStart
    skip
    parser.children = readVerb("\\end{" + envname + "}")(parser.latex) :: parser.children
    env
  }
}
case class InlineVerbRule(name:String) extends MacroRule with VerbatimLikeRule {
  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    parser.readOptAgument
    parser.latex.In.trim
    val ch = parser.latex.In.nextChar
    val c = ch.str.head
    parser.children ::= ch
    parser.children = readVerb(c.toString)(parser.latex) :: parser.children
    parser.latex.In.drop(c.toString)
    parser.children ::= PlainText(c.toString, parser.latex.In.offset - 1, parser.latex.In.offset)
    new MacroApplication
  }
}

trait Skip extends TeXRule {
  val args : String
  def skip[A <: TeXTokenLike](implicit parser: ParseState[A]) = {
    args.foreach {
      case 'v' => parser.readArgumentNoRules
      case 'n' => parser.readArgument
      case 'o' => parser.readOptAgument
      case 't' => parser.latex.escapeMath {parser.readArgument}
      case 'm' => parser.latex.inMath {parser.readArgument}
    }
  }
}
case class SkipEnvironment(envname: String, args:String) extends EnvironmentRule with Skip {
  override def applyStart(implicit parser: ParseState[Begin.BeginMacro]): Environment = {
    skip
    new Environment(parser.trigger)
  }

}

case class SkipCommand(name : String,args:String) extends MacroRule with Skip {
  override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
    skip
    new MacroApplication
  }
}

object TeXRules {
  val url = new MacroRule with VerbatimLikeRule {
    val name="url"
    private def parserec(implicit parser: ParseState[PlainMacro]): Unit = {
      parser.latex.In.drop("{")
      parser.children ::= PlainText("{",parser.latex.In.offset-1,parser.latex.In.offset)
      parserest
    }
    private def parserest(implicit parser: ParseState[PlainMacro]): Unit ={
      parser.children = readVerb("}", "{")(parser.latex) :: parser.children
      parser.latex.In.first match {
        case '{' =>
          parserec
          parserest
        case '}' =>
          parser.latex.In.drop("}")
          parser.children ::= PlainText("}", parser.latex.In.offset - 1, parser.latex.In.offset)
      }
    }
    override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
      parser.readOptAgument
      parser.latex.In.trim
      parserec
      new MacroApplication
    }
  }
  val lstdefinelanguage = new MacroRule {
    val name = "lstdefinelanguage"
    override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
      import parser._
      readArgumentNoRules
      readOptAgument match {
        case Some(_) =>
          readArgumentNoRules
        case _ =>
      }
      readArgumentNoRules
      new MacroApplication
    }
  }
  val makeatletter = new MacroRule {
    override def name: String = "makeatletter"

    override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
      parser.latex.groups.head.letters += "@"
      new MacroApplication
    }
  }
  val makeatother = new MacroRule {
    override def name: String = "makeatother"

    override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
      parser.latex.groups.head.letters = parser.latex.groups.head.letters.filterNot(_ == '@')
      new MacroApplication
    }
  }
  val explsyntaxon = new MacroRule {
    override def name: String = "ExplSyntaxOn"

    override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
      parser.latex.groups.head.letters += ":_"
      new MacroApplication
    }
  }
  val explsyntaxoff = new MacroRule {
    override def name: String = "ExplSyntaxOff"

    override def apply(implicit parser: ParseState[PlainMacro]): MacroApplication = {
      parser.latex.groups.head.letters = parser.latex.groups.head.letters.filterNot(c => c == ':' || c == '_')
      new MacroApplication
    }
  }

  val allrules : List[TeXRule] = List(Begin,End,
    VerbatimEnvRule("verbatim"),VerbatimEnvRule("lstlisting"),VerbatimEnvRule("stexcode"),
    InlineVerbRule("lstinline"),InlineVerbRule("verb"),InlineVerbRule("stexcodeinline"),
    lstdefinelanguage,url,makeatletter,makeatother,explsyntaxon,explsyntaxoff,
    SkipCommand("newcommand","voov"),
    SkipCommand("providecommand", "voov"),
    SkipCommand("renewcommand", "voov"),
    SkipCommand("newenvironment", "voovv"),
    SkipCommand("renewenvironment", "voovv"),
    SkipCommand("NewDocumentCommand", "vvv"),
    SkipCommand("DeclareDocumentCommand", "vvv"),
    SkipCommand("DeclareRobustCommand", "vv"),
    SkipCommand("NewDocumentEnvironment", "vvvv"),
    SkipCommand("DeclareDocumentEnvironment", "vvvv"),
    SkipCommand("hbox","t"),SkipCommand("vbox","t"),SkipCommand("text","t"),
    SkipCommand("texttt","t"),SkipCommand("ensuremath","m"),SkipCommand("scalebox","vt")
  )
}
