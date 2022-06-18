package info.kwarc.mmt.stex.parsing

import info.kwarc.mmt.api.Level
import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api.utils.{File, Unparsed}
import info.kwarc.mmt.stex.STeXError

import scala.collection.mutable

case class LaTeXParseError(s : String,extramsg : Option[String] = None,lvl : Level = Level.Error) extends Throwable

trait TeXTokenLike {
  val startoffset : Int
  val endoffset: Int
  def iterate(f : TeXTokenLike => Unit) = f(this)
  val attributes = mutable.HashMap.empty[String,Any]
  var errors : List[STeXError] = Nil
  def addError(msg : String,extramsg : Option[String] = None,lvl : Level = Level.Error) = {
    errors ::= new STeXError(msg, extramsg, Some(lvl))
  }

}
trait MacroLike extends TeXTokenLike
class PlainText(val str : String,val startoffset:Int,val endoffset:Int) extends TeXTokenLike {
  override def toString: String = str
}
class PlainMacro(val name:String, val startoffset:Int, val endoffset:Int) extends MacroLike {
  override def toString: String = "\\" + name + " "
}
class MacroApplication(val plain:PlainMacro,val children:List[TeXTokenLike],val rule:TeXRule) extends MacroLike {
  val startoffset = plain.startoffset
  val endoffset = children.lastOption.map(_.endoffset).getOrElse(plain.endoffset)

  override def iterate(f: TeXTokenLike => Unit): Unit = {
    super.iterate(f)
    children.foreach(_.iterate(f))
  }

  override def toString: String = "Macro[" + plain.toString + ", " + children.mkString + "]"
}
class Group(val content:List[TeXTokenLike],val startoffset:Int,val endoffset:Int) extends TeXTokenLike {
  override def iterate(f: TeXTokenLike => Unit): Unit = {
    super.iterate(f)
    content.foreach(_.iterate(f))
  }
  override def toString: String = "{" + content.mkString + "}"
}
case class Comment(str : String,startoffset:Int,endoffset:Int) extends TeXTokenLike {
  override def toString: String = "%" + str
}
case class Math(content:List[TeXTokenLike],startdelim:TeXTokenLike,enddelim:TeXTokenLike) extends TeXTokenLike {
  val startoffset = startdelim.startoffset
  val endoffset = enddelim.endoffset

  override def toString: String = startdelim.toString + content.mkString + enddelim.toString

  override def iterate(f: TeXTokenLike => Unit): Unit = {
    super.iterate(f)
    startdelim.iterate(f)
    content.foreach(_.iterate(f))
    enddelim.iterate(f)
  }
}
class Environment(val begin:MacroApplication,val end:TeXTokenLike,val children:List[TeXTokenLike],val rule:Option[EnvironmentRule]) extends TeXTokenLike {
  val startoffset = begin.startoffset
  val endoffset = end.endoffset
  var beginname : Option[TeXTokenLike] = None
  var endname : Option[TeXTokenLike] = None

  override def toString: String = begin.toString + children.mkString + end.toString
  override def iterate(f: TeXTokenLike => Unit): Unit = {
    super.iterate(f)
    begin.iterate(f)
    children.foreach(_.iterate(f))
    end.iterate(f)
  }
}

trait TeXRule {
  def safely[A <: TeXTokenLike](elem: => A)(f : => A): A = try { f } catch {
    case LaTeXParseError(s,e,l) =>
      val ge = elem
      ge.addError(s,e,l)
      ge
  }
  val name:String
  def readChar(c:Char)(implicit in: Unparsed, state: LaTeXParserState): (Boolean,List[TeXTokenLike]) = {
    import SuperficialLaTeXParser._
    in.trim
    var occured = false
    var children : List[TeXTokenLike] = Nil
    while (!in.empty && (in.first match {
      case `c` =>
        val start = in.offset
        in.drop(1)
        children ::= new PlainText(c.toString,start,in.offset)
        occured = true
        false
      case '%' =>
        children ::= readComment
        true
      case _ =>
        false
    })) {}
    (occured,children)
  }

  def readArg(implicit in: Unparsed, state:LaTeXParserState) : (TeXTokenLike,List[TeXTokenLike]) = {
    import SuperficialLaTeXParser._
    var children : List[TeXTokenLike] = Nil
    var done = false
    in.trim
    while (!done) {
      in.first match {
        case '%' =>
          children ::= readComment
          in.trim
        case '{' =>
          done = true
          val ret = readGroup
          children ::= ret
          return (ret,children.reverse)
        case o =>
          done = true
          in.drop(1)
          val ret = new PlainText(o.toString, in.offset-1, in.offset)
          children ::= ret
          return (ret,children.reverse)
      }
    }
    ???
  }

  def readOptArg(implicit in: Unparsed, state:LaTeXParserState) : (List[List[TeXTokenLike]],List[TeXTokenLike]) = {
    import SuperficialLaTeXParser._
    in.trim
    var children : List[TeXTokenLike] = Nil
    var args : List[List[TeXTokenLike]] = Nil
    while (true) {
      in.first match {
        case '%' =>
          children = children ::: List(readComment)
          in.trim
        case '[' =>
          var done = false
          in.drop(1)
          children = children ::: List(new PlainText("[", in.offset - 1, in.offset))
          while (!done) {
            var curr : List[TeXTokenLike] = Nil
            var inbrackets = 0
            var break = false
            while(!in.empty && !break) {
              in.first match {
                case ']' if inbrackets == 0 =>
                  children = children ::: List(new PlainText("]", in.offset, in.offset + 1))
                  in.drop(1)
                  done = true
                  break = true
                case ']' =>
                  inbrackets -= 1
                  in.drop(1)
                  children = children ::: List(new PlainText("]", in.offset, in.offset + 1))
                  curr ::= new PlainText("]", in.offset, in.offset + 1)
                case ',' if inbrackets == 0 =>
                  children = children ::: List(new PlainText(",", in.offset, in.offset + 1))
                  in.drop(1)
                  break = true
                case '[' =>
                  inbrackets += 1
                  in.drop(1)
                  children = children ::: List(new PlainText("[", in.offset, in.offset + 1))
                  curr ::= new PlainText("[", in.offset, in.offset + 1)
                case _ =>
                  val ret = readOne(List('[',']',',').contains)
                  children = children ::: ret :: Nil
                  curr ::= ret
              }
            }
            if (curr.nonEmpty)
              args ::= curr.filterNot(_.isInstanceOf[Comment]).reverse
            curr = Nil
          }
          return (args.reverse, children)
        case _ => return (Nil, children)
      }
    }
    ???
  }

}
abstract class EnvironmentRule(val name:String) extends TeXRule {
  def parse(begin:MacroApplication)(implicit in: Unparsed, state:LaTeXParserState): MacroApplication
  def finalize(env : Environment)(implicit state:LaTeXParserState) : Environment
}
trait MacroRule extends TeXRule {
  val name:String
  def parse(plain:PlainMacro)(implicit in: Unparsed, state:LaTeXParserState) : TeXTokenLike
  //def apply(plain:PlainMacro,ls:List[TeXTokenLike]) : MacroApplication
}
class SimpleMacroRule(val name : String,args:Int=0,maybestarred:Boolean = false) extends MacroRule {
  def after(ma:SimpleMacroApplication)(implicit in: Unparsed, state: LaTeXParserState) = {}
  override def parse(plain: PlainMacro)(implicit in: Unparsed, state: LaTeXParserState): MacroApplication = {
    import SuperficialLaTeXParser._
    var children : List[TeXTokenLike] = Nil
    val starred = if (maybestarred) {
      val (b,c) = readChar('%')
      children = c
      b
    } else false
    var argls : List[TeXTokenLike] = Nil
    (0 until args).foreach{_ =>
      in.trim
      if (!in.empty) in.first match {
        case '%' =>
          children ::= readComment
        case _ =>
          val (arg,ch) = readArg
          children = ch.reverse ::: children
          argls ::= arg
      }
    }
    val ret = new SimpleMacroApplication(plain,children.reverse,starred,argls.reverse,this)
    after(ret)
    ret
  }
}
class SimpleMacroApplication(plain:PlainMacro,children:List[TeXTokenLike],val starred:Boolean,val args:List[TeXTokenLike], rule:TeXRule) extends MacroApplication(plain,children,rule)

trait DefLikeRule extends MacroRule {
  override def parse(plain: PlainMacro)(implicit in: Unparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
    import SuperficialLaTeXParser._
    val pre = readTop(c => c == '{')
    val currrules = state.rules
    val main = try {
      state.rules = Nil
      readGroup
    } finally {
      state.rules = currrules
    }
    new SimpleMacroApplication(plain,pre ::: main :: Nil,false,Nil,this)
  }
}

object LaTeXRules {

  val _def = new MacroRule with DefLikeRule {
    override val name: String = "def"
  }
  val edef = new MacroRule with DefLikeRule {
    override val name: String = "edef"
  }

  val end = new SimpleMacroRule("end",1) {
    override def after(ma: SimpleMacroApplication)(implicit in: Unparsed, state: LaTeXParserState): Unit = {
      state.closegroup
      super.after(ma)
    }
  }
  val begin = new MacroRule {
    val name = "begin"
    override def parse(plain: PlainMacro)(implicit in: Unparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
      state.opengroup
      val (r,mchildren) = readArg
      r match {
        case g : Group => g.content match {
          case List(st : PlainText) =>
            val name = st.str.trim
            val (rule,bg) = state.envrules.find(_.name == name) match {
              case Some(r) =>
                (Some(r),r.parse(new MacroApplication(plain,mchildren,this)))
              case _ =>
                (None,new MacroApplication(plain,mchildren,this))
            }
            var endtk : Option[SimpleMacroApplication] = None
            import SuperficialLaTeXParser._
            var children : List[TeXTokenLike] = Nil
            def getEnd = endtk.getOrElse{children match {
              case Nil => bg
              case h :: tail => h
            }}
            try {
              while ( {
                val next = readOne()
                if (next.errors.nonEmpty) {
                  children ::= next
                  false
                } else (next match {
                    case ma: SimpleMacroApplication if ma.rule == end =>
                      endtk = Some(ma)
                      false
                    case _ =>
                      children ::= next
                      true
                  })
              }) {}
              val env = new Environment(bg,getEnd,children.reverse,rule)
              env.beginname = Some(st)
              rule.map(_.finalize(env)).getOrElse(env)
            } catch {
              case LaTeXParseError(s,em,l) =>
                bg.addError("Environment not closed",em match {
                  case Some(em) => Some(s + ": " + em)
                  case _ => Some(s)
                })
                val e = new Environment(bg,getEnd,children.reverse,rule)
                e.beginname = Some(st)
                e
            }
          case _ => new MacroApplication(plain,List(g),this)
        }
        case o => new MacroApplication(plain,List(o),this)
      }
    }
  }
  val makeatletter = new SimpleMacroRule("makeatletter") {
    override def after(ma: SimpleMacroApplication)(implicit in: Unparsed, state: LaTeXParserState): Unit = state.letters ::= '@'
  }
  val makeatother = new SimpleMacroRule("makeatother") {
    override def after(ma: SimpleMacroApplication)(implicit in: Unparsed, state: LaTeXParserState): Unit = state.letters = state.letters.filterNot(_ == '@')
  }
  val explsyntaxon = new SimpleMacroRule("ExplSyntaxOn") {
    override def after(ma: SimpleMacroApplication)(implicit in: Unparsed, state: LaTeXParserState): Unit = {
      state.letters ::= '_'
      state.letters ::= ':'
    }
  }
  val explsyntaxoff = new SimpleMacroRule("ExplSyntaxOff") {
    override def after(ma: SimpleMacroApplication)(implicit in: Unparsed, state: LaTeXParserState): Unit = state.letters = state.letters.filterNot(c => c == '_' || c == ':')
  }
  val dmathend = new SimpleMacroRule("]") {
    override def after(ma: SimpleMacroApplication)(implicit in: Unparsed, state: LaTeXParserState): Unit = {
      state.closegroup
      super.after(ma)
    }
  }
  val dmathstart = new MacroRule {
    val name = "["
    override def parse(plain: PlainMacro)(implicit in: Unparsed, state: LaTeXParserState): Math = {
      import SuperficialLaTeXParser._
      state.opengroup
      state.inmath = true
      var children:List[TeXTokenLike] = Nil
      try {
        while ({
          val next = readOne()
          children ::= next
          next match {
            case ma : SimpleMacroApplication if ma.rule == dmathend =>
              false
            case _ => true
          }
        }) {}
      } finally {
        state.inmath = false
      }
      Math(children.tail.reverse,new MacroApplication(plain,Nil,this),children.head)
    }
  }

  val allrules = List(
    makeatletter,
    makeatother,
    explsyntaxon,
    explsyntaxoff,
    dmathstart,
    dmathend,
    begin,end,
    _def,edef
  )
}

object SuperficialLaTeXParser {
  def apply(file: File,rules : List[TeXRule] = LaTeXRules.allrules) : List[TeXTokenLike] = apply(File.read(file),rules)
  def apply(string : String,rules : List[TeXRule]) : List[TeXTokenLike] = {
    implicit val in = new Unparsed(string,s => throw new STeXError(s,None,None))
    implicit val state = new LaTeXParserState(rules)
    readTop()
  }
  def readMacro(implicit in: Unparsed, state:LaTeXParserState) : TeXTokenLike = {
    val start = in.offset
    in.drop("\\")
    val first = in.first
    val str = if (state.isLetter(first)) {
      in.takeWhile(state.isLetter)
    } else {
      in.drop(1)
      first.toString
    }
    val plain = new PlainMacro(str,start,in.offset)
    val ret = state.macrorules.find(_.name == plain.name).map(_.parse(plain)).getOrElse(plain)
    in.trim
    ret
  }
  def readTop(break:Char => Boolean = _ => false)(implicit in: Unparsed, state:LaTeXParserState) : List[TeXTokenLike] = {
    var ret : List[TeXTokenLike] = Nil
    while(!in.empty && !break(in.first)) ret ::= readOne(break)
    ret.reverse
  }
  def readOne(break:Char => Boolean = _ => false)(implicit in: Unparsed, state:LaTeXParserState): TeXTokenLike = {
    if (in.empty) throw LaTeXParseError("Unexpected end of file")
    in.first match {
      case '\\' => readMacro
      case '{' => readGroup
      case '%' => readComment
      case '$' if in.getnext(2).toString == "$$" =>
        readMath("$$","$$")
      case '$' => readMath("$","$")
      case _ => readText(break)
    }
  }
  def readMath(starts : String,ends:String)(implicit in: Unparsed, state:LaTeXParserState) = {
    val start = in.offset
    var ret : List[TeXTokenLike] = Nil
    in.drop(starts)
    state.opengroup
    state.inmath = true
    try {
      while (!in.empty && in.getnext(ends.length).toString != ends) ret ::= readOne()
    } finally {
      state.inmath = false
      state.closegroup
    }
    in.drop(ends)
    Math(ret.reverse,new PlainText(starts,start,start + starts.length),new PlainText(ends,in.offset - ends.length,in.offset))
  }
  def readGroup(implicit in: Unparsed, state:LaTeXParserState) = {
    val start = in.offset
    in.drop("{")
    val ct = readTop(_ == '}')
    val gr = new Group(ct,start,in.offset+1)
    if (!in.empty && in.first == '}')
      in.drop("}")
    else if (in.empty) {
      gr.addError("Group never closes")
    } else {
      print("")
      ???
    }
    gr
  }
  def readText(break:Char => Boolean = _ => false)(implicit in: Unparsed, state:LaTeXParserState) = {
    val start = in.offset
    val str = in.takeWhileSafe(c => !break(c) && !state.specialchars.contains(c))
    new PlainText(str,start,in.offset)
  }
  def readComment(implicit in: Unparsed, state:LaTeXParserState) = {
    val start = in.offset
    in.drop("%")
    val str = in.takeWhileSafe(_ != '\n')
    Comment(str,start,in.offset)
  }
}

class LaTeXParserState(initrules : List[TeXRule]) {
  var inmath = false
  var rules : List[RuleContainer] = List({
    val rc = new RuleContainer
    rc.rules = initrules
    rc
  })
  def opengroup = {
    rules ::= new RuleContainer
  }
  def closegroup = rules = rules.tail
  def macrorules = rules.flatMap(_.rules).collect{case mr : MacroRule => mr}
  def envrules = rules.flatMap(_.rules).collect{case er: EnvironmentRule => er}
  def addRule(rule : TeXRule,global:Boolean = false) = if (global) rules.foreach(_.rules ::= rule) else rules.head.rules ::= rule
  var letters : List[Char] = Nil
  var specialchars : List[Char] = List('\\','%','{','$')
  def isLetter(c:Char) = c.isLetter || letters.contains(c)
}

class RuleContainer {
  var rules:List[TeXRule] = Nil
}