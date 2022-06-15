package info.kwarc.mmt.stex.parsing

import info.kwarc.mmt.api.utils.{File, Unparsed}
import info.kwarc.mmt.stex.STeXError


trait TeXTokenLike {
  val startoffset : Int
  val endoffset: Int
}
trait MacroLike extends TeXTokenLike
class PlainText(val str : String,val startoffset:Int,val endoffset:Int) extends TeXTokenLike
class PlainMacro(val name:String, val startoffset:Int, val endoffset:Int) extends MacroLike
class MacroApplication(val plain:PlainMacro,val children:List[TeXTokenLike],val rule:TeXRule) extends MacroLike {
  val startoffset = plain.startoffset
  val endoffset = children.lastOption.map(_.endoffset).getOrElse(plain.endoffset)
}
class Group(val content:List[TeXTokenLike],val startoffset:Int,val endoffset:Int) extends TeXTokenLike
case class Comment(str : String,startoffset:Int,endoffset:Int) extends TeXTokenLike
case class Math(content:List[TeXTokenLike],startdelim:TeXTokenLike,enddelim:TeXTokenLike) extends TeXTokenLike {
  val startoffset = startdelim.startoffset
  val endoffset = enddelim.endoffset
}
class Environment(begin:MacroApplication,end:MacroApplication,children:List[TeXTokenLike],rule:Option[EnvironmentRule]) extends TeXTokenLike {
  val startoffset = begin.startoffset
  val endoffset = end.endoffset
}

trait TeXRule {
  val name:String
  def readChar(c:Char)(implicit in: Unparsed, state: STeXParserState): (Boolean,List[TeXTokenLike]) = {
    import SuperficialSTeXParser._
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

  def readArg(implicit in: Unparsed, state:STeXParserState) : TeXTokenLike = {
    import SuperficialSTeXParser._
    val start = in.offset
    in.first match {
      case '{' => readGroup
      case o =>
        in.drop(1)
        new PlainText(o.toString,start,in.offset)
    }
  }

  def readOptArg(implicit in: Unparsed, state:STeXParserState) : (List[List[TeXTokenLike]],List[TeXTokenLike]) = {
    import SuperficialSTeXParser._
    in.trim
    var children : List[TeXTokenLike] = Nil
    var args : List[List[TeXTokenLike]] = Nil
    in.first match {
      case '[' =>
        var done = false
        while (!done) {
          val ret = readTop(c => c == ']' || c == ',')
          children = children ::: ret
          args ::= children.filterNot(_.isInstanceOf[Comment]).reverse
          if (in.empty) done = true
          else if (in.first == ',') {
            children = children ::: List(new PlainText(",",in.offset,in.offset+1))
            in.drop(1)
          } else if (in.first == ']') {
            children = children ::: List(new PlainText(",",in.offset,in.offset+1))
            in.drop(1)
            done = true
          }
        }
        (args.reverse,children)
      case _ => (Nil,Nil)
    }
  }

}
abstract class EnvironmentRule(val name:String) extends TeXRule {
  def parse(begin:MacroApplication)(implicit in: Unparsed, state:STeXParserState): MacroApplication
  def finalize(env : Environment) : Environment
}
abstract class MacroRule(val name:String) extends TeXRule {
  def parse(plain:PlainMacro)(implicit in: Unparsed, state:STeXParserState) : TeXTokenLike
  //def apply(plain:PlainMacro,ls:List[TeXTokenLike]) : MacroApplication
}
class SimpleMacroRule(name : String,args:Int=0,maybestarred:Boolean = false) extends MacroRule(name) {
  def after(ma:SimpleMacroApplication)(implicit in: Unparsed, state: STeXParserState) = {}
  override def parse(plain: PlainMacro)(implicit in: Unparsed, state: STeXParserState): MacroApplication = {
    import SuperficialSTeXParser._
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
        case '%' => children ::= readComment
        case _ =>
          val arg = readArg
          children ::= arg
          argls ::= arg
      }
    }
    val ret = new SimpleMacroApplication(plain,children.reverse,starred,argls.reverse,this)
    after(ret)
    ret
  }
}
class SimpleMacroApplication(plain:PlainMacro,children:List[TeXTokenLike],val starred:Boolean,val args:List[TeXTokenLike], rule:TeXRule) extends MacroApplication(plain,children,rule)

object LaTeXRules {
  val end = new SimpleMacroRule("end",1)
  val begin = new MacroRule("begin") {
    override def parse(plain: PlainMacro)(implicit in: Unparsed, state: STeXParserState): TeXTokenLike = {
      readArg match {
        case g : Group => g.content match {
          case List(s : PlainText) =>
            val name = s.str.trim
            val (rule,bg) = state.envrules.find(_.name == name) match {
              case Some(r) =>
                (Some(r),r.parse(new MacroApplication(plain,List(g),this)))
              case _ =>
                (None,new MacroApplication(plain,List(g),this))
            }
            var children : List[TeXTokenLike] = Nil
            var endtk : Option[SimpleMacroApplication] = None
            import SuperficialSTeXParser._
            while ({
              val next = readOne()
              children ::= next
              next match {
                case ma : SimpleMacroApplication if ma.rule == end =>
                  endtk = Some(ma)
                  false
                case _ => true
              }
            }) {}
            val env = new Environment(bg,endtk.getOrElse(bg),children,rule)
            rule.map(_.finalize(env)).getOrElse(env)
          case _ => new MacroApplication(plain,List(g),this)
        }
        case o => new MacroApplication(plain,List(o),this)
      }
    }
  }
  val makeatletter = new SimpleMacroRule("makeatletter") {
    override def after(ma: SimpleMacroApplication)(implicit in: Unparsed, state: STeXParserState): Unit = state.letters ::= '@'
  }
  val makeatother = new SimpleMacroRule("makeatother") {
    override def after(ma: SimpleMacroApplication)(implicit in: Unparsed, state: STeXParserState): Unit = state.letters = state.letters.filterNot(_ == '@')
  }
  val explsyntaxon = new SimpleMacroRule("ExplSyntaxOn") {
    override def after(ma: SimpleMacroApplication)(implicit in: Unparsed, state: STeXParserState): Unit = {
      state.letters ::= '_'
      state.letters ::= ':'
    }
  }
  val explsyntaxoff = new SimpleMacroRule("ExplSyntaxOff") {
    override def after(ma: SimpleMacroApplication)(implicit in: Unparsed, state: STeXParserState): Unit = state.letters = state.letters.filterNot(c => c == '_' || c == ':')
  }
  val dmathend = new SimpleMacroRule("]")
  val dmathstart = new MacroRule("[") {
    override def parse(plain: PlainMacro)(implicit in: Unparsed, state: STeXParserState): Math = {
      import SuperficialSTeXParser._
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
    begin,end
  )
}

object SuperficialSTeXParser {
  def apply(file: File)(rules : List[TeXRule]) : List[TeXTokenLike] = apply(File.read(file))(rules)
  def apply(string : String)(rules : List[TeXRule]) : List[TeXTokenLike] = {
    implicit val in = new Unparsed(string,s => throw new STeXError(s,None,None))
    implicit val state = new STeXParserState(rules)
    readTop()
  }
  def readMacro(implicit in: Unparsed, state:STeXParserState) : TeXTokenLike = {
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
  def readTop(break:Char => Boolean = _ => false)(implicit in: Unparsed, state:STeXParserState) : List[TeXTokenLike] = {
    var ret : List[TeXTokenLike] = Nil
    while(!in.empty && !break(in.first)) ret ::= readOne(break)
    ret.reverse
  }
  def readOne(break:Char => Boolean = _ => false)(implicit in: Unparsed, state:STeXParserState): TeXTokenLike = {
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
  def readMath(starts : String,ends:String)(implicit in: Unparsed, state:STeXParserState) = {
    val start = in.offset
    var ret : List[TeXTokenLike] = Nil
    in.drop(starts)
    state.inmath = true
    try {
      while (!in.empty && in.getnext(ends.length).toString != ends) ret ::= readOne()
    } finally {
      state.inmath = false
    }
    in.drop(ends)
    Math(ret.reverse,new PlainText(starts,start,start + starts.length),new PlainText(ends,in.offset - ends.length,in.offset))
  }
  def readGroup(implicit in: Unparsed, state:STeXParserState) = {
    val start = in.offset
    in.drop("{")
    val ct = readTop(_ == '}')
    in.drop("}")
    new Group(ct,start,in.offset)
  }
  def readText(break:Char => Boolean = _ => false)(implicit in: Unparsed, state:STeXParserState) = {
    val start = in.offset
    val str = in.takeWhile(c => !break(c) && !state.specialchars.contains(c))
    new PlainText(str,start,in.offset)
  }
  def readComment(implicit in: Unparsed, state:STeXParserState) = {
    val start = in.offset
    in.drop("%")
    val str = in.takeWhile(_ != '\n')
    Comment(str,start,in.offset)
  }
}

class STeXParserState(initrules : List[TeXRule]) {
  var inmath = false
  var rules : List[TeXRule] = initrules
  def macrorules = rules.collect{case mr : MacroRule => mr}
  def envrules = rules.collect{case er: EnvironmentRule => er}
  def addRule(rule : TeXRule) = rules ::= rule
  def removeRule(name:String*) = rules.filterNot(name.contains)
  var letters : List[Char] = Nil
  var specialchars : List[Char] = List('\\','%','{','$')
  def isLetter(c:Char) = c.isLetter || letters.contains(c)
}