package info.kwarc.mmt.stex.parsing

import info.kwarc.mmt.api.Level
import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.lsp.{SyncedDocUnparsed, SyncedDocument}
import info.kwarc.mmt.stex.STeXError

import scala.collection.mutable

case class LaTeXParseError(s : String,extramsg : Option[String] = None,lvl : Level = Level.Error) extends Throwable

object SuperficialLaTeXParser {
  def apply(file: File,rules : List[TeXRule] = LaTeXRules.allrules) : List[TeXTokenLike] = apply(File.read(file),rules)
  def apply(string : String,rules : List[TeXRule]) : List[TeXTokenLike] = {
    val nd = new SyncedDocument
    nd.set(string)
    apply(nd,rules)
  }
  def apply(sd : SyncedDocument,rules:List[TeXRule]): List[TeXTokenLike] = {
    implicit val in = sd.unparsed
    implicit val state = new LaTeXParserState(rules)
    readTop()
  }
  def readMacro(implicit in: SyncedDocUnparsed, state:LaTeXParserState) : TeXTokenLike = {
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
  def readTop(break:Char => Boolean = _ => false)(implicit in: SyncedDocUnparsed, state:LaTeXParserState) : List[TeXTokenLike] = {
    var ret : List[TeXTokenLike] = Nil
    while(!in.empty && !break(in.first)) ret ::= readOne(break)
    ret.reverse
  }
  def readOne(break:Char => Boolean = _ => false)(implicit in: SyncedDocUnparsed, state:LaTeXParserState): TeXTokenLike = {
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
  def readMath(starts : String,ends:String)(implicit in: SyncedDocUnparsed, state:LaTeXParserState) = {
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
    val res = Math(ret.reverse,new PlainText(starts,start,start + starts.length),new PlainText(ends,in.offset,in.offset + ends.length))
    if (in.empty) {
      res.addError("Math environment ends unexpectedly")
    } else in.drop(ends)
    res
  }
  def readGroup(implicit in: SyncedDocUnparsed, state:LaTeXParserState) = {
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
  def readText(break:Char => Boolean = _ => false)(implicit in: SyncedDocUnparsed, state:LaTeXParserState) = {
    val start = in.offset
    val str = in.takeWhileSafe(c => !break(c) && !state.specialchars.contains(c))
    new PlainText(str,start,in.offset)
  }
  def readComment(implicit in: SyncedDocUnparsed, state:LaTeXParserState) = {
    val start = in.offset
    in.drop("%")
    val str = in.takeWhileSafe(_ != '\n')
    if (!in.empty && in.first == '\n')
      in.drop("\n")
    else if (!in.empty)
      in.drop("\r\n")
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
  def getRules = rules.flatMap(_.rules)
  def envrules = rules.flatMap(_.rules).collect{case er: EnvironmentRule => er}
  def addRule(rule : TeXRule,global:Boolean = false) = if (global) rules.foreach(_.rules ::= rule) else rules.head.rules ::= rule
  var letters : List[Char] = Nil
  var specialchars : List[Char] = List('\\','%','{','$')
  def isLetter(c:Char) = c.isLetter || letters.contains(c)
}

class RuleContainer {
  var rules:List[TeXRule] = Nil
}


object LaTeXRules {

  val _def = new MacroRule with DefLikeRule {
    override val name: String = "def"
  }
  val edef = new MacroRule with DefLikeRule {
    override val name: String = "edef"
  }

  val end = new SimpleMacroRule("end",1) {
    override def after(ma: SimpleMacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): Unit = {
      state.closegroup
      super.after(ma)
    }
  }
  val begin = new MacroRule {
    val name = "begin"
    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
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
              case h :: _ => h
            }}




            try {
              while ( {
                val next = readOne()
                next match {
                  case ma: SimpleMacroApplication if ma.rule == end =>
                    endtk = Some(ma)
                    false
                  case _ =>
                    children ::= next
                    true
                }
              }) {}
              val env = new Environment(bg,getEnd,children.reverse,rule)
              getEnd match {
                case ma:SimpleMacroApplication if ma.rule == end =>
                  ma.children match {
                    case List(gr : Group) =>
                      gr.content match {
                        case List(pt:PlainText) =>
                          if (pt.str != name) {
                            throw LaTeXParseError("\\begin{" + name + "} ended by " + pt.str)
                          }
                        case _ =>
                      }
                    case _ =>
                  }
                case _ =>
              }
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
    override def after(ma: SimpleMacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): Unit = state.letters ::= '@'
  }
  val makeatother = new SimpleMacroRule("makeatother") {
    override def after(ma: SimpleMacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): Unit = state.letters = state.letters.filterNot(_ == '@')
  }
  val explsyntaxon = new SimpleMacroRule("ExplSyntaxOn") {
    override def after(ma: SimpleMacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): Unit = {
      state.letters ::= '_'
      state.letters ::= ':'
    }
  }
  val explsyntaxoff = new SimpleMacroRule("ExplSyntaxOff") {
    override def after(ma: SimpleMacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): Unit = state.letters = state.letters.filterNot(c => c == '_' || c == ':')
  }
  val dmathend = new SimpleMacroRule("]") {
    override def after(ma: SimpleMacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): Unit = {
      state.closegroup
      super.after(ma)
    }
  }
  val dmathstart = new MacroRule {
    val name = "["
    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): Math = {
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
  val eqnarray = new EnvironmentRule("eqnarray") with MathEnvRule
  val eqnarray_star = new EnvironmentRule("eqnarray*") with MathEnvRule
  val array = new EnvironmentRule("array") with MathEnvRule
  val array_star = new EnvironmentRule("array*") with MathEnvRule
  val align = new EnvironmentRule("align") with MathEnvRule
  val align_star = new EnvironmentRule("align*") with MathEnvRule
  val displaynd = new EnvironmentRule("displaynd") with MathEnvRule
  val capital_displaynd = new EnvironmentRule("Displaynd") with MathEnvRule
  val displaytableau = new EnvironmentRule("displaytableau") with MathEnvRule
  val displaytableau_star = new EnvironmentRule("displaytableau*") with MathEnvRule
  val tableau = new EnvironmentRule("tableau") with MathEnvRule
  val textnd = new EnvironmentRule("textnd") with MathEnvRule
  val cboxnd = new EnvironmentRule("cboxnd") with MathEnvRule
  val fignd = new EnvironmentRule("fignd") with MathEnvRule

  val verbatim = new EnvironmentRule("verbatim") with VerbatimLikeRule {
    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      val (terminated,nch) = readVerb("\\end{verbatim}")
      val ret = new MacroApplication(begin.plain,begin.children ::: nch :: Nil,this)
      if (!terminated) {
        ret.addError("verbatim environment closed unexpectedly")
      }
      ret
    }

    override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = env
  }
  val lstlisting = new EnvironmentRule("lstlisting") with VerbatimLikeRule {
    override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      val (terminated,nch) = readVerb("\\end{lstlisting}")
      val ret = new MacroApplication(begin.plain,begin.children ::: nch :: Nil,this)
      if (!terminated) {
        ret.addError("lstlisting environment closed unexpectedly")
      }
      ret
    }

    override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = env
  }
  val iffalse = new MacroRule with VerbatimLikeRule {
    override val name: String = "iffalse"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
      val (tm,ch) = readVerb("\\fi")
      val res = new MacroApplication(plain,List(ch),this)
      if (!tm) res.addError("\\iffalse not properly terminated")
      res
    }
  }
  val lstinline = new InlineVerbRule("lstinline")
  val inlineverb = new InlineVerbRule("inlineverb")
  val verb = new InlineVerbRule("verb")

  val ensuremath = new SimpleMacroRule("ensuremath",args=1) with MathMacroRule {
    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = inmath { super.parse(plain) }
  }

  val allrules = List(
    makeatletter,
    makeatother,
    explsyntaxon,
    explsyntaxoff,
    dmathstart, dmathend, ensuremath,
    begin,end,
    _def,edef,
    array,array_star,eqnarray,eqnarray_star,align,align_star,displaynd,displaytableau,displaytableau_star,textnd,cboxnd,tableau,
    capital_displaynd,fignd,
    verbatim,lstlisting,lstinline,inlineverb,verb,iffalse
  )
}