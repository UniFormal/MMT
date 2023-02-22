package info.kwarc.mmt.stex.parsing


import info.kwarc.mmt.api.{Error, Level, ParseError}
import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.lsp.SyncedDocUnparsed

import scala.collection.mutable

case class LaTeXParseError(msg : String,extraMsg : Option[String] = None,override val level : Level = Level.Error, tokens: List[TeXTokenLike] = Nil) extends Error(msg) {
  override val extraMessage : String = extraMsg.getOrElse("")
}

trait TeXTokenLike {
  def startoffset : Int
  def endoffset: Int
  def iterate(f : TeXTokenLike => Unit) = f(this)
  val attributes = mutable.HashMap.empty[String,Any]
  var errors : List[LaTeXParseError] = Nil
  def addError(msg : String,extramsg : Option[String] = None,lvl : Level = Level.Error) = {
    errors ::= LaTeXParseError(msg, extramsg, lvl)
  }
  def =?=(that : TeXTokenLike) : Boolean = false
  def asPlain : String = toString
}
object TeXTokenLike {
  def cleanstring(s : String) : String = s.replaceAll("\\s+"," ")
}
case class PlainText(str : String, startoffset:Int,endoffset:Int) extends TeXTokenLike {
  override def toString: String = str
}
case class PlainMacro(name:String, startoffset:Int,endoffset:Int) extends TeXTokenLike {
  override def toString = "\\" + name
}
case class Group(children:List[TeXTokenLike], startoffset:Int,endoffset:Int) extends TeXTokenLike {
  override def iterate(f: TeXTokenLike => Unit): Unit = {
    f(this)
    children.foreach(_.iterate(f))
  }
  override def toString: String = "{" + children.mkString + "}"

  override def asPlain: String = "{" + children.map(_.asPlain).mkString + "}"
}
case class Math(children:List[TeXTokenLike], startdelim:TeXTokenLike,enddelim:TeXTokenLike) extends TeXTokenLike {
  override def iterate(f: TeXTokenLike => Unit): Unit = {
    f(this)
    startdelim.iterate(f)
    children.foreach(_.iterate(f))
    enddelim.iterate(f)
  }
  override def toString: String = startdelim.toString + children.mkString + enddelim.toString

  override def asPlain: String = startdelim.asPlain + children.map(_.asPlain).mkString + enddelim.asPlain
  val startoffset = startdelim.startoffset
  val endoffset = enddelim.endoffset
}

case class Comment(string:String, startoffset:Int) extends TeXTokenLike {
  val endoffset = startoffset + string.length

  override def toString: String = "%" + string
}

case class MathToken(str:String, startoffset:Int) extends TeXTokenLike {
  val endoffset = startoffset + str.length
  override def toString = str
}

class LaTeXParser(stringin:SyncedDocUnparsed,initrules : List[TeXRule] = TeXRules.allrules) {
  object In {
    private[LaTeXParser] var ls:List[TeXTokenLike] = Nil
    def offset = ls.headOption match {
      case Some(t) => t.startoffset
      case _ => stringin.offset
    }
    def peek = ls.headOption
    def empty = ls.headOption match {
      case Some(_) => false
      case _ => stringin.empty
    }
    def take : Option[TeXTokenLike] = ls.headOption match {
      case Some(_:PlainText) => None
      case Some(g : Group) =>
        ls = PlainText("{",g.startoffset,g.startoffset) :: g.children ::: PlainText("}",g.endoffset,g.endoffset) :: ls.tail
        None
      case Some(t) =>
        ls = ls.tail
        Some(t)
      case _ => None
    }
    def first = ls.headOption match {
      case Some(p: PlainText) => p.str.head
      case Some(g: Group) =>
        ls = PlainText("{", g.startoffset, g.startoffset) :: g.children ::: PlainText("}", g.endoffset, g.endoffset) :: ls.tail
        '{'
      case Some(o) => o.toString.head
      case _ => stringin.first
    }
    def next() : Char = ls.headOption match {
      case Some(g: Group) =>
        ls = g.children ::: PlainText("}", g.endoffset, g.endoffset) :: ls.tail
        '{'
      case Some(PlainText(str,_,_)) if str.length == 1 =>
        ls = ls.tail
        str.head
      case Some(PlainText("",_,_)) =>
        ls = ls.tail
        next()
      case Some(PlainText(str,s,e)) =>
        ls = PlainText(str.tail,s+1,e) :: ls.tail
        str.head
      case Some(o) =>
        val str = o.toString
        ls = PlainText(str.tail,o.startoffset + 1,o.endoffset) :: ls.tail
        str.head
      case _ => stringin.next()
    }
    def dropCharMaybe(c: Char): (Boolean,List[TeXTokenLike]) = ls.headOption match {
      case Some(g: Group) =>
        ls = g.children ::: PlainText("}", g.endoffset, g.endoffset) :: ls.tail
        (false,Nil)
      case Some(cm:Comment) =>
        ls = ls.tail
        val ret = dropCharMaybe(c)
        (ret._1,cm :: ret._2)
      case Some(PlainText("", _, _)) =>
        ls = ls.tail
        dropCharMaybe(c)
      case Some(pt@PlainText(str, _, _)) if str.length == 1 && str.head == c =>
        ls = ls.tail
        (true,List(pt))
      case Some(PlainText(str, s, e)) if str.head == c =>
        ls = PlainText(str.tail, s + 1, e) :: ls.tail
        (true,List(PlainText(c.toString,s,s+1)))
      case Some(_) =>
        ???
      case _ =>
        if (stringin.first == c) {
          (true,List(PlainText(stringin.next().toString,stringin.offset - 1,stringin.offset)))
        } else (false,Nil)
    }

    def nextChar : PlainText = ls.headOption match {
      case Some(g: Group) =>
        ls = g.children ::: PlainText("}", g.endoffset, g.endoffset) :: ls.tail
        nextChar
      case Some(cm: Comment) =>
        ls = ls.tail
        nextChar
      case Some(PlainText("", _, _)) =>
        ls = ls.tail
        nextChar
      case Some(pt@PlainText(str, _, _)) if str.length == 1 =>
        ls = ls.tail
        pt
      case Some(PlainText(str, s, e)) =>
        ls = PlainText(str.tail, s + 1, e) :: ls.tail
        PlainText(str.head.toString,s,s+1)
      case Some(_) =>
        ???
      case None =>
        PlainText(stringin.next().toString,stringin.offset - 1,stringin.offset)
    }
    def startsWith(s : String): Boolean = {
      var done = ""
      ls.foreach{tk =>
        done += tk.toString
        if (done.length >= s.length) {
          return done.startsWith(s)
        }
      }
      if (s.startsWith(done)) {
        val next = s.drop(done.length)
        stringin.startsWith(next)
      } else false
    }
    def drop(s: String): Unit = {
      if (!startsWith(s)) throw ParseError(s + " expected")
      s.foreach(_ => next())
    }
    def trim : Unit = ls.headOption match {
      case Some(PlainText(str,_,_)) if str.isEmpty =>
        ls = ls.tail
        trim
      case Some(PlainText(str,s,e)) =>
        val nstr = str.trim
        if (nstr != str) {
          ls = PlainText(nstr,s + (str.length - nstr.length),e) :: ls.tail
        }
      case Some(_) =>
      case None =>
        stringin.trim
    }

    def enqueue(tk: TeXTokenLike) = ls ::= tk

    def enqueueAll(nls: List[TeXTokenLike]) = ls = nls ::: ls

    def stringUntil(f: => Boolean) : String = {
      val sb = new StringBuilder()
      while (!f) {
        ls.headOption match {
          case Some(s) => sb.append(s.toString)
          case _ =>
            sb += stringin.next()
        }
      }
      sb.mkString
    }

    /*
    def head = ls.headOption
    def nonEmpty = ls.nonEmpty
    def dequeue() : TeXTokenLike = {
      val h :: t = ls
      ls = t
      h
    }
     */
  }

  def readOne(break: => Boolean = { false }): TeXTokenLike = {
    if (In.empty) throw LaTeXParseError("Unexpected end of file")
    In.take match {
      case Some(pm:PlainMacro) =>
        return doMacro(pm)
      case Some(mt:MathToken) => return readMathI(mt)
      case Some(c:Comment) => return c
      case Some(pt:PlainText) => return pt
      case Some(o) =>
        println(o)
        ???
      case _ =>
    }
    In.first match {
      case '\\' => doMacro(readMacro)
      case '{' => readGroup
      case '}' =>
        if (!inGroup) {
          In.next()
          val ret = PlainText("}", In.offset - 1, In.offset)
          ret.addError("No group here to end")
          ret
        } else
          throw LaTeXParseError("group should not end here")
      case '$' if In.startsWith("$$") => readMath("$$")
      case '$' => readMath("$")
      case '%' =>
        readComment
      case _ => readText { break }
    }
  }

  def readOneNoRules(break: => Boolean = { false }): TeXTokenLike = {
    if (In.empty) throw LaTeXParseError("Unexpected end of file")
    In.take match {
      case Some(pm: PlainMacro) => return pm
      case Some(mt: MathToken) => return mt
      case Some(c: Comment) => return c
      case Some(o) =>
        println(o)
        ???
      case _ =>
    }
    In.first match {
      case '\\' => readMacro
      case '{' => readGroupNoRules
      case '}' =>
        if (!inGroup) {
          In.next()
          val ret = PlainText("}", In.offset - 1, In.offset)
          ret.addError("No group here to end")
          ret
        } else
          throw LaTeXParseError("group should not end here")
      case '$' if In.startsWith("$$") =>
        val starts = In.offset
        In.drop("$$")
        MathToken("$$", starts)
      case '$' =>
        val starts = In.offset
        In.drop("$")
        MathToken("$", starts)
      case '%' =>
        readComment
      case _ => readText {
        break
      }
    }
  }

  def next(break: => Boolean = { false }): TeXTokenLike = In.take match {
    case Some(t) => t
    case _ => readOne { break }
  }

  def nextNoRules(break: => Boolean = { false }): TeXTokenLike = In.take match {
    case Some(t) => t
    case _ => readOneNoRules { break }
  }

  def readNoRules(break: => Boolean = { false }): List[TeXTokenLike] = {
    var ret: List[TeXTokenLike] = Nil
    while (!In.empty && !break) {
      ret ::= nextNoRules {
        break
      }
    }
    ret.reverse
  }

  def readTop(break: => Boolean = { false }): List[TeXTokenLike] = {
    var ret: List[TeXTokenLike] = Nil
    try {
      while (!In.empty && !break) {
        next { break } match {
          case pm: PlainMacro =>
            getMacroRule(pm.name) match {
              case None => ret ::= pm
              case Some(rl) =>
                rl.parse(pm)(this) match {
                  case Some(ma) => ret ::= ma
                  case _ => ret ::= pm
                }
            }
          case n => ret ::= n
        }
      }
    } catch {
      case lpe: LaTeXParseError =>
        throw LaTeXParseError(lpe.msg, lpe.extraMsg, lpe.level, ret.reverse ::: lpe.tokens)
    }
    ret.reverse
  }

  private def readMath(start: String) = {
    val starts = In.offset
    In.drop(start)
    val starttk = MathToken(start, starts)
    readMathI(starttk)
  }
  private def readMathI(starttk:MathToken) = {
    if (inmath) throw LaTeXParseError("Unexpected Math Token", tokens = List(starttk))
    val break = starttk.toString
    val ret = inMath {
      readTop {
        In.peek match {
          case Some(mt:MathToken) => mt.toString == break
          case _ =>
            In.startsWith(break)
        }
      }
    }
    val endtk = In.peek match {
      case Some(mt: MathToken) =>
        In.take.asInstanceOf[MathToken]
      case _ =>
        val curr = In.offset
        val ret = MathToken(break, curr)
        try{ In.drop(break) } catch {
          case ParseError(s) =>
            ret.addError(s)
        }
        ret
    }
    Math(ret, starttk, endtk)
  }

  def read: List[TeXTokenLike] = try { readTop() } catch {
    case e: LaTeXParseError if e.tokens.nonEmpty =>
      e.tokens.last.errors ::= e
      e.tokens ::: read
    case pe: LaTeXParseError =>
      pe.printStackTrace()
      Nil
  }

  var inmath = false
  var groups: List[RuleContainer] = List({
    val rc = new RuleContainer
    rc.letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    initrules.foreach(rl => rc.rules(rl.name) = rl)
    rc
  })

  def getRule(name:String) : Option[TeXRule] = {
    groups.foreach(_.rules.get(name).foreach{
      rule => return Some(rule)
    })
    None
  }
  def getMacroRule(name:String) : Option[MacroRule] = getRule(name) match {
    case Some(mr : MacroRule) => Some(mr)
    case _ => None
  }

  def collectFirstRule[A](f : PartialFunction[TeXRule,A] ) : Option[A] = {
    groups.foreach { g =>
      g.rules.values.foreach {
        case f(s) => return Some(s)
        case _ =>
      }
    }
    None
  }
  def collectRules[A](f: PartialFunction[TeXRule,A]) : List[A] = {
    var ret : List[A] = Nil
    groups.foreach { g =>
      g.rules.values.foreach {
        case f(s) => ret ::= s
        case _ =>
      }
    }
    ret.reverse
  }

  def macrorules = groups.view.flatMap(_.rules.values).collect { case mr: MacroRule => mr }.toList.distinct

  def getRules = groups.view.flatMap(_.rules.values).toList.distinct

  def envrules = groups.view.flatMap(_.rules.values).collect { case er: EnvironmentRule => er }.toList.distinct

  def addRule(rule: TeXRule, global: Boolean = false) = if (global) groups.foreach(_.rules(rule.name) = rule) else groups.head.rules(rule.name) = rule

  def opengroup = {
    groups ::= RuleContainer.from(groups.head)
  }

  private def safely[A](f : => A) = try { f } catch {
    case ParseError(s) => throw LaTeXParseError(s)
  }

  def closegroup = {
    if (groups.length <= 1) throw LaTeXParseError("No group here to close")
    groups = groups.tail
  }

  def inGroup[A](f : => A) = {
    opengroup
    try { f } finally { closegroup }
  }

  def inMath[A](f : => A) = {
    val currmath = inmath
    inmath = true
    try { f } finally { inmath = currmath }
  }
  def escapeMath[A](f : => A) = {
    val currmath = inmath
    inmath = false
    try { f } finally { inmath = currmath }
  }

  var inGroup = false
  private def readGroup : Group = {
    val start = In.offset
    In.drop("{")
    val oldgroup = inGroup
    val rets = try {
      inGroup = true
      readTop{ In.first == '}' }
    } finally {
      inGroup = oldgroup
    }
    try {
      In.drop("}")
      Group(rets, start, In.offset)
    } catch {
      case e:ParseError =>
        val ret = Group(rets, start, In.offset)
        ret.errors ::= LaTeXParseError("Missing '}'")
        ret
    }
  }

  private def readGroupNoRules: Group = {
    val start = In.offset
    In.drop("{")
    val rets = readNoRules {
      In.first == '}'
    }
    In.drop("}")
    Group(rets, start, In.offset)
  }
  private def readComment : Comment = {
    if (In.ls.nonEmpty) {
      ???
    }
    val start = stringin.offset
    stringin.drop("%")
    var str = stringin.takeWhileSafe(c => c != '\n' && c != '\r')
    str = str + stringin.takeWhileSafe(c => c == '\n' || c == '\r' || c == ' ' || c == '\t')
    Comment(str,start)
  }
  def doMacro(pm:PlainMacro) = {
    getMacroRule(pm.name) match {
      case None => pm
      case Some(rl) =>
        rl.parse(pm)(this) match {
          case Some(ma) => ma
          case _ => pm
        }
    }
  }
  def readMacro : PlainMacro = safely {
    In.peek match {
      case Some(_:PlainMacro) =>
        return In.take.asInstanceOf[PlainMacro]
      case _ =>
    }
    if (In.ls.nonEmpty) {
      ???
    }
    val start = stringin.offset
    stringin.drop("\\")
    val letters = groups.head.letters
    val first = stringin.next()
    val name = if (letters.contains(first)) {
      first.toString + stringin.takeWhileSafe(letters.contains(_))
    } else first.toString
    PlainMacro(name,start,stringin.offset)
  }

  private val breakchars = ",[]{}=\\$%.-_|?!()/;:+*~#'&§@"
  def readText(break: => Boolean = { false }): PlainText = safely {
    In.ls.headOption match {
      case Some(pt:PlainText) =>
        In.ls = In.ls.tail
        return pt
      case _ =>
    }
    if (In.ls.nonEmpty) {
      ???
    }
    val start = stringin.offset
    var str = stringin.takeWhileSafe(c => !break && !breakchars.contains(c))
    if (str.isEmpty && !break) {
      str = In.next().toString
    }
    PlainText(str, start, In.offset)
  }

}
/*
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

  def readMacroNoRule(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
    val start = in.offset
    in.drop("\\")
    val first = in.first
    val str = if (state.isLetter(first)) {
      in.takeWhile(state.isLetter)
    } else {
      in.drop(1)
      first.toString
    }
    val plain = new PlainMacro(str, start, in.offset)
    //val ret = state.macrorules.find(_.name == plain.name).map(_.parse(plain)).getOrElse(plain)
    //in.trim
    //ret
    plain
  }
  def readTop(break:Char => Boolean = _ => false)(implicit in: SyncedDocUnparsed, state:LaTeXParserState) : List[TeXTokenLike] = {
    var ret : List[TeXTokenLike] = Nil
    while(!in.empty && !break(in.first)) try {
      ret ::= readOne(break)
    } catch {
      case e:LaTeXParseError =>
        ret.headOption match {
          case Some(tk) => tk.addError(e.s,e.extramsg,e.lvl)
          case None =>
        }
    }
    ret.reverse
  }
  def readOne(break:Char => Boolean = _ => false)(implicit in: SyncedDocUnparsed, state:LaTeXParserState): TeXTokenLike = {
    if (in.empty) {
      throw LaTeXParseError("Unexpected end of file")
    }
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
  def readOneNoRule(break: Char => Boolean = _ => false)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
    if (in.empty) {
      throw LaTeXParseError("Unexpected end of file")
    }
    in.first match {
      case '\\' => readMacroNoRule
      case '{' => readGroup
      case '%' => readComment
      case '$' if in.getnext(2).toString == "$$" =>
        readMath("$$", "$$")
      case '$' => readMath("$", "$")
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
  def readComment(implicit in: SyncedDocUnparsed, state:LaTeXParserState) = if (state.rules.nonEmpty) {
    val start = in.offset
    in.drop("%")
    val str = in.takeWhileSafe(_ != '\n')
    if (!in.empty && in.first == '\n')
      in.drop("\n")
    else if (!in.empty)
      in.drop("\r\n")
    Comment(str,start,in.offset)
  } else {
    val start = in.offset
    in.drop("%")
    new PlainText("%",start,in.offset)
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
  def closegroup = {
    if (rules.isEmpty) throw LaTeXParseError("No group here to close")
    rules = rules.tail
  }
  def macrorules = rules.view.flatMap(_.rules).collect{case mr : MacroRule => mr}.toList.distinct
  def getRules = rules.view.flatMap(_.rules).toList.distinct
  def envrules = rules.view.flatMap(_.rules).collect{case er: EnvironmentRule => er}.toList.distinct
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
  val let = new MacroRule {
    override val name: String = "let"

    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
      val rules = state.rules
      state.rules = Nil
      val ch = try {(readArg, readArg)} finally {
        state.rules = rules
      }
      new SimpleMacroApplication(plain, ch._1._2 ::: ch._2._2, false, List(ch._1._1, ch._2._1), this)
    }
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
  class MathEnd(s: String) extends SimpleMacroRule(s) {
    override def after(ma: SimpleMacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): Unit = {
      state.closegroup
      super.after(ma)
    }
  }
  val dmathend = new MathEnd("]")
  val pmathend = new MathEnd(")")
  class MathStart(val name:String,end:MathEnd) extends MacroRule {
    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): Math = {
      import SuperficialLaTeXParser._
      state.opengroup
      state.inmath = true
      var children: List[TeXTokenLike] = Nil
      try {
        while ( {
          val next = readOne()
          children ::= next
          next match {
            case ma: SimpleMacroApplication if ma.rule == end =>
              false
            case _ => true
          }
        }) {}
      } finally {
        state.inmath = false
      }
      Math(children.tail.reverse, new MacroApplication(plain, Nil, this), children.head)
    }
  }
  val dmathstart = new MathStart("[",dmathend)
  val pmathstart = new MathStart("(",pmathend)
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
  val text = new SimpleMacroRule("text",args=1)  {
    override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
      val math = state.inmath
      state.inmath = false
      val (ret,ch) = try { readArg } finally { state.inmath = math }
      new SimpleMacroApplication(plain,ch,false,Nil,this)
    }
  }

  val allrules = List(
    makeatletter,
    makeatother,
    explsyntaxon,
    explsyntaxoff,
    dmathstart, dmathend, ensuremath,pmathstart,pmathend,
    begin,end,
    _def,edef,let,
    array,array_star,eqnarray,eqnarray_star,align,align_star,displaynd,displaytableau,displaytableau_star,textnd,cboxnd,tableau,
    capital_displaynd,fignd,text,
    verbatim,lstlisting,lstinline,inlineverb,verb,iffalse,
    new NoLintRule("url")
  )
}*/