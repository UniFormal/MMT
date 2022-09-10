package info.kwarc.mmt.stex.parsing

import info.kwarc.mmt.lsp.SyncedDocUnparsed

import scala.collection.mutable


trait TeXRule {
  val noargs = List("}", "\\begin", "\\end", "$", "\\]", "\\[")
  def safely[A <: TeXTokenLike](elem: => A)(f : => A): A = try { f } catch {
    case LaTeXParseError(s,e,l) =>
      val ge = elem
      ge.addError(s,e,l)
      ge
  }
  def name:String
  def readChar(c:Char)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): (Boolean,List[TeXTokenLike]) = {
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

  def readSafeArg(top:String)(implicit in: SyncedDocUnparsed, state:LaTeXParserState) : (TeXTokenLike,List[TeXTokenLike]) = {
    import SuperficialLaTeXParser._
    var children : List[TeXTokenLike] = Nil
    var done = false
    in.trim
    while (!done && !in.empty) {
      if (noargs.exists(in.startsWith)) {
        throw LaTeXParseError(noargs.find(in.startsWith).get + " should not start an argument for " + top)
      }
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
    throw LaTeXParseError("File ended unexpectedly")
  }

  def readArg(implicit in: SyncedDocUnparsed, state:LaTeXParserState) : (TeXTokenLike,List[TeXTokenLike]) = {
    import SuperficialLaTeXParser._
    var children : List[TeXTokenLike] = Nil
    var done = false
    in.trim
    while (!done && !in.empty) {
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
    throw LaTeXParseError("File ended unexpectedly")
  }

  def readOptArg(implicit in: SyncedDocUnparsed, state:LaTeXParserState) : (List[List[TeXTokenLike]],List[TeXTokenLike]) = {
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
            if (in.empty) {
              done = true
              throw LaTeXParseError("File ended unexpectedly")
            }
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

trait MacroRule extends TeXRule {
  def name:String
  def parse(plain:PlainMacro)(implicit in: SyncedDocUnparsed, state:LaTeXParserState) : TeXTokenLike
  //def apply(plain:PlainMacro,ls:List[TeXTokenLike]) : MacroApplication
}

class SimpleMacroRule(val name : String,args:Int=0,maybestarred:Boolean = false) extends MacroRule {
  def after(ma:SimpleMacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState) = {}
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
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

abstract class EnvironmentRule(val name:String) extends TeXRule {
  def parse(begin:MacroApplication)(implicit in: SyncedDocUnparsed, state:LaTeXParserState): MacroApplication
  def finalize(env : Environment)(implicit state:LaTeXParserState) : Environment
}

trait VerbatimLikeRule extends TeXRule {
  def readVerb(finish:String)(implicit in: SyncedDocUnparsed, state:LaTeXParserState) = {
    val currrules = state.rules
    state.rules = Nil
    val start = in.offset
    val sb = new mutable.StringBuilder()
    try {
      while (!in.empty && !in.startsWith(finish)) {
        sb += in.next()
      }
    } finally {
      state.rules = currrules
    }
    (!in.empty,new PlainText(sb.mkString,start,in.offset))
  }
}

class InlineVerbRule(val name: String) extends MacroRule with VerbatimLikeRule {
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = {
    import SuperficialLaTeXParser._
    var done = false
    var children: List[TeXTokenLike] = readOptArg._2.reverse
    while (!in.empty && !done) {
      in.trim
      in.first match {
        case '%' => children ::= readComment
        case c =>
          done = true
          children ::= new PlainText(c.toString, in.offset, in.offset + 1)
          in.drop(c.toString)
          val (terminated, ch) = readVerb(c.toString)
          if (terminated) {
            children ::= new PlainText(c.toString, in.offset, in.offset + 1)
            in.drop(c.toString)
          }
          val res = new MacroApplication(plain, (ch :: children).reverse, this)
          if (!terminated) res.addError("\\" + name + " not properly terminated")
          return res
      }
    }
    val res = new MacroApplication(plain, children.reverse, this)
    res.addError("\\" + name + " not properly terminated")
    res
  }
}

trait MathMacroRule extends MacroRule {
  def inmath[A](f : => A)(implicit state:LaTeXParserState): A = {
    val prevmath = state.inmath
    state.inmath = true
    try { f } finally {state.inmath = prevmath}
  }
}

trait MathEnvRule extends EnvironmentRule {
  case class MathMacroAppl(pl:PlainMacro,ch:List[TeXTokenLike],rl:TeXRule,prev:Boolean) extends MacroApplication(pl,ch,rl)
  override def parse(begin: MacroApplication)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): MacroApplication = {
    val ret = MathMacroAppl(begin.plain,begin.children,this,state.inmath)
    state.inmath = true
    ret
  }

  override def finalize(env: Environment)(implicit state: LaTeXParserState): Environment = {
    env.begin match {
      case MathMacroAppl(_, _, _, prev) => state.inmath = prev
      case _ =>
    }
    env
  }
}

trait DefLikeRule extends MacroRule {
  override def parse(plain: PlainMacro)(implicit in: SyncedDocUnparsed, state: LaTeXParserState): TeXTokenLike = safely[TeXTokenLike](plain) {
    import SuperficialLaTeXParser._
    val pre = readTop(c => c == '{')
    val currrules = state.rules
    val main = try {
      state.rules = Nil
      readArg
    } finally {
      state.rules = currrules
    }
    new SimpleMacroApplication(plain,pre ::: main._2,false,Nil,this)
  }
}