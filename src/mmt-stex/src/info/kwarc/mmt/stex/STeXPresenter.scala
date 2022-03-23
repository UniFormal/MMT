package info.kwarc.mmt.stex

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.{ObjectPresenter, PresentationContext, RenderingHandler}
import info.kwarc.mmt.api.{CPath, ContentPath, StructuralElement}
import info.kwarc.mmt.stex
import info.kwarc.mmt.stex.rules.StringLiterals
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, ParsingState}
import info.kwarc.mmt.stex.xhtml.{HTMLParser, OMDocHTML}

import scala.xml.Elem

case class STeXNotation(tm : Term, head : ContentPath, macroname : String, notation_used : HTMLNode, fragment: String, arity : String, allnotations : List[(String,String,String,HTMLNode)])

trait STeXPresenter extends ObjectPresenter {
  lazy val server = controller.extman.get(classOf[STeXServer]).head
  protected def getComponents(cp : ContentPath, tm : Term) : Option[STeXNotation] = controller.getO(cp) match {
    case Some(c: StructuralElement) =>
      val notations = OMDocHTML.getNotations(c,controller)
      val notationFragment = tm.metadata.getValues(STeX.meta_notation).headOption match {
        case Some(StringLiterals(s)) => s
        case _ => ""
      }
      // TODO withnotations
      val notation = notations.collectFirst { case (s,p,_, n) if s == notationFragment => n } match {
        case Some(n) =>
          HTMLParser(n.toString)(new ParsingState(controller,server.extensions.flatMap(_.rules)))
        case _ =>
          return None
      }
      val macroname = OMDocHTML.getMacroName(c) match {
        case Some(n) => n
        case _ =>
          "???"
      }
      val arity = c.metadata.getValues(STeX.meta_arity) match {
        case List(StringLiterals(s)) => s
        case _ => ""
      }
      Some(stex.STeXNotation(tm,cp,macroname,notation,notationFragment,arity,notations))
    case None =>
      None
  }

}

class STeXPresenterML extends InformalMathMLPresenter with STeXPresenter {
  def applyWithNotations(o: Obj, origin: Option[CPath], notations : Any /* TODO */)(implicit rh: RenderingHandler): Unit = {
    // TODO
    apply(o,origin)
  }

  private var currentDown = -1000000
  private def withDown[A](prec : Int)(f : => A) = {
    val oldDown = currentDown
    currentDown = prec
    val ret : A = f
    currentDown = oldDown
    ret
  }

  override def recurse(obj: Obj)(implicit pc: PresentationContext): Int = {
    lazy val default = {
      super.recurse(obj)
    }
    obj match {
      case ctx : Context =>
        if (ctx.length == 0) 0
        else if (ctx.length == 1) recurse(ctx.variables.head) else {
          recurse(ctx.variables.head)
          pc.out({
            <mi>,</mi>
          }.toString())
          recurse(Context(ctx.variables.tail: _*))
        }
      case vd : VarDecl =>
        vd.tp match {
          case Some(tm) =>
            pc.out("<mrow><mi>"+vd.name+"</mi><mo>:</mo>")
            recurse(tm)
            pc.out("</mrow>")
          case _ => pc.out({<mi>{vd.name}</mi>}.toString())
        }
        0
      case STeX.informal(n) =>
        val node = HTMLParser(n.toString())(new ParsingState(controller,server.extensions.flatMap(_.rules)))
        node.attributes(("","mathbackground")) = "#ff0000"
        pc.out(node.toString)
        0
      case OMA(OMS(STeX.informal.op.opsym),StringLiterals(label) :: args) =>
        pc.out("<" + label + " mathbackground=\"#ff0000\"" + ">")
        args.foreach(recurse(_))
        pc.out("</" + label + ">")
        0
      case tm@OMS(gn) =>
        val comps = getComponents(gn,tm).getOrElse{
          return default
        }
        pc.out(comps.notation_used.toString)
        0
      case tm@OMA(OMS(gn),args) =>
        val comps = getComponents(gn,tm).getOrElse{
          return default
        }
        val br = 0 // TODO
        var ret = comps.notation_used.toString
        var doarg : List[Unit => Unit] = Nil
        var i = 0
        comps.arity.toList.zipWithIndex.foreach {
          case ('i', _) | ('b', _) =>
            val j = i
            doarg ::= { _: Unit =>
              recurse(args(j))
            }
            i += 1
          case ('a', j) =>
            val n = comps.notation_used.get("mrow")()().reverse.collectFirst {
              case n if n.toString.contains("<mtext>##" + (i + 1) + "a</mtext>") && n.toString.contains("<mtext>##" + (i + 1) + "b</mtext>") =>
                n
            }.getOrElse{
              print("")
              ???
            }
            val nstr = n.toString
            val retargs = if (i == comps.arity.length - 1) args.drop(i)
            else args.drop(i).dropRight(comps.arity.reverse.indexOf('a'))
            i += retargs.length
            ret = ret.replace(nstr, "<mtext>#" + (j + 1) + "</mtext>")
            val pre = nstr.take(nstr.indexOf("<mtext>##" + (j + 1)))
            val next = nstr.drop(pre.length + "<mtext>##</mtext>".length + 2)
            val middle = next.take(next.indexOf("<mtext>##" + (j + 1)))
            val end = next.drop(middle.length + "<mtext>##</mtext>".length + 2)
            doarg ::= { _: Unit =>
              retargs.init.foreach { a =>
                pc.out(pre)
                recurse(a)
                pc.out(middle)
              }
              recurse(retargs.last)
              (1 until retargs.length).foreach(_ => pc.out(end))
            }
          case _ =>
            ???
        }
        doarg = doarg.reverse
        while (ret.nonEmpty) {
          ret.indexOf("<mtext>#") match {
            case -1 =>
              pc.out(ret)
              ret = ""
            case i if ret.charAt(i+8).isDigit =>
              pc.out(ret.take(i))
              ret = ret.drop(i+8)
              val argnum = ret.head.toString.toInt
              ret = ret.drop(1)
              assert(ret.startsWith("</mtext>"))
              ret = ret.drop(8)
              // TODO downwardsprec
              if (!doarg.isDefinedAt(argnum-1)) {
                assert(doarg.isDefinedAt(argnum - 1))
              }
              doarg(argnum-1)(())
            case _ =>
              ???
          }
        }
        br
      case tm@OMBIND(OMS(gn),ctx,arg) if ctx.length == 1 =>
        val comps = getComponents(gn,tm).getOrElse{
          return default
        }
        val br = 0 // TODO
        var ret = comps.notation_used.toString
        while (ret.nonEmpty) {
          // TODO associative args
          ret.indexOf("<mtext>#") match {
            case -1 =>
              pc.out(ret)
              ret = ""
            case i if ret.charAt(i + 8).isDigit =>
              pc.out(ret.take(i))
              ret = ret.drop(i + 8)
              val argnum = ret.head.toString.toInt
              ret = ret.drop(1)
              assert(ret.startsWith("</mtext>"))
              ret = ret.drop(8)
              if (argnum == 1)
                recurse(ctx)
              else if (argnum == 2)
                recurse(arg)
              else {
                ???
              }
            // TODO downwardsprec
          }
        }
        br
      case tm@OMBIND(OMS(gn),ctx,arg) if ctx.length > 1 => recurse(OMBIND(OMS(gn),Context(ctx.variables.head),OMBIND(OMS(gn),Context(ctx.variables.tail:_*),arg)))
      case _ =>
        return default
    }
  }
}

class STeXPresenterTex extends STeXPresenter {
  override def apply(o: Obj, origin: Option[CPath])(implicit rh: RenderingHandler): Unit = o match {
    case t@OMID(cn) =>
      getComponents(cn,t) match {
        case Some(comp) =>
          rh("\\" + comp.macroname + {if (comp.fragment.isEmpty) "" else "[" + comp.fragment + "]"})
        case _ =>
          rh("???")
      }
    case t@OMA(OMS(cn),args) =>
      getComponents(cn,t) match {
        case Some(comp) =>
          rh("\\" + comp.macroname + {if (comp.fragment.isEmpty) "" else "[" + comp.fragment + "]"})
        // TODO associative args
          args.foreach{t =>
            rh("{")
            apply(t,origin)
            rh("}")
          }
        case _ =>
          rh("???")
      }
    case t@OMBINDC(OMS(cn),ctx,args) =>
      getComponents(cn,t) match {
        case Some(comp) =>
          rh("\\" + comp.macroname + {if (comp.fragment.isEmpty) "" else "[" + comp.fragment + "]"})
          // TODO associative args
          // TODO bound variables
          args.foreach{t =>
            rh("{")
            apply(t,origin)
            rh("}")
          }
        case _ =>
          rh("???")
      }
    case o@UnknownOMLIT(valstr,_) => rh(valstr)
    case o@OMLIT(valstr,rt) => rh(rt.semType.toString(valstr))
    case o@OMV(n) =>
      // TODO
      rh("\\" + n)
    case _ =>
      rh("???")
  }
}