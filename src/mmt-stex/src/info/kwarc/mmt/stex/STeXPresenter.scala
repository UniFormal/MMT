package info.kwarc.mmt.stex

import info.kwarc.mmt.api.checking.History
import info.kwarc.mmt.api.notations.TextNotation
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.{ObjectPresenter, PresentationContext, RenderingHandler, VarData}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, Structure}
import info.kwarc.mmt.api.{CPath, ComplexStep, ContentPath, GlobalName, LocalName, StructuralElement}
import info.kwarc.mmt.stex
import info.kwarc.mmt.stex.Extensions.NotationExtractor
import info.kwarc.mmt.stex.rules.{Getfield, ModelsOf, ModuleType, StringLiterals}
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText, ParsingState}
import info.kwarc.mmt.stex.xhtml.{HTMLParser, OMDocHTML}

import scala.xml.{Elem, Node}

case class STeXNotation(tm : Term, head : ContentPath, macroname : String, notation_used : HTMLNode, fragment: String, arity : String, allnotations : List[(String,String,String,HTMLNode)])

trait STeXPresenter extends ObjectPresenter {
  lazy val server = controller.extman.get(classOf[STeXServer]).head

  protected def getMainNotation(s : GlobalName) = {
    val allNotations = OMDocHTML.getNotations(s)(controller).reverse
    allNotations.collectFirst {
      case t if t._1 == s.module => t
    } match {
      case None => allNotations.headOption
      case o => o
    }
  }
  protected def getMainNotation(s : LocalName)(implicit context:Context) = {
    context.findLast(_.name == s).flatMap(_.metadata.get(STeX.notation.tp.sym).headOption.map(_.value)) match {
      case Some(STeX.notation(n,s1,s2,o)) =>
        Some((n,s1,s2,o))
      case _ => None
    }
  }

  private def getImplicitTerm(t : Term)(implicit context:Context) : Option[Term] = {
    t match {
      case OMV(n) => context.findLast(_.name== n) match {
        case Some(vd) =>
          vd.tp match {
            case Some(t) => Some(t)
            case _ => vd.df
          }
        case _ => None
      }
      case OMS(p) => controller.getO(p).flatMap {
        case c : Constant =>
          c.tp match {
            case Some(t) => Some(t)
            case _ => c.df
          }
        case _ => None
      }
      case Getfield(t, f) => OMDocHTML.getOriginal(t, f)(controller,context).flatMap(c => getImplicitTerm(OMS(c.path)))
      case _ => None
    }

  }

  protected def implicits(t : Term)(implicit context : Context) : (List[SOMBArg],String,Term) = {
    def getArgs(tp : Term,args : List[SOMBArg]) : (List[SOMBArg],List[SOMBArg]) = tp match {
      case STeX.implicit_binder(_,_,bd) =>
        val (i,r) = getArgs(bd,args.tail)
        (args.head :: i,r)
      case _ => (Nil,args)
    }
    t match {
      case SOMA(f,args) => implicits(OMA(f,args))
      case OMA(f,args) =>
        val (i,r) = getImplicitTerm(f) match {
          case Some(t) => getArgs(t,args.map(STerm))
          case _ => (Nil,args.map(STerm))
        }
        val (ii,rr,s) = reorder(OMA(f,r.map(_.asInstanceOf[STerm].tm) ))
        ((i ::: ii).distinct,s,rr)
      case SOMB(f,args) =>
        val (i,r) = getImplicitTerm(f) match {
          case Some(t) => getArgs(t,args)
          case _ => (Nil,args)
        }
        val (ii,rr,s) = reorder(SOMB(f,r:_*))
        ((i ::: ii).distinct,s,rr)
      case _ => (Nil,"",t)
    }
  }

  protected def reorder(t : Term)(implicit context:Context) : (List[SOMBArg],Term,String) = {
    t match {
      case OMA(f,args) =>
        val (ros,assoc,arity) = OMDocHTML.getRuleInfo(f)(controller,context).getOrElse{ return (Nil,t,"") }
        val reordered = ros match {
          case Nil => args
          case ls => (1 to ls.max).map(i => args(ls.indexOf(i) + 1))
        }
        (assoc,arity,reordered) match {
          case (Some("bin"|"binr"),"a",List(a,b)) =>
            b match {
              case SOMA(`f`,_) | OMA(`f`,_) =>
                val (is,_,nit) = implicits(b)
                nit match {
                  case OMA(`f`,List(STeX.flatseq(na))) =>
                    (is,OMA(f,List(STeX.flatseq(a :: na :_*))),arity)
                  case _ =>
                    (is,OMA(f,reordered.toList),arity)
                }
              case _ =>
                (Nil,OMA(f,List(STeX.flatseq(reordered:_*))),arity)
            }
          case (Some("bin"|"binr"),"ai",List(a,b)) =>
            b match {
              case SOMA(`f`,_) | OMA(`f`,_) =>
                val (is,_,nit) = implicits(b)
                nit match {
                  case OMA(`f`,List(STeX.flatseq(na),l)) =>
                    (is,OMA(f,List(STeX.flatseq(a :: na :_*),l)),arity)
                  case _ =>
                    (is,OMA(f,List(STeX.flatseq(a),b)),arity)
                }
              case _ =>
                (Nil,OMA(f,List(STeX.flatseq(a),b)),arity)
            }
          case (Some("conj"),"a",List(a,b)) =>
            (Nil,OMA(f,List(STeX.flatseq(a,b))),arity)
          case (Some(s),_,_) =>
            (Nil,OMA(f,reordered.toList),arity)
          case (_,_,_) => (Nil,OMA(f,reordered.toList),arity)
        }
      case SOMB(f,args) =>
        val (ros,assoc,arity) = OMDocHTML.getRuleInfo(f)(controller,context).getOrElse{ return (Nil,t,"") }
        val reordered = ros match {
          case Nil => args
          case ls => (1 to ls.max).map(i => args(ls.indexOf(i) + 1))
        }
        (assoc,arity,reordered) match {
          case (Some("pre"),"Bi",List(SCtx(ctx1),STerm(it))) =>
            it match {
              case SOMB(`f`,_) =>
                val (is,_,nit) = implicits(it)(context ++ ctx1)
                nit match {
                  case SOMB(`f`,List(SCtx(ctx2),bd)) =>
                    (is,SOMB(f,List(SCtx(ctx1 ++ ctx2),bd) :_*),arity)
                  case _ =>
                    (is,SOMB(f,reordered.toList :_*),arity)
                }
              case _ =>
                (Nil,SOMB(f,reordered.toList :_*),arity)
            }
          case (Some("pre"),_,_) =>
            (Nil,SOMB(f,reordered.toList :_*),arity)
          case (_,_,_) =>
            (Nil,SOMB(f,reordered.toList :_*),arity)
        }
      case _ => (Nil,t,"")
    }
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
  private def doAssoc(n : HTMLNode,arg : SOMBArg, args:List[(Char,SOMBArg)],owner:String)(implicit pc: PresentationContext,htmlstate : HTMLParser.ParsingState) : Unit = {
    def doPair(arg1: Unit => Unit, arg2: Unit => Unit) : Unit => Unit = {
      def rec(n : HTMLNode) : Unit = {
        val property = n.attributes.getOrElse((n.namespace,"property"),"")
        val resource = n.attributes.getOrElse((n.namespace,"resource"),"")
        n match {
          case t : HTMLText => pc.out(t.text)
          case n if property == "stex:arg" && (resource.startsWith("a") || resource.startsWith("B")) =>
            val i = resource.last.toString.toInt
            if (args.isDefinedAt(i-1)) {
              val newcontext = pc.context ::: (args.take(i-1).collect {
                case (_,SCtx(ctx)) => ctx.map(vd => VarData(vd,None,Position.Init))
              }).flatten
              doAssoc(n,args(i-1)._2,args,owner)(pc.copy(context = newcontext),htmlstate)
            } else {
              pc.out("<mi>TODO</mi>")
            }
          case _ if property == "stex:argmarker" && resource.endsWith("a") =>
            arg1(())
          case _ if property == "stex:argmarker" && resource.endsWith("b") =>
            arg2(())
          case _ if property == "stex:argmarker" =>
            val i = resource.head.toString.toInt
            if (args.isDefinedAt(i-1)) {
              val newcontext = pc.context ::: (args.take(i-1).collect {
                case (_,SCtx(ctx)) => ctx.map(vd => VarData(vd,None,Position.Init))
              }).flatten
              val arg = args(i - 1)
              recurse(arg._2.obj)(pc.copy(context = newcontext))
            } else {
              pc.out("<mi>TODO</mi>")
            }
          case n =>
            pc.out("<" + n.label +
              (if (n.classes.nonEmpty) " class=\"" + n.classes.mkString(" ") + "\"" else "") +
              n.attributes.toList.map{
                case ((ns,k),v) if ns == n.namespace => " " + k + "=\"" + v + "\""
                case ((ns,k),v) => " " + htmlstate.namespaces(ns) + ":" + k + "=\"" + v + "\""
              }.mkString +
              (if (n.children.isEmpty) "/>" else ">")
            )
            if (n.children.nonEmpty) {
              n.children.foreach(rec)
              pc.out("</" + n.label + ">")
            }
        }
      }
      _ => n.children.foreach(rec)
    }
    val f = arg match {
      case SCtx(ctx) if ctx.length > 1 =>
        val newcontext = pc.context ::: ctx.map(vd => VarData(vd,None,Position.Init))
        ctx.variables.init.init.foldRight(
          doPair(_ => recurse(ctx.variables.init.last)(pc.copy(context = newcontext)),_ => recurse(ctx.variables.last)(pc.copy(context = newcontext)))
        )((vd,f) =>
          doPair(_ => recurse(vd)(pc.copy(context = newcontext)),f)
        )
      case STerm(STeX.flatseq(ls)) if ls.length > 1 =>
        ls.init.init.foldRight(
          doPair(_ => recurse(ls.init.last),_ => recurse(ls.last))
        )((vd,f) =>
          doPair(_ => recurse(vd),f)
        )
      case STerm(OMV(n)) if pc.getContext.exists(v => v.name == n &&
        v.tp.exists(t => STeX.flatseq.tp.unapply(t).nonEmpty)
      ) => (_ : Unit) =>
        pc.out("<mrow><mi>" + owner + "</mi><mo>(</mo>")
        recurse(OMV(n))
        pc.out("<mo>)</mo></mrow>")
      case o => (_:Unit) => recurse(o.obj)
    }
    f(())
  }

  private def substitute(n : Node,args : List[(Char,SOMBArg)],owner:String)(implicit pc: PresentationContext) : Unit = {
    implicit val htmlstate : HTMLParser.ParsingState = new HTMLParser.ParsingState(controller,Nil)
    val top = HTMLParser(n.toString)
    def rec(n : HTMLNode) : Unit = {
      val property = n.attributes.getOrElse((n.namespace,"property"),"")
      val resource = n.attributes.getOrElse((n.namespace,"resource"),"")
      n match {
        case t : HTMLText => pc.out(t.text)
        case n if property == "stex:arg" && (resource.startsWith("a") || resource.startsWith("B")) =>
          val i = resource.last.toString.toInt
          if (args.isDefinedAt(i-1)) {
            val newcontext = pc.context ::: (args.take(i-1).collect {
              case (_,SCtx(ctx)) => ctx.map(vd => VarData(vd,None,Position.Init))
            }).flatten
            doAssoc(n,args(i-1)._2,args,owner)(pc.copy(context = newcontext ),htmlstate)
          } else {
            pc.out("<mi>TODO</mi>")
          }
        case _ if property == "stex:argmarker" =>
          val i = resource.head.toString.toInt
          if (args.isDefinedAt(i-1)) {
            val newcontext = pc.context ::: (args.take(i-1).collect {
              case (_,SCtx(ctx)) => ctx.map(vd => VarData(vd,None,Position.Init))
            }).flatten
            val arg = args(i - 1)
            recurse(arg._2.obj)(pc.copy(context = newcontext))
          } else {
            pc.out("<mi>TODO</mi>")
          }
        case n =>
          pc.out("<" + n.label +
            (if (n.classes.nonEmpty) " class=\"" + n.classes.mkString(" ") + "\"" else "") +
            n.attributes.toList.map{
              case ((ns,k),v) if ns == n.namespace => " " + k + "=\"" + v + "\""
              case ((ns,k),v) => " " + htmlstate.namespaces(ns) + ":" + k + "=\"" + v + "\""
            }.mkString +
            (if (n.children.isEmpty) "/>" else ">")
          )
          if (n.children.nonEmpty) {
            n.children.foreach(rec)
            pc.out("</" + n.label + ">")
          }
      }
    }
    rec(top)
  }

  override def recurse(obj: Obj, bracket: TextNotation => Int)(implicit pc: PresentationContext): Int = {
    lazy val default = {
      super.recurse(obj,bracket)
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
            pc.out("<mrow>")
            getMainNotation(vd.name)(pc.getContext ++ vd) match {
              case Some((_,a,b,Some(n))) =>
                substitute(n,Nil,n.toString)
              case Some((n,a,b,_)) =>
                substitute(n,Nil,n.toString)
              case None =>
                pc.out(<mi>{vd.name}</mi>.toString)
            }
            pc.out("<mo>:</mo>")
            recurse(tm)
            pc.out("</mrow>")
          case _ =>
            getMainNotation(vd.name)(pc.getContext ++ vd) match {
              case Some((_,a,b,Some(n))) =>
                substitute(n,Nil,n.toString)
              case Some((n,a,b,_)) =>
                substitute(n,Nil,n.toString)
              case None =>
                pc.out(<mi>{vd.name}</mi>.toString)
            }
        }
        0
      case STeX.informal(n) =>
        val node = HTMLParser(n.toString())(new ParsingState(controller,server.extensions.flatMap(_.rules)))
        node.attributes(("","mathbackground")) = "#ff0000"
        pc.out(node.toString)
        0
      case STeX.informal.op(label,args) =>
        pc.out("<" + label + " mathbackground=\"#ff0000\"" + ">")
        args.foreach(recurse(_))
        pc.out("</" + label + ">")
        0
      case SOMA(f,args) => recurse(OMA(f,args),bracket)
      case tm : Term =>
        val (is,ar,t) = implicits(tm)(pc.getContext)
        def ret() = t match {
          case STeX.symboldoc(ls,s,n) =>
            pc.out("<mtext>Documentation (" + s + ") for " + ls.mkString(",") + " :")
            pc.out(n.toString())
          case OMA(OMS(p),args) =>
            getMainNotation(p) match {
              case Some((_,n,a,b,_)) =>
                substitute(n,ar.zip(args.map(STerm)).toList,p.name.toString)
              case None =>
                pc.out("<mrow>")
                pc.out(<mi>{p.name}</mi>.toString)
                pc.out("<mo>(</mo>")
                args.foreach(recurse)
                pc.out("<mo>)</mo>")
                pc.out("</mrow>")
            }
          case OMA(OMV(n),args) =>
            getMainNotation(n)(pc.getContext) match {
              case Some((n,a,b,_)) =>
                substitute(n,ar.zip(args.map(STerm)).toList,n.toString)
              case None =>
                pc.out("<mrow>")
                pc.out(<mi>{n}</mi>.toString)
                pc.out("<mo>(</mo>")
                args.foreach(recurse)
                pc.out("<mo>)</mo>")
                pc.out("</mrow>")
            }
          case SOMB(OMS(p),args) =>
            getMainNotation(p) match {
              case Some((_,n,a,b,_)) =>
                substitute(n,ar.zip(args).toList,p.name.toString)
              case None =>
                pc.out("<mrow>")
                pc.out(<mi>{p.name}</mi>.toString)
                pc.out("<mo>(</mo>")
                args.foreach(a => recurse(a.obj))
                pc.out("<mo>)</mo>")
                pc.out("</mrow>")
            }
          case SOMB(OMV(n),args) =>
            getMainNotation(n)(pc.getContext) match {
              case Some((n,a,b,_)) =>
                substitute(n,ar.zip(args).toList,n.toString)
              case None =>
                pc.out("<mrow>")
                pc.out(<mi>{n}</mi>.toString)
                pc.out("<mo>(</mo>")
                args.foreach(a => recurse(a.obj))
                pc.out("<mo>)</mo>")
                pc.out("</mrow>")
            }
          case OMS(p) =>
            getMainNotation(p) match {
              case Some((_,_,a,b,Some(n))) =>
                substitute(n,Nil,p.name.toString)
              case Some((_,n,a,b,_)) =>
                substitute(n,Nil,p.name.toString)
              case None =>
                pc.out(<mi>{p.name}</mi>.toString)
            }
          case OMV(n) =>
            getMainNotation(n)(pc.getContext) match {
              case Some((_,a,b,Some(n))) =>
                substitute(n,Nil,n.toString)
              case Some((n,a,b,_)) =>
                substitute(n,Nil,n.toString)
              case None =>
                pc.out(<mi>{n}</mi>.toString)
            }
          case _ =>
            default
        }
        if (is.isEmpty) ret() else {
          pc.out("<munder><munder accentunder=\"true\"><mrow>")
          ret()
          pc.out("</mrow><mo>&#x23DF;</mo></munder><mrow>")
          recurse(is.head.obj)
          is.tail.foreach{a =>
            pc.out("<mo>,</mo>")
            recurse(a.obj)
          }
          pc.out("</mrow></munder>")
        }
        0
      case _ =>
        default
    }
  }
}

class STeXPresenterTex extends STeXPresenter {
  def recurse(obj: Obj)(implicit pc: PresentationContext): Unit = obj match {
    case c : Context if c.nonEmpty =>
      recurse(c.variables.head)(pc.copy(context = pc.context ::: c.map(vd => VarData(vd,None,Position.Init))))
      c.variables.tail.foreach { v =>
        pc.out(", ")
        recurse(v)(pc.copy(context = pc.context ::: c.map(vd => VarData(vd,None,Position.Init))))
      }
    case vd : VarDecl =>
      val macroname = OMDocHTML.getMacroName(vd)
      pc.out("\\" + macroname.getOrElse("svar{" + vd.name + "}"))

    case c : Context =>
    case SOMA(f,args) => recurse(OMA(f,args))
    case tm : Term =>
      val (_,ar,t) = implicits(tm)(pc.getContext)
      t match {
        case SOMB(f,arg) =>
          f match {
            case OMS(p) =>
              controller.getO(p).flatMap(OMDocHTML.getMacroName) match {
                case Some(s) => pc.out("\\" + s)
                case _ => pc.out("\\" + p)
              }
            case OMV(n) =>
              val macroname = pc.getContext.findLast(_.name == n).flatMap(OMDocHTML.getMacroName)
              pc.out("\\" + macroname.getOrElse("svar{" + n + "}"))
            case _ =>
              pc.out("(unclear)")
          }
          val nctx = arg.collect{case SCtx(ctx) => ctx}.flatten
          ar.zip(arg).foreach {
            case ('a'|'B',STeX.flatseq(a :: ls)) =>
              pc.out("{")
              recurse(a)(pc.copy(context = pc.context ::: nctx.map(vd => VarData(vd,None,Position.Init))))
              ls.foreach {t =>
                pc.out(", ")
                recurse(t)(pc.copy(context = pc.context ::: nctx.map(vd => VarData(vd,None,Position.Init))))
              }
              pc.out("}")
            case ('a'|'B',STeX.flatseq(List(a))) =>
              pc.out("{")
              recurse(a)(pc.copy(context = pc.context ::: nctx.map(vd => VarData(vd,None,Position.Init))))
              pc.out("}")
            case (_,a) =>
              pc.out("{")
              recurse(a.obj)(pc.copy(context = pc.context ::: nctx.map(vd => VarData(vd,None,Position.Init))))
              pc.out("}")
          }
        case OMA(f,arg) =>
          f match {
            case OMS(p) =>
              controller.getO(p).flatMap(OMDocHTML.getMacroName) match {
                case Some(s) => pc.out("\\" + s)
                case _ => pc.out("\\" + p)
              }
            case OMV(n) =>
              val macroname = pc.getContext.findLast(_.name == n).flatMap(OMDocHTML.getMacroName)
              pc.out("\\" + macroname.getOrElse("svar{" + n + "}"))
            case _ =>
              pc.out("(unclear)")
          }
          ar.zip(arg).foreach {
            case ('a'|'B',STeX.flatseq(a :: ls)) =>
              pc.out("{")
              recurse(a)
              ls.foreach {t =>
                pc.out(", ")
                recurse(t)
              }
              pc.out("}")
            case ('a'|'B',STeX.flatseq(List(a))) =>
              pc.out("{")
              recurse(a)
              pc.out("}")
            case (_,a) =>
              pc.out("{")
              recurse(a)
              pc.out("}")
          }
        case OMV(n) =>
          val macroname = pc.getContext.findLast(_.name == n).flatMap(OMDocHTML.getMacroName)
          pc.out("\\" + macroname.getOrElse("svar{" + n + "}"))
        case OMS(p) =>
          controller.getO(p).flatMap(OMDocHTML.getMacroName) match {
            case Some(s) => pc.out("\\" + s)
            case _ => pc.out("\\" + p.toString)
          }
        case _ =>
          pc.out("(unclear)")
      }
    case _ =>
      pc.out("(unclear)")
  }

  override def apply(o: Obj, origin: Option[CPath])(implicit rh: RenderingHandler): Unit = {
    rh.apply("<pre>")
    val pc = new PresentationContext(rh,origin,Nil,None,Position.Init,Context.empty,Nil,None)
    recurse(o)(pc)
    rh.apply("</pre>")
  }
}