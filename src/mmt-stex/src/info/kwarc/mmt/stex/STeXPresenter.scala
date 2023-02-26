package info.kwarc.mmt.stex

import info.kwarc.mmt.api.checking.History
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.notations.TextNotation
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.{ObjectPresenter, PresentationContext, RenderingHandler, VarData}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, Structure}
import info.kwarc.mmt.api.{CPath, ComplexStep, ContentPath, GetError, GlobalName, LocalName, RuleSet, StructuralElement, presentation}
import info.kwarc.mmt.stex
import info.kwarc.mmt.stex.Extensions.NotationExtractor
import info.kwarc.mmt.stex.rules.{Getfield, ModelsOf, ModuleType, IntLiterals, RulerRule}
import info.kwarc.mmt.stex.xhtml.HTMLParser
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState

import scala.io.Source
import scala.xml.{NodeSeq, XML}
//import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, HTMLText, ParsingState}
//import info.kwarc.mmt.stex.xhtml.{HTMLParser, OMDocHTML}

import scala.xml.{Elem, Node}

//case class STeXNotation(tm : Term, head : ContentPath, macroname : String, notation_used : HTMLNode, fragment: String, arity : String, allnotations : List[(String,String,String,HTMLNode)])

trait STeXPresenter extends ObjectPresenter {
  lazy val server = controller.extman.get(classOf[STeXServer]).head

/*
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

 */

}

class STeXPresenterML extends InformalMathMLPresenter with STeXPresenter {

  private def recurseI(obj:Obj)(implicit pc: PresentationContext): NodeSeq = {
    val sb = new presentation.StringBuilder
    recurse(obj)(pc.copy(rh=sb))
    XML.loadString(s"<mrow>${sb.get}</mrow>").head.child
  }

  private def undoPre(h :  SHTMLHoas.HoasRule,f : Term,args:List[SOMBArg]) : List[SOMBArg] = args match {
    case SCtx(Context(vd)) :: STerm(SHTMLHoas.Omb(`h`,`f`,iargs)) :: Nil =>
      val ret = undoPre(h,f,iargs)
      ret.headOption match {
        case Some(SCtx(ctx)) =>
          SCtx(Context(vd) ++ ctx) :: ret.tail
        case _ => args
      }
    case STerm(t) :: SCtx(Context(vd)) :: STerm(SHTMLHoas.Omb(`h`, `f`, iargs)) :: Nil =>
      val ret = undoPre(h, f, iargs)
      ret.headOption match {
        case Some(STerm(`t`)) =>
          ret.tail.headOption match {
            case Some(SCtx(ctx)) =>
              STerm(t) :: SCtx(Context(vd) ++ ctx) :: ret.tail.tail
            case _ => args
          }
        case _ => args
      }
    case _ => args
  }
  private def undoBin(h : Option[SHTMLHoas.HoasRule],f : Term,pre:List[Term],ls:List[Term],post:List[Term]): List[Term] = ls match {
    case List(SHTMLHoas.OmaSpine(`h`,`f`,ls),b) if ls.startsWith(pre) && ls.endsWith(post) =>
      undoBin(h,f,pre,ls.drop(pre.length).dropRight(post.length),post) ::: b :: Nil
    case List(SHTMLHoas.OmaSpine(`h`,`f`,ls)) if ls.startsWith(pre) && ls.endsWith(post) =>
      undoBin(h,f,pre,ls.drop(pre.length).dropRight(post.length),post)
    case _ =>
      ls
  }

  var downwards_precedence = 2147483647
  def with_precedence[A](i:Int)(f : => A): A = {
    val oldprec = downwards_precedence
    try {
      downwards_precedence = i
      f
    } finally {
      downwards_precedence = oldprec
    }
  }
  private def normalizeTerm(t : Term)(implicit pc: PresentationContext) = {
    val (ruler,head,isvar) = {
      (t match {
        case SHTMLHoas.Omb(_, f, _) =>
          Some(f)
        case SHTMLHoas.OmaSpine(_, f, _) =>
          Some(f)
        case OMBIND(f, _, _) =>
          Some(f)
        case OMS(c) => Some(OMS(c))
        case OMV(x) => Some(OMV(x))
        case _ =>
          None
      }) match {
        case None =>
          (None,None,false)
        case Some(t@OMS(p)) =>
          (server.getRuler(t, pc.getContext),controller.getO(p),false)
        case Some(t@OMV(p)) =>
          (server.getRuler(t, pc.getContext), None,true)
        case Some(t) =>
          (server.getRuler(t, pc.getContext), None,false)
      }
    }
    val nots = head.toList.flatMap(server.getNotations) ::: ruler.toList.flatMap(server.getNotations)
    var rett = t match {
      case SHTML.implicit_binder.spine(ctx,tm) =>
        OMBIND(OMS(SHTML.implicit_binder.path),ctx,tm)
      case _ =>
        t
    }

    def implicitsFromTerm(tm: Term) = tm match {
      case SHTML.implicit_binder.spine(ctx, _) => ctx.length
      case _ => 0
    }

    val implnum = ruler match {
      case Some(c: Constant) if c.tp.isDefined =>
        implicitsFromTerm(c.tp.get)
      case Some(vd: VarDecl) if vd.tp.isDefined =>
        implicitsFromTerm(vd.tp.get)
      case _ =>
        0
    }

    ruler match {
      case Some(o) =>
        (rett,server.getAssoctype(o),server.getArity(o)) match {
          case (SHTMLHoas.Omb(h,f,args),Some("pre"),_) =>
            rett = h.HOMB(f,undoPre(h,f,args))
            rett = h.HOMB(f, undoPre(h, f, args))
          case (SHTMLHoas.OmaSpine(h,f,ls),Some("bin"|"binr"|"conj"),Some("a")) =>
            val (pre,args) = ls.splitAt(implnum)
            rett = SHTMLHoas.OmaSpine(h,f,pre ::: SHTML.flatseq(undoBin(h,f,pre,args,Nil)) :: Nil)
          case (SHTMLHoas.OmaSpine(h, f, ls), Some("bin" | "binr" | "conj"), Some("ai")) =>
            val (pre, args) = ls.splitAt(implnum)
            rett = SHTMLHoas.OmaSpine(h, f, pre ::: SHTML.flatseq(undoBin(h, f, pre, args.init, List(args.last))) :: args.last :: Nil)
          case (SHTMLHoas.OmaSpine(h, f, ls), Some("bin" | "binr" | "conj"), Some(a)) =>
            println(a)
            rett = SHTMLHoas.OmaSpine(h, f, undoBin(h, f, Nil, ls, Nil))
          case _ =>
        }
      case _ =>
    }

    val impls = if (implnum == 0) Nil else {rett match {
      case SHTMLHoas.OmaSpine(h, OMS(p), args) =>
        val is = args.take(implnum)
        rett = SHTMLHoas.OmaSpine(h, OMS(p), args.drop(implnum))
        is
      case SHTMLHoas.Omb(h, OMS(p), args) =>
        val is = args.take(implnum).map(_.asInstanceOf[STerm].tm)
        rett = h.HOMB(OMS(p), args.drop(implnum))
        is
      case _ => Nil
    }}

    (ruler,impls,nots,rett,isvar)
  }

  private def present(n : Node) = server.present(n.toString(),true)(None)
  override def recurse(obj: Obj, bracket: TextNotation => Int)(implicit pc: PresentationContext): Int = {
    def default(implicit pc: PresentationContext) = {
      super.recurse(obj, bracket)
    }
    obj match {
      case ctx: Context =>
        pc.out("<mrow><mo stretchy=\"true\">{</mo>")
        var vars = ctx.variables
        try {
          while (vars.nonEmpty) {
            recurse(vars.head)
            vars = vars.tail
            if (vars.nonEmpty) pc.out("<mo>,</mo>")
          }
        } finally {
          pc.out("<mo stretchy=\"true\">}</mo></mrow>")
        }
        0
      case vd : VarDecl =>
        val notations = server.getNotations(vd)
        val arity = server.getArity(vd)
        pc.out("<mrow>")
        arity match {
          case Some("") | None =>
            notations match {
              // TODO get notation id
              case not :: _ =>
                pc.out(present(not.present(Nil,isvar=true)).toString())
              case Nil => pc.out("<mi>" + vd.name + "</mi>")
            }
          case _ =>
            // TODO get notation id
            notations.find(_.op.isDefined) match {
              case Some(not) =>
                pc.out(present(not.present(Nil, isvar = true)).toString())
              case _ => pc.out("<mi>" + vd.name + "</mi>")
            }
        }
        vd.tp.foreach{tp =>
          pc.out("<mo>:</mo>")
          recurse(tp)
        }
        vd.df.foreach { t =>
          pc.out("<mo>:=</mo>")
          recurse(t)
        }
        pc.out("</mrow>")
        0
      case tm: Term =>
        val (ruler,is,notations,t,isvar) = normalizeTerm(tm)

        def ret() = t match {
          case SHTML.informal(n) =>
            val node = server.present(n.toString())(None)
            node.plain.attributes((node.namespace, "mathbackground")) = "#ff0000"
            pc.out(node.toString)
          case SHTML.informal.op(label, args) =>
            pc.out("<" + label + " mathbackground=\"#ff0000\"" + ">")
            args.foreach(recurse(_))
            pc.out("</" + label + ">")
          case SHTMLHoas.Omb(_, f, args) =>
            // TODO attach f somewhere?
            val arity = ruler.flatMap(server.getArity).getOrElse("")
            val nctx = pc.context ::: args.flatMap{case SCtx(ctx) => ctx case _ => Context.empty}
              .map(VarData(_,None,pc.pos))
            notations match {
              // TODO get notation id
              case not :: _ =>
                val nargs = arity.zip(args).zipWithIndex.map {
                  case (('a', STerm(SHTML.flatseq(ls))),p) =>
                    with_precedence(not.argprecs(p)) {
                      ls.map {
                        recurseI(_)(pc.copy(context = nctx))
                      }
                    }
                  case (('i', STerm(tm)),p) =>
                    with_precedence(not.argprecs(p)) {
                      List(recurseI(tm)(pc.copy(context = nctx)))
                    }
                  case (('b', SCtx(Context(v))),p) =>
                    with_precedence(not.argprecs(p)) {
                      List(recurseI(v)(pc.copy(context = nctx)))
                    }
                  case (('B', SCtx(ctx)),p) =>
                    with_precedence(not.argprecs(p)) {
                      ctx.map {
                        recurseI(_)(pc.copy(context = nctx))
                      }
                    }
                }
                pc.out(present(not.present(nargs.toList,downwards_precedence,isvar)).toString())
              case _ => default(pc.copy(context = nctx))
            }
          case OMBIND(f, ctx, bd) =>
            // TODO attach f somewhere?
            val args = List(SCtx(ctx),STerm(bd))
            val arity = ruler.flatMap(server.getArity).getOrElse("")
            val nctx = pc.context ::: ctx.map(VarData(_, None, pc.pos))
            notations match {
              // TODO get notation id
              case not :: _ =>
                val nargs = arity.zip(args).zipWithIndex.map {
                  case (('a', STerm(SHTML.flatseq(ls))),p) =>
                    with_precedence(not.argprecs(p)) {
                      ls.map {
                        recurseI(_)(pc.copy(context = nctx))
                      }
                    }
                  case (('i', STerm(tm)),p) =>
                    with_precedence(not.argprecs(p)) {
                      List(recurseI(tm)(pc.copy(context = nctx)))
                    }
                  case (('b', SCtx(Context(v))),p) =>
                    with_precedence(not.argprecs(p)) {
                      List(recurseI(v)(pc.copy(context = nctx)))
                    }
                  case (('B', SCtx(ctx)),p) =>
                    with_precedence(not.argprecs(p)) {
                      ctx.map {
                        recurseI(_)(pc.copy(context = nctx))
                      }
                    }
                }
                pc.out(present(not.present(nargs.toList,downwards_precedence,isvar)).toString())
              case _ => default(pc.copy(context = nctx))
            }
          case SHTMLHoas.OmaSpine(_, f, args) =>
            // TODO attach f somewhere?
            val arity = ruler.flatMap(server.getArity).getOrElse("")
            notations match {
              // TODO get notation id
              case not :: _ =>
                val nargs = arity.zip(args).zipWithIndex.map {
                  case (('a', SHTML.flatseq(ls)),p) =>
                    with_precedence(not.argprecs(p)) {
                      ls.map {
                        recurseI(_)
                      }
                    }
                  case (('a', tm),p) =>
                    // TODO
                    with_precedence(not.argprecs(p)) {
                      List(recurseI(tm))
                    }
                  case (('i', tm),p) =>
                    with_precedence(not.argprecs(p)) {List(recurseI(tm))}
                }
                pc.out(present(not.present(nargs.toList,downwards_precedence,isvar)).toString())
              case _ => default
            }
          case OMS(_)|OMV(_) =>
            val ruler = server.getRuler(t,pc.getContext)
            val arity = ruler.flatMap(server.getArity)
            arity match {
              case Some("") | None =>
                notations match {
                  // TODO get notation id
                  case not :: _ =>
                    // TODO parentheses
                    pc.out(present(not.present(Nil,isvar=isvar)).toString())
                  case Nil => default
                }
              case _ =>
                // TODO get notation id
                notations.find(_.op.isDefined) match {
                  case Some(not) =>
                    // TODO parentheses
                    pc.out(present(not.present(Nil,isvar=isvar)).toString())
                  case _ => default
                }
            }
          case _ =>
            default
        }
        object subs extends Traverser[Term] {
          object Done extends Throwable

          def traverse(tm: Term)(implicit con: Context, state: Term): Term = tm match {
            case `state` => throw Done
            case _ => Traverser(this, tm)(con,state)
          }
          def contains(itm : Term) : Boolean = try {
            traverse(t)(Context.empty,itm)
            false
          } catch {
            case Done => true
          }

        }
        if (is.isEmpty) ret()
        else if (is.forall(i => subs contains i)) ret()
        else {
          pc.out("<munder><munder><mrow>")
          ret()
          pc.out("</mrow><mo>⏟</mo></munder><mrow>")
          var implicits = is
          while (implicits.nonEmpty) {
            val h :: t = implicits
            implicits = t
            recurse(h)
            if (implicits.nonEmpty) pc.out("<mo>,</mo>")
          }
          pc.out("</mrow></munder>")
        }
        0
      case _ =>
        default
    }
  }

  private var currentDown = -1000000
  private def withDown[A](prec : Int)(f : => A) = {
    val oldDown = currentDown
    currentDown = prec
    val ret : A = f
    currentDown = oldDown
    ret
  }
  /*
  private def doAssoc(n : HTMLNode,arg : SOMBArg, args:List[(Char,SOMBArg)],owner:String)(implicit pc: PresentationContext,htmlstate : HTMLParser.ParsingState) : Unit = {
    def doPair(arg1: Unit => Unit, arg2: Unit => Unit) : Unit => Unit = {
      def rec(n : HTMLNode) : Unit = {
        val property = n.attributes.getOrElse((n.namespace,"property"),"")
        val resource = n.attributes.getOrElse((n.namespace,"resource"),"")
        n match {
          case t : HTMLText => pc.out(t.text)
          case n if property == "shtml:arg" && (resource.startsWith("a") || resource.startsWith("B")) =>
            val i = resource.last.toString.toInt
            if (args.isDefinedAt(i-1)) {
              val newcontext = pc.context ::: (args.take(i-1).collect {
                case (_,SCtx(ctx)) => ctx.map(vd => VarData(vd,None,Position.Init))
              }).flatten
              doAssoc(n,args(i-1)._2,args,owner)(pc.copy(context = newcontext),htmlstate)
            } else {
              pc.out("<mi>TODO</mi>")
            }
          case _ if property == "shtml:argmarker" && resource.endsWith("a") =>
            arg1(())
          case _ if property == "shtml:argmarker" && resource.endsWith("b") =>
            arg2(())
          case _ if property == "shtml:argmarker" =>
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
        case n if property == "shtml:arg" && (resource.startsWith("a") || resource.startsWith("B")) =>
          val i = resource.last.toString.toInt
          if (args.isDefinedAt(i-1)) {
            val newcontext = pc.context ::: (args.take(i-1).collect {
              case (_,SCtx(ctx)) => ctx.map(vd => VarData(vd,None,Position.Init))
            }).flatten
            doAssoc(n,args(i-1)._2,args,owner)(pc.copy(context = newcontext ),htmlstate)
          } else {
            pc.out("<mi>TODO</mi>")
          }
        case _ if property == "shtml:argmarker" =>
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

   */
}

class STeXPresenterTex extends STeXPresenter {
  override def apply(o: Obj, origin: Option[CPath])(implicit rh: RenderingHandler): Unit = ???
  /*
  def recurse(obj: Obj)(implicit pc: PresentationContext): Unit = obj match {

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
  } */
}