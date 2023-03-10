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
import info.kwarc.mmt.stex.rules.{Getfield}

import scala.xml.{NodeSeq, XML}

import scala.xml.{Elem, Node}

trait STeXPresenter extends ObjectPresenter {
  lazy val server = controller.extman.get(classOf[STeXServer]).head

}

class STeXPresenterML extends InformalMathMLPresenter with STeXPresenter {
  override def asXML(o: Obj, origin: Option[CPath]): Node = {
    val s = asString(o, origin)
    try {
      scala.xml.XML.loadString(s.replace("&","&amp;"))
    } catch {
      case t: Throwable =>
        print("")
        throw t
    }
  }

  private def recurseI(obj:Obj)(implicit pc: PresentationContext): NodeSeq = {
    val sb = new presentation.StringBuilder
    recurse(obj)(pc.copy(rh=sb))
    XML.loadString(s"<mrow>${sb.get}</mrow>".replace("&","&amp;")).head.child
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
    var rett = t match {
      case SHTML.implicit_binder.spine(ctx, tm) =>
        OMBIND(OMS(SHTML.implicit_binder.path), ctx, tm)
      case _ =>
        t
    }
    val (fun,ruler,headO,isvar,tis,args) = rett match {
      case Getfield(SHTML.of_type(ti,_),_) =>
        (rett,server.getRuler(t, pc.getContext),None,false,Some(ti),Nil)
      case Getfield(ti, _) =>
        (rett, server.getRuler(t, pc.getContext), None, false, Some(ti), Nil)
      case SHTMLHoas.Omb(_,it@Getfield(SHTML.of_type(ti,_),_),args) =>
        (it,server.getRuler(it, pc.getContext), None, false, Some(ti), args)
      case SHTMLHoas.Omb(_, it@Getfield(ti, _), args) =>
        (it, server.getRuler(it, pc.getContext), None, false, Some(ti), args)
      case SHTMLHoas.OmaSpine(_,it@Getfield(SHTML.of_type(ti,_),_),args) =>
        (it,server.getRuler(it, pc.getContext), None, false, Some(ti), args.map(STerm))
      case SHTMLHoas.OmaSpine(_, it@Getfield(ti, _), args) =>
        (it, server.getRuler(it, pc.getContext), None, false, Some(ti), args.map(STerm))
      case SHTMLHoas.Omb(_, f@OMV(_), args) =>
        (f,server.getRuler(f, pc.getContext), None, true, None, args)
      case SHTMLHoas.Omb(_, f@OMS(p), args) =>
        (f,server.getRuler(f, pc.getContext), controller.getO(p), false, None, args)
      case SHTMLHoas.Omb(_, f, args) =>
        (f,server.getRuler(f, pc.getContext), None, false, None, args)
      case SHTMLHoas.OmaSpine(_, f@OMV(_), args) =>
        (f,server.getRuler(f, pc.getContext), None, true, None, args.map(STerm))
      case SHTMLHoas.OmaSpine(_, f@OMS(p), args) =>
        (f,server.getRuler(f, pc.getContext), controller.getO(p), false, None, args.map(STerm))
      case SHTMLHoas.OmaSpine(_, f, args) =>
        (f,server.getRuler(f, pc.getContext), None, false, None, args.map(STerm))
      case OMV(_) =>
        (rett,server.getRuler(rett,pc.getContext),None,true,None,Nil)
      case OMS(p) =>
        (rett,server.getRuler(rett, pc.getContext), controller.getO(p), false, None, Nil)
      case _ =>
        (rett,server.getRuler(rett, pc.getContext),None,false,None,Nil)
    }
    /*val (ruler,head,isvar) = {
      (t match {
        case Getfield(_,_) => Some(t)
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
    }*/
    val nots = headO.toList.flatMap(server.getNotations) ::: ruler.toList.flatMap(server.getNotations)

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
    var retargs = args

    ruler match {
      case Some(o) =>
        (rett,server.getAssoctype(o),server.getArity(o)) match {
          case (SHTMLHoas.Omb(h,f,args),Some("pre"),_) =>
            retargs = undoPre(h,f,args)
            rett = h.HOMB(f, retargs)
          case (SHTMLHoas.OmaSpine(h,f,ls),Some("bin"|"binr"|"conj"),Some("a")) =>
            val (pre,args) = ls.splitAt(implnum)
            val nargs = pre ::: SHTML.flatseq(undoBin(h,f,pre,args,Nil)) :: Nil
            retargs = nargs.map(STerm)
            rett = SHTMLHoas.OmaSpine(h,f,nargs)
          case (SHTMLHoas.OmaSpine(h, f, ls), Some("bin" | "binr" | "conj"), Some("ai")) =>
            val (pre, args) = ls.splitAt(implnum)
            val nargs = pre ::: SHTML.flatseq(undoBin(h, f, pre, args.init, List(args.last))) :: args.last :: Nil
            retargs = nargs.map(STerm)
            rett = SHTMLHoas.OmaSpine(h, f, nargs)
          case (SHTMLHoas.OmaSpine(h, f, ls), Some("bin" | "binr" | "conj"), Some(a)) =>
            val nargs = undoBin(h, f, Nil, ls, Nil)
            retargs = nargs.map(STerm)
            rett = SHTMLHoas.OmaSpine(h, f, nargs)
          case _ =>
        }
      case _ =>
    }

    retargs = retargs.drop(implnum)
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

    ruler.flatMap(server.getReorder) match {
      case Some(ls) if retargs.length == ls.length =>
        retargs = ls.map(i => retargs(i-1))
      case _ =>
    }

    (ruler,impls,nots,rett,isvar,fun,tis,retargs)
  }

  private def present(n : Node) = server.present(n.toString(),true)(None)
  override def recurse(obj: Obj, bracket: TextNotation => Int)(implicit pc: PresentationContext): Int = {
    def default(implicit pc: PresentationContext) = {
      obj match {
        case SHTML.informal(n) =>
          val node = server.present(n.toString())(None)
          node.plain.attributes((node.namespace, "mathbackground")) = "#ff0000"
          pc.out(node.toString)
          0
        case SHTML.informal.op(label, args) =>
          pc.out("<" + label + " mathbackground=\"#ff0000\"" + ">")
          args.foreach(recurse(_))
          pc.out("</" + label + ">")
          0
        case OMS(p) => pc.out(
          s"""<mrow shtml:term="OMID" shtml:head="${p.toString}" shtml:maincomp="${p.toString}">
            |  <mi mathvariant="normal">${p.name}</mi>
            |</mrow>""".stripMargin
        )
        0
        case t@OMV(p) =>
          pc.out(server.getRuler(t,pc.getContext) match {
            case Some(c : Constant) =>
              s"""<mrow shtml:term="OMV" shtml:head="${p.toString}" shtml:varcomp="${p.toString}" mmt:variable="${c.path}">
              |  <mi>${p.toString}</mi>
              |</mrow>""".stripMargin
            case _ =>
              s"""<mrow shtml:term="OMV" shtml:head="${p.toString}" shtml:varcomp="${p.toString}">
              |  <mi>${p.toString}</mi>
              |</mrow>""".stripMargin
          })
          0
        case _ =>
          super.recurse(obj, bracket)
      }
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
        val (ruler,is,notations,t,isvar,head,tis,args) = normalizeTerm(tm)
        lazy val arity = ruler.flatMap(server.getArity).getOrElse("")
        lazy val ntis = tis.map{t => recurseI(t)}
        lazy val nctx = pc.context ::: args.flatMap { case SCtx(ctx) => ctx case _ => Context.empty }
          .map(VarData(_, None, pc.pos))
        def ret() = if (arity.length == args.length) {
          notations match {
            case not :: _ =>
              val nargs = arity.zip(args).zipWithIndex.map {
                case (('a', STerm(SHTML.flatseq(ls))), p) =>
                  with_precedence(not.argprecs(p)) {
                    ls.map {
                      recurseI(_)(pc.copy(context = nctx))
                    }
                  }
                case (('a', STerm(a)), p) =>
                  with_precedence(not.argprecs(p)) {
                    List(recurseI(a)(pc.copy(context = nctx)))
                  }
                case (('i', STerm(tm)), p) =>
                  with_precedence(not.argprecs(p)) {
                    List(recurseI(tm)(pc.copy(context = nctx)))
                  }
                case (('b', SCtx(Context(v))), p) =>
                  with_precedence(not.argprecs(p)) {
                    List(recurseI(v)(pc.copy(context = nctx)))
                  }
                case (('B', SCtx(ctx)), p) =>
                  with_precedence(not.argprecs(p)) {
                    ctx.map {
                      recurseI(_)(pc.copy(context = nctx))
                    }
                  }
              }
              pc.out(present(not.present(nargs.toList, downwards_precedence, isvar,ntis)).toString())
            case _ => default(pc.copy(context = nctx))
          }
        } else if (args.isEmpty) {
          notations.find(_.op.isDefined) match {
            case Some(not) =>
              // TODO parentheses
              pc.out(present(not.present(Nil, isvar = isvar,tis=ntis)).toString())
            case _ => default
          }
        } else default

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