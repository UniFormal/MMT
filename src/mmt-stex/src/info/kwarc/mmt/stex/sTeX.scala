package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.History
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.notations.{HOAS, HOASNotation, NestedHOASNotation}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, Structure}
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardDouble, StandardInt, StandardNat, StandardPositive, StandardRat, StandardString}
import info.kwarc.mmt.api.utils.{URI, XMLEscaping}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX
import info.kwarc.mmt.sequences.Sequences
import info.kwarc.mmt.stex.Extensions.NotationExtractor
import info.kwarc.mmt.stex.rules.{Getfield, ModelsOf, ModuleType, RecordError, StringLiterals}
import info.kwarc.mmt.stex.xhtml.HTMLParser
import info.kwarc.mmt.stex.xhtml.HTMLParser.ParsingState

import scala.xml._
import objects._

sealed abstract class SOMBArg {
  def obj:Obj
}
case class STerm(tm : Term) extends SOMBArg {
  val obj = tm
}
case class SCtx(ctx : Context) extends SOMBArg {
  val obj = ctx
}

object SOMBArg {
  implicit def toSOMB(ctx: Context) = SCtx(ctx)
  implicit def toSOMB(tm : Term) = STerm(tm)
}

import SOMBArg._

/*
object SOMB {

  object bound {
    def apply(binder:Term,ctx: Context, body: Term) = SOMB(binder, ctx, body)

    def apply(binder:Term,ln: LocalName, tp: Option[Term], body: Term) = SOMB(binder, Context(tp match {
      case Some(t) => OMV(ln) % t
      case None => VarDecl(ln)
    }), body)

    def unapply(tm: Term) = tm match {
      case SOMB(tt, List(SCtx(Context(vd, rest@_*)), STerm(bd))) =>
        if (rest.isEmpty) Some(tt,vd.name, vd.tp, bd) else Some(tt,vd.name, vd.tp, apply(tt,Context(rest: _*), bd))
      case _ => None
    }
  }
  def apply(f : Term,scopes:SOMBArg*) : OMBINDC = {
    val ctx = Context(scopes.flatMap{
      case SCtx(ctx) => ctx.variables
      case _ => Nil
    }:_*)
    OMBIND(OMS(SHTML.apply),ctx,OMA(f,scopes.toList.map{
      case STerm(tm) => tm
      case SCtx(ctx) => OMA(OMS(SHTML.meta_quantification),ctx.map(vd => OMV(vd.name)))
    }))
  }
  def unapply(tm : Term) = tm match {
    case OMBIND(OMS(SHTML.apply),ctx,OMA(f,scopes)) =>
      var nctx = Context.empty
      val ret = (f,scopes.map {
        case OMA(OMS(SHTML.meta_quantification),args : List[OMV@unchecked]) =>
          SCtx(args.map(v => {val vd = ctx(v.name); nctx = nctx ++ vd; vd} ))
        case t => STerm(t)
      })
      if (ctx.forall(nctx.contains)) Some(ret) else None
    case _ => None
  }
}

object SOMA {
  def apply(f : Term,args : Term*) : Term = args.toList match {
    case Nil => f
    case ls => OMA(OMS(SHTML.apply),f :: ls)
    //case List(a) => OMA(OMS(STeX.apply),List(f,a))
    //case h :: tail => apply(OMA(OMS(STeX.apply),List(f,h)),tail:_*)
  }
  def unapply(tm : Term) : Option[(Term,List[Term])] = tm match {
    /*case OMA(OMS(STeX.apply),(it@OMA(OMS(STeX.apply),_ :: _)) :: ls) =>
      unapply(it).map { case (h,as) =>
        (h,as ::: ls)
      }*/
    case OMA(OMS(SHTML.apply),f :: ls) => Some((f,ls))
    case _ => None
  }
}

 */

object SHTMLHoas {
  val sym = SHTML.mmtmeta_path ? "hoas"

  case class HoasRule(app : GlobalName, lambda : GlobalName, pi: GlobalName) extends Rule {
    def apply[A <: HasMetaData](o : A): A = {
      o.metadata.update(sym,OMA(OMS(sym),List(OMS(app),OMS(lambda),OMS(pi))))
      o
    }
    private val self = this
    def has(o: HasMetaData) = o.metadata.getValues(sym).contains(
      OMA(OMS(sym),List(OMS(app),OMS(lambda),OMS(pi)))
    )
    object HOMA {
      def apply(f: Term, args: List[Term]) = args match {
        case Nil => f
        case ls =>
          args.tail.foldLeft(
            self.apply(OMA(OMS(app),List(f,ls.head)))
          )((f,a) => self.apply(OMA(OMS(app),List(f,a))))
      }

      def unapply(tm: Term) = tm match {
        case OMA(OMS(`app`), f :: a :: Nil) if self.has(tm) => Some((f, a))
        case _ => None
      }
    }
    object HOMB {
      def apply(f : Term, scopes: List[SOMBArg]) = {
        val ctx = Context(scopes.flatMap {
          case SCtx(ctx) => ctx.variables
          case _ => Nil
        }: _*)
        self.apply(
          OMBIND(OMS(app), ctx, OMA(f, scopes.map {
            case STerm(tm) => tm
            case SCtx(ctx) => OMA(OMS(SHTML.meta_quantification), ctx.map(vd => OMV(vd.name)))
          }))
        )
      }

      def unapply(tm : Term): Option[(Term,List[SOMBArg])] = tm match {
        case OMBIND(OMS(`app`),ctx,OMA(f,scopes)) if self.has(tm) =>
          var nctx = Context.empty
          val ret = (f, scopes.map {
            case OMA(OMS(SHTML.meta_quantification), args: List[OMV@unchecked]) =>
              SCtx(args.map(v => {
                val vd = ctx(v.name);
                nctx = nctx ++ vd;
                vd
              }))
            case t => STerm(t)
          })
          if (ctx.forall(nctx.contains)) Some(ret) else None
        case _ => None
      }
    }
  }

  def get(o: HasMetaData): Option[HoasRule] = o.metadata.getValues(sym).headOption match {
    case Some(OMA(OMS(`sym`), List(OMS(app), OMS(lambda), OMS(pi)))) => Some(HoasRule(app, lambda, pi))
    case _ => None // should be unreachable
  }
  private def getHoas(tm : Term) = tm.metadata.getValues(sym).headOption match {
    case Some(OMA(OMS(`sym`),List(OMS(app),OMS(lambda),OMS(pi)))) => Some(HoasRule(app,lambda,pi))
    case _ => None // should be unreachable
  }
  object Oma {
    def unapply(tm: Term): Option[(HoasRule, Term, Term)] = tm match {
      case t@OMA(_,_) =>
        getHoas(t).flatMap(h => h.HOMA.unapply(t).map{case (t,ls) => (h,t,ls)})
      case _ => None
    }
  }
  object OmaSpine {
    def apply(h : Option[HoasRule],f : Term,args : List[Term]) = h match {
      case Some(h) => h.HOMA(f,args)
      case _ => OMA(f,args)
    }
    def unapply(tm : Term) : Option[(Option[HoasRule], Term, List[Term])] = tm match {
      case OMA(OMS(_),List(f,a)) if getHoas(tm).isDefined =>
        (getHoas(tm),unapply(f)) match {
          case (Some(h),Some((Some(hr),nf,na))) if h == hr =>
            Some((Some(h),nf,na ::: a :: Nil))
          case (Some(h),None) =>
            h.HOMA.unapply(tm).map { case (t, a) => (Some(h), t, List(a)) }
          case _ =>
            None
        }
      case OMA(f,List(a)) if getHoas(tm).isEmpty =>
        unapply(f) match {
          case Some((None,nf,na)) => Some((None,nf,na ::: a :: Nil))
          case None => Some((None,f,a :: Nil))
        }
      case _ => None
    }
  }

  object Omb {
    def apply(hoas:HoasRule, binder: Term, ctx: Context, body: Term) = hoas.HOMB(binder, SCtx(ctx) :: STerm(body) :: Nil)

    def apply(hoas:HoasRule, binder: Term, ln: LocalName, tp: Option[Term], body: Term) = hoas.HOMB(binder,SCtx(tp match {
        case Some(t) => VarDecl(ln,t)
        case _ => VarDecl(ln)
      }) :: STerm(body) :: Nil)

    def unapply(tm: Term): Option[(HoasRule,Term,List[SOMBArg])] = tm match {
      case OMBIND(OMS(_), _, OMA(_, _)) =>
        getHoas(tm) match {
          case Some(hoas) => hoas.HOMB.unapply(tm) match {
            case Some((t,ls)) => Some((hoas,t,ls))
            case _ => None
          }
          case _ => None
        }
      case _ => None
    }
  }

  object bound {

    def unapply(tm: Term) = tm match {
      case Omb(h,f,List(SCtx(Context(vd, rest@_*)), STerm(bd))) =>
        if (rest.isEmpty) Some(Some(h),f, vd.name, vd.tp, bd) else Some(Some(h),f, vd.name, vd.tp, Omb(h,f, Context(rest: _*), bd))
      case OMBIND(f,Context(vd,rest@_*),bd) =>
        if (rest.isEmpty) Some(None,f,vd.name,vd.tp,bd) else Some(None,f,vd.name,vd.tp,OMBIND(f,Context(rest:_*),bd))
      case _ => None
    }
    def apply(h : Option[HoasRule],f:Term,ln:LocalName,tp:Option[Term],bd:Term) = h match {
      case Some(h) => Omb(h,f,ln,tp,bd)
      case _ => OMBIND(f,Context(VarDecl(ln,None,tp,None,None)),bd)
    }
  }
}

object IsSeq{
    def apply(tm: Term) = tm match {
      case SHTML.flatseq(_) => true
      case OMV(_) if tm.metadata.get(SHTML.flatseq.sym).nonEmpty => true
      case _ => false
    }

    def unapply(tms: List[Term]) = {
      val i = tms.indexWhere(apply)
      if (i == -1) None else {
        Some(tms.take(i), tms(i), tms.drop(i + 1))
      }
    }
}

sealed trait STeXHOAS {
  def toTerm : Term

  def apply(head: Term, args: List[Term]): Term

  def apply(head: Term, vd:VarDecl,bd:Term): Term
}
object STeXHOAS {
  /*
  def fromTerm(tm : Obj) = tm match {
    case OMA(OMS(STeX.meta_hoas),List(OMS(a),OMS(l))) =>
      Some(SimpleHOAS(HOAS(a,l)))
    case OMA(OMS(STeX.meta_hoas), List(
    OMA(OMS(STeX.meta_hoas), List(OMS(a1), OMS(l1))),
    OMA(OMS(STeX.meta_hoas), List(OMS(a2), OMS(l2))))) =>
      Some(NestedHOAS(HOAS(a1,l1),HOAS(a2,l2)))
    case _ => None
  } */
}
/*
case class SimpleHOAS(inner:HOAS) extends STeXHOAS {
  override def toTerm: Term = STeX.meta_hoas(OMS(inner.apply),OMS(inner.bind))

  def apply(head: Term, args: List[Term]): Term =
    args.foldLeft(head)((f,a) => inner.apply(f,a))

  def apply(head: Term, vd: VarDecl, bd: Term): Term =
    inner.apply(head,OMBIND(OMS(inner.bind),Context(vd),bd))

}
case class NestedHOAS(obj:HOAS,meta:HOAS) extends STeXHOAS{
  override def toTerm: Term = STeX.meta_hoas(
    STeX.meta_hoas(OMS(obj.apply),OMS(obj.bind)),
    STeX.meta_hoas(OMS(meta.apply),OMS(meta.bind))
  )
  def apply(head: Term, args: List[Term]): Term = ???

  def apply(head: Term, vd: VarDecl, bd: Term): Term = ???
}
*/

object OMDocHTML {
  /*
  def setHOAS(se : HasMetaData,hoas:STeXHOAS) = {
    se.metadata.update(STeX.meta_hoas,hoas.toTerm)
  }
  def getHOAS(se: HasMetaData) = {
    se.metadata.get(STeX.meta_hoas) match {
      case List(a) =>
        STeXHOAS.fromTerm(a.value)
      case _ => None
    }
  }

  def getNotations(s : GlobalName)(implicit controller:Controller) = {
    val ret = controller.depstore.queryList(s,-NotationExtractor.notation)
    ret.flatMap(controller.getO).collect {
      case c : Constant if c.tp.exists(t => STeX.notation.tp.unapply(t).exists(_._1 == s)) =>
        c.df.flatMap(STeX.notation.unapply).map(t => (c.parent,t._1,t._2,t._3,t._4))
    }.flatten
  }

  def getRuler(tm: Term)(implicit controller: Controller, context: Context): Option[HasMetaData] = tm match {
    case OMS(p) => p.name match {
      case LocalName(_ :: ComplexStep(ip) :: n) =>
        getRuler(OMS(ip ? n))
      case LocalName(s :: rest) if rest.nonEmpty =>
        controller.getO(p.module ? s) match {
          case Some(s: Structure) => s.tp match {
            case Some(OMPMOD(mod, _)) =>
              getRuler(OMS(mod ? rest))
            case _ => controller.getO(p) match {
              case Some(d: Declaration) if d.path != p => getRuler(OMS(d.path))
              case Some(d) => Some(d)
              case _ => None
            }
          }
          case _ => controller.getO(p) match {
            case Some(d: Declaration) if d.path != p => getRuler(OMS(d.path))
            case Some(d) => Some(d)
            case _ => None
          }
        }
      case _ => controller.getO(p) match {
        case Some(d: Declaration) if d.path != p => getRuler(OMS(d.path))
        case Some(d) => Some(d)
        case _ => None
      }
    }
    case OMV(n) =>
      context.findLast(_.name == n)
    case Getfield(t, f) => getOriginal(t, f).flatMap(c => getRuler(c.toTerm))
    case _ => None
  }

  def getRuleInfo(tm : Term)(implicit controller:Controller,context:Context) : Option[(List[Int],Option[String],String)] =
    getRuler(tm).map(r => (OMDocHTML.getReorder(r),OMDocHTML.getAssoctype(r),OMDocHTML.getArity(r).getOrElse("")))
  /*tm match {
    case OMS(p) => p.name match {
      case LocalName(_ :: ComplexStep(ip) :: n) =>
        getRuleInfo(OMS(ip?n))
      case LocalName(s :: rest) if rest.nonEmpty =>
        controller.getO(p.module ? s) match {
          case Some(s : Structure) => s.tp match {
            case Some(OMPMOD(mod,_)) =>
              getRuleInfo(OMS(mod ? rest))
            case _ => controller.getO(p) match {
              case Some(d:Declaration) if d.path != p => getRuleInfo(OMS(d.path))
              case Some(d) => Some((OMDocHTML.getReorder(d),OMDocHTML.getAssoctype(d),OMDocHTML.getArity(d).getOrElse("")))
              case _ => None
            }
          }
          case _ => controller.getO(p) match {
            case Some(d:Declaration) if d.path != p => getRuleInfo(OMS(d.path))
            case Some(d) => Some((OMDocHTML.getReorder(d),OMDocHTML.getAssoctype(d),OMDocHTML.getArity(d).getOrElse("")))
            case _ => None
          }
        }
      case _ => controller.getO(p) match {
        case Some(d:Declaration) if d.path != p => getRuleInfo(OMS(d.path))
        case Some(d) => Some((OMDocHTML.getReorder(d),OMDocHTML.getAssoctype(d),OMDocHTML.getArity(d).getOrElse("")))
        case _ => None
      }
    }
    case OMV(n) =>
      val vd = context.findLast(_.name == n)
      vd.map(v => (OMDocHTML.getReorder(v),OMDocHTML.getAssoctype(v),OMDocHTML.getArity(v).getOrElse("")))
    case Getfield(t,f) => getOriginal(t,f).flatMap(c => getRuleInfo(c.toTerm))
    case _ => None
  }*/

  def getOriginal(tm : Term, fieldname : LocalName)(implicit controller:Controller,context:Context) : Option[Constant] = tm match {
    case OMV(n) => context.findLast(_.name == n).flatMap { vd =>
      vd.tp match {
        case Some(ModelsOf(OMPMOD(mp,as))) =>
          try {
            ModuleType(mp, as, controller.library).getOrig(fieldname)(controller.library, new History(Nil)) match {
              case Some(c: Constant) => Some(c)
              case _ => None
            }
          } catch {
            case RecordError(_) => None
          }
        case _ => None
      }
    }
    case OMS(p) => controller.getO(p).flatMap {
      case c : Constant =>
        c.tp match {
          case Some(ModelsOf(OMPMOD(mp,as))) =>
            try {
              ModuleType(mp, as, controller.library).getOrig(fieldname)(controller.library, new History(Nil)) match {
                //Try(controller.library.get(mod,fieldname)).toOption match {
                case Some(c: Constant) => Some(c)
                case _ => None
              }
            } catch {
              case RecordError(_) => None
            }
          case _ => None
        }
      case _ => None
    }
  }

  def setMacroName(se : HasMetaData, s : String) = if (s != "") {
    se.metadata.update(STeX.meta_macro,StringLiterals(s))
  }

  def getMacroName(se : HasMetaData) = se.metadata.getValues(STeX.meta_macro).collectFirst {
    case StringLiterals(mn) => mn
  }

  def setAssoctype(se : HasMetaData, s : String) =  if (s != "") se.metadata.update(STeX.meta_assoctype,StringLiterals(s))
  def getAssoctype(se : HasMetaData) =  se.metadata.getValues(STeX.meta_assoctype).collectFirst {
    case StringLiterals(at) => at
  }
  def setReorder(se : HasMetaData,s : String) = if (s != "") se.metadata.update(STeX.meta_reorder,StringLiterals(s))
  def getReorder(se : HasMetaData) =  se.metadata.getValues(STeX.meta_reorder).collectFirst {
    case StringLiterals(at) => at.split(',').map(_.toInt - 1).toList
  }.getOrElse(Nil)

  def setArity(se : HasMetaData,s : String) = if (s != "") se.metadata.update(STeX.meta_arity,StringLiterals(s))
  def getArity(se : HasMetaData) =  se.metadata.getValues(STeX.meta_arity).collectFirst {
    case StringLiterals(at) => at
  }

  def getNotations(se : StructuralElement,controller : Controller) = {
    val server = controller.extman.get(classOf[STeXServer]).head
    controller.depstore.queryList(se.path,NotationExtractor.notation).map(controller.getO).map {
      case Some(c : Constant) =>
        val arity = c.tp match {
          case Some(STeX.notation.tp(_,a)) => a
          case _ =>
            print("")
            ???
        }
        val (fragment,prec,node,op) = c.df match {
          case Some(STeX.notation(n,p,f,o)) => (f,p,n,o)
          case _ =>
            print("")
            ???
        }
        (fragment,prec,arity,HTMLParser(node.toString())(new ParsingState(controller,server.extensions.flatMap(_.rules))),
          op.map(n => HTMLParser(n.toString())(new ParsingState(controller,server.extensions.flatMap(_.rules)))))
    }
  }
  def OMAorSOMA(f : Term,args : List[Term]) = (f,args) match {
    case (ModelsOf.term,List(OMPMOD(m,as))) =>
      ModelsOf(m,as:_*)
    case (OMS(STeX.flatseq.sym),_) => STeX.flatseq(args:_*)
    case (OMS(STeX.flatseq.tp.sym),List(tp)) => STeX.flatseq.tp(tp)
    case _ => SOMA(f,args:_*)
  }

   */
}

object SHTML {
  val all_languages = List("en","de","ar","bg","ru","fi","ro","tr","fr")

  val meta_dpath = DPath(URI.http colon "mathhub.info") / "sTeX" / "meta"
  val meta_path = meta_dpath ? "Metatheory"
  val mmtmeta_path = meta_dpath ? "MMTMeta"
  val meta_quantification = mmtmeta_path ? "quantification"

  val string = mmtmeta_path ? "stringliteral"
  val nat = meta_path ? "integer literal"

  val headterm = mmtmeta_path ? "headsymbol"

  val informal = new {
    val sym = mmtmeta_path ? "informalsym"
    val op = new {
      val opsym = mmtmeta_path ? "informalapply"

      def apply(label: String, args: List[Term]) = {
        OMA(OMS(opsym), StringLiterals(label) :: args)
      }

      def unapply(tm: Term) = tm match {
        case OMA(OMS(`opsym`), StringLiterals(label) :: args) => Some((label, args))
        case _ => None
      }
    }

    def apply(n: Node) = OMA(OMS(sym), OMFOREIGN(n) :: Nil)

    def unapply(tm: Term) = tm match {
      case OMA(OMS(`sym`), OMFOREIGN(n) :: Nil) =>
        Some(n)
      case _ => None
    }
  }

  val flatseq = new {
    val sym = meta_path ? "sequence expression"

    def apply(tms: List[Term]) = OMA(OMS(sym), tms)

    def unapply(tm: Term) = tm match {
      case OMA(OMS(`sym`), ls) => Some(ls)
      case _ => None
    }

    val tp = new {
      val sym = meta_path ? "sequence type"

      def apply(tp: Term /*,lower:Term,upper:Term*/) = OMA(OMS(sym), List(tp /*,lower,upper*/))

      def unapply(tm: Term) = tm match {
        case OMA(OMS(`sym`), List(tp /*,lower,upper*/)) => Some(tp)
        case _ => None
      }
    }
  }


  val binder = new {
    val path = meta_path ? "bind"

    def apply(ctx: Context, body: Term) : Term = if (ctx.isEmpty) body else OMBIND(OMS(path), ctx, body)

    def apply(ln: LocalName, tp: Option[Term], body: Term) = OMBIND(OMS(path), Context(tp match {
      case Some(t) => OMV(ln) % t
      case None => VarDecl(ln)
    }), body)

    def unapply(tm: Term) = tm match {
      case OMBIND(OMS(`path`), Context(vd, rest@_*), bd) =>
        if (rest.isEmpty) Some(vd.name, vd.tp, bd) else Some(vd.name, vd.tp, apply(Context(rest: _*), bd))
      case _ => None
    }
  }

  val implicit_binder = new {
    val path = meta_path ? "implicit bind"

    def apply(ctx: Context, body: Term) : Term = if (ctx.isEmpty) body else OMBIND(OMS(path), ctx, body)

    def apply(ln: LocalName, tp: Option[Term], body: Term) = OMBIND(OMS(path), Context(tp match {
      case Some(t) => OMV(ln) % t
      case None => VarDecl(ln)
    }), body)

    def unapply(tm: Term) = tm match {
      case OMBIND(OMS(`path`), Context(vd, rest@_*), bd) =>
        if (rest.isEmpty) Some(vd.name, vd.tp, bd) else Some(vd.name, vd.tp, apply(Context(rest: _*), bd))
      case _ => None
    }
  }

  val prop = meta_path ? "proposition"

  val apply = meta_path ? "apply"


  import info.kwarc.mmt.api.objects.Conversions._

  val seqfoldleft = new {
    val sym = meta_path ? "fold left"

    def apply(init: Term, seq: Term, v1: LocalName, tp1: Term, v2: LocalName, tp2: Term, f: Term) =
      OMBINDC(OMS(sym),Context(v1%tp1,v2%tp2),init :: seq :: f :: Nil)//SOMB(OMS(sym), STerm(init), STerm(seq), SCtx(Context(v1 % tp1)), SCtx(v2 % tp2), STerm(f))

    def unapply(tm: Term) = tm match {
      case OMBINDC(OMS(`sym`), Context(v1,v2),List(init,seq,f)) =>
        Some((seq, init, v1.name, v1.tp, v2.name, v2.tp, f))
      case _ => None
    }
  }

  val seqfoldright = new {
    val sym = meta_path ? "fold right"

    def apply(init: Term, seq: Term, v1: LocalName, tp1: Term, v2: LocalName, tp2: Term, f: Term) =
      OMBINDC(OMS(sym), Context(v1 % tp1, v2 % tp2), init :: seq :: f :: Nil) //SOMB(OMS(sym), STerm(init), STerm(seq), SCtx(Context(v1 % tp1)), SCtx(v2 % tp2), STerm(f))

    def unapply(tm: Term) = tm match {
      case OMBINDC(OMS(`sym`), Context(v1, v2), List(init, seq, f)) =>
        Some((seq, init, v1.name, v1.tp, v2.name, v2.tp, f))
      case _ => None
    }
  }

  val seqhead = new {
    val sym = meta_path ? "head"

    def apply(seq: Term) = OMA(OMS(`sym`), List(seq))

    def unapply(tm: Term) = tm match {
      case OMA(OMS(`sym`), List(seq)) =>
        Some(seq)
      case _ => None
    }
  }

  val seqtail = new {
    val sym = meta_path ? "tail"

    def apply(seq: Term) = OMA(OMS(`sym`), List(seq))

    def unapply(tm: Term) = tm match {
      case OMA(OMS(`sym`), seq) =>
        Some(seq)
      case _ => None
    }
  }

  val seqlast = new {
    val sym = meta_path ? "last"

    def apply(seq: Term) = OMA(OMS(`sym`), List(seq))

    def unapply(tm: Term) = tm match {
      case OMA(OMS(`sym`), List(seq)) =>
        Some(seq)
      case _ => None
    }
  }

  val seqinit = new {
    val sym = meta_path ? "init"

    def apply(seq: Term) = OMA(OMS(`sym`), List(seq))

    def unapply(tm: Term) = tm match {
      case OMA(OMS(`sym`), List(seq)) =>
        Some(seq)
      case _ => None
    }
  }


  val seqmap = new {
    val sym = meta_path ? "sequence map"

    def apply(f: Term, seq: Term) = OMA(OMS(sym), List(f,seq))

    def unapply(tm: Term) = tm match {
      case OMA(OMS(`sym`),List(f,seq)) => Some((f,seq))
      case _ => None
    }
  }

  val seqprepend = new {
    val sym = meta_path ? "seqprepend"

    def apply(a: Term, seq: Term) = OMA(OMS(`sym`), List(a, seq))

    def unapply(tm: Term) = tm match {
      case OMA(OMS(`sym`), List(a, seq)) =>
        Some((a, seq))
      case _ => None
    }
  }

  val seqappend = new {
    val sym = meta_path ? "seqappend"

    def apply(seq: Term, a: Term) = OMA(OMS(`sym`), List(seq, a))

    def unapply(tm: Term) = tm match {
      case OMA(OMS(`sym`), List(seq, a)) =>
        Some((seq, a))
      case _ => None
    }
  }


  val judgmentholds = new {
    val sym = meta_path ? "judgmentholds"
  }

  /*
  val notation = new {
    val tp = new {
      val sym = mmtmeta_path ? "notationtype"
      def apply(nsym : GlobalName, arity : String) = OMA(OMS(`sym`),List(OMS(nsym),StringLiterals(arity)))

      def unapply(tm : Term) = tm match {
        case OMA(OMS(`sym`),List(OMS(nsym),StringLiterals(arity))) => Some((nsym,arity))
        case _ => None
      }
    }
    def apply(node : Node, prec : String, frag : String,op:Option[Node]) = {
      op match {
        case Some(op) =>
          OMA(OMS(meta_notation),List(StringLiterals(prec),StringLiterals(frag),OMFOREIGN(node),OMFOREIGN(op)))
        case None =>
          OMA(OMS(meta_notation),List(StringLiterals(prec),StringLiterals(frag),OMFOREIGN(node)))
      }
    }
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`meta_notation`),List(StringLiterals(prec),StringLiterals(frag),OMFOREIGN(node))) =>
        Some((node,prec,frag,None))
      case OMA(OMS(`meta_notation`),List(StringLiterals(prec),StringLiterals(frag),OMFOREIGN(node),OMFOREIGN(op))) =>
        Some((node,prec,frag,Some(op)))
      case _ => None
    }
  }
  }



  import SOMBArg._

  val symboldoc = new {
    val tp = mmtmeta_path ? "symboldoc"
    val sym = mmtmeta_path ? "symboldocfor"
    def apply(symbol : List[ContentPath],lang : String,doc : HTMLNode) = {
      OMA(OMS(sym),StringLiterals(lang) :: OMFOREIGN(doc.node) :: symbol.map(s => StringLiterals(s.toString))) // OMFOREIGN
    }
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),StringLiterals(lang) :: OMFOREIGN(n) :: ls) =>
        Some((ls.map(StringLiterals.unapply(_).get),lang,n))
      case _ =>
        None
    }
  }


  val meta_macro = mmtmeta_path ? "macroname"
  val meta_arity = mmtmeta_path ? "arity"
  val meta_assoctype = mmtmeta_path ? "assoctype"
  val meta_reorder = mmtmeta_path ? "reorder"
  val meta_hoas = mmtmeta_path ? "hoas"

  val meta_qforall = mmtmeta_path ? "universal"
  val meta_qexists = mmtmeta_path ? "existential"

  val meta_doctitle = mmtmeta_path ? "doctitle"

   */
}