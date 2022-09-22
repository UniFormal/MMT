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
import info.kwarc.mmt.stex.xhtml.HTMLParser.{HTMLNode, ParsingState}

import scala.xml._
import objects._

sealed trait STeXHOAS {
  def toTerm : Term

  def apply(head: Term, args: List[Term]): Term

  def apply(head: Term, vd:VarDecl,bd:Term): Term
}
object STeXHOAS {
  def fromTerm(tm : Obj) = tm match {
    case OMA(OMS(STeX.meta_hoas),List(OMS(a),OMS(l))) =>
      Some(SimpleHOAS(HOAS(a,l)))
    case OMA(OMS(STeX.meta_hoas), List(
    OMA(OMS(STeX.meta_hoas), List(OMS(a1), OMS(l1))),
    OMA(OMS(STeX.meta_hoas), List(OMS(a2), OMS(l2))))) =>
      Some(NestedHOAS(HOAS(a1,l1),HOAS(a2,l2)))
    case _ => None
  }
}
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


object OMDocHTML {
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
            ModuleType(mp,as,controller.library).getOrig(fieldname)(controller.library,new History(Nil)) match {
              //Try(controller.library.get(mod,fieldname)).toOption match {
              case Some(c : Constant) => Some(c)
              case _ => None
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
}

object STeX {
  val all_languages = List("en","de","ar","bg","ru","fi","ro","tr","fr")

  val meta_dpath = DPath(URI.http colon "mathhub.info") / "sTeX" / "meta"
  val meta_path = meta_dpath ? "Metatheory"
  val mmtmeta_path = meta_dpath ? "MMTMeta"

  val string = mmtmeta_path ? "stringliteral"
  val nat = meta_path ? "natliteral"

  val meta_notation = mmtmeta_path ? "notation"

  val prop = meta_path ? "proposition"

  val apply = meta_path ? "apply"

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
  val informal = new {
    val sym = mmtmeta_path ? "informalsym"
    val op = new {
      val opsym = mmtmeta_path ? "informalapply"
      def apply(label : String,args : List[Term]) = {
        OMA(OMS(opsym),StringLiterals(label) :: args)
      }
      def unapply(tm : Term) = tm match {
        case OMA(OMS(`opsym`),StringLiterals(label) :: args) => Some((label,args))
        case _ => None
      }
    }
    def apply(n : Node) = OMA(OMS(sym),OMFOREIGN(n) :: Nil)
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),OMFOREIGN(n) :: Nil) =>
        Some(n)
      case _ => None
    }
  }

  val flatseq = new {
    val sym = meta_path ? "seqexpr"
    def apply(tms : Term*) = OMA(OMS(sym),tms.toList)
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),ls) => Some(ls)
      case _ => None
    }
    val tp = new {
      val sym = meta_path ? "seqtype"
      def apply(tp : Term/*,lower:Term,upper:Term*/) = OMA(OMS(sym),List(tp/*,lower,upper*/))
      def unapply(tm : Term) = tm match {
        case OMA(OMS(`sym`),List(tp/*,lower,upper*/)) => Some(tp)
        case _ => None
      }
    }
  }

  val seqmap = new {
    val sym = meta_path ? "seqmap"
    def apply(seq : Term,v : LocalName,f : Term) = SOMB(OMS(sym),STerm(seq),SCtx(Context(VarDecl(v))),STerm(f))
    def unapply(tm : Term) = tm match {
      case SOMB(OMS(`sym`),List(STerm(seq),SCtx(Context(v)),STerm(f))) =>
        Some((seq,v.name,v.tp,f))
      case _ => None
    }
  }

  val seqprepend = new {
    val sym = meta_path ? "seqprepend"
    def apply(a : Term,seq : Term) = SOMA(OMS(`sym`),a,seq)
    def unapply(tm : Term) = tm match {
      case SOMA(OMS(`sym`),List(a,seq)) =>
        Some((a,seq))
      case _ => None
    }
  }

  val seqappend = new {
    val sym = meta_path ? "seqappend"
    def apply(seq : Term,a : Term) = SOMA(OMS(`sym`),seq,a)
    def unapply(tm : Term) = tm match {
      case SOMA(OMS(`sym`),List(seq,a)) =>
        Some((seq,a))
      case _ => None
    }
  }

  import info.kwarc.mmt.api.objects.Conversions._

  val seqfoldleft = new {
    val sym = meta_path ? "seqfoldleft"
    def apply(init:Term,seq : Term,v1 : LocalName, tp1: Term,v2:LocalName, tp2 : Term,f : Term) = SOMB(OMS(sym),STerm(init),STerm(seq),SCtx(Context(v1 % tp1)),SCtx(v2 % tp2),STerm(f))
    def unapply(tm : Term) = tm match {
      case SOMB(OMS(`sym`),List(STerm(init),STerm(seq),SCtx(Context(v1)),SCtx(Context(v2)),STerm(f))) =>
        Some((seq,init,v1.name,v1.tp,v2.name,v2.tp,f))
      case _ => None
    }
  }

  val seqfoldright = new {
    val sym = meta_path ? "seqfoldright"
    def apply(init:Term,seq : Term,v1 : LocalName, tp1: Term,v2:LocalName, tp2 : Term,f : Term) = SOMB(OMS(sym),STerm(init),STerm(seq),SCtx(Context(v1 % tp1)),SCtx(v2 % tp2),STerm(f))
    def unapply(tm : Term) = tm match {
      case SOMB(OMS(`sym`),List(STerm(init),STerm(seq),SCtx(Context(v1)),SCtx(Context(v2)),STerm(f))) =>
        Some((seq,init,v1.name,v1.tp,v2.name,v2.tp,f))
      case _ => None
    }
  }

  val seqhead = new {
    val sym = meta_path ? "seqhead"
    def apply(seq : Term) = SOMA(OMS(`sym`),seq)
    def unapply(tm : Term) = tm match {
      case SOMA(OMS(`sym`),seq) =>
        Some(seq)
      case _ => None
    }
  }

  val seqtail = new {
    val sym = meta_path ? "seqtail"
    def apply(seq : Term) = SOMA(OMS(`sym`),seq)
    def unapply(tm : Term) = tm match {
      case SOMA(OMS(`sym`),seq) =>
        Some(seq)
      case _ => None
    }
  }

  val seqlast = new {
    val sym = meta_path ? "seqlast"
    def apply(seq : Term) = SOMA(OMS(`sym`),seq)
    def unapply(tm : Term) = tm match {
      case SOMA(OMS(`sym`),seq) =>
        Some(seq)
      case _ => None
    }
  }

  val seqinit = new {
    val sym = meta_path ? "seqinit"
    def apply(seq : Term) = SOMA(OMS(`sym`),seq)
    def unapply(tm : Term) = tm match {
      case SOMA(OMS(`sym`),seq) =>
        Some(seq)
      case _ => None
    }
  }

  import SOMBArg._

  val binder = new {
    val path = meta_path ? "bind"
    def apply(ctx : Context,body : Term) = SOMB(OMS(path),ctx,body)
    def apply(ln : LocalName,tp : Option[Term],body : Term) = SOMB(OMS(path),Context(tp match {
      case Some(t) => OMV(ln) % t
      case None => VarDecl(ln)
    }),body)
    def unapply(tm : Term) = tm match {
      case SOMB(OMS(`path`),List(SCtx(Context(vd, rest @_*)),STerm(bd))) =>
        if (rest.isEmpty) Some(vd.name,vd.tp,bd) else Some(vd.name,vd.tp,apply(Context(rest:_*),bd))
      case _ => None
    }
  }
  val implicit_binder = new {
    val path = meta_path ? "implicitbind"
    def apply(ctx : Context,body : Term) = SOMB(OMS(path),ctx,body)
    def apply(ln : LocalName,tp : Option[Term],body : Term) = SOMB(OMS(path),Context(tp match {
      case Some(t) => OMV(ln) % t
      case None => VarDecl(ln)
    }),body)
    def unapply(tm : Term) = tm match {
      case SOMB(OMS(`path`),List(SCtx(Context(vd, rest @_*)),STerm(bd))) =>
        if (rest.isEmpty) Some(vd.name,vd.tp,bd) else Some(vd.name,vd.tp,apply(Context(rest:_*),bd))
      case _ => None
    }
  }

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

  val judgmentholds = new {
    val sym = meta_path ? "judgmentholds"
    def apply(tm : Term) = SOMA(OMS(sym),tm)
    def unapply(tm : Term) = tm match {
      case SOMA(OMS(`sym`),List(t)) => Some(t)
      case _ => None
    }
  }

  val meta_macro = mmtmeta_path ? "macroname"
  val meta_language = mmtmeta_path ? "language"
  val meta_arity = mmtmeta_path ? "arity"
  val meta_assoctype = mmtmeta_path ? "assoctype"
  val meta_reorder = mmtmeta_path ? "reorder"
  val meta_hoas = mmtmeta_path ? "hoas"

  val meta_quantification = mmtmeta_path ? "quantification"
  val meta_qforall = mmtmeta_path ? "universal"
  val meta_qexists = mmtmeta_path ? "existential"

  val meta_doctitle = mmtmeta_path ? "doctitle"
}