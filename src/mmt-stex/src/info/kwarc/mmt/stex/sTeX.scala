package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardDouble, StandardInt, StandardNat, StandardPositive, StandardRat, StandardString}
import info.kwarc.mmt.api.utils.{URI, XMLEscaping}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX
import info.kwarc.mmt.sequences.Sequences
import info.kwarc.mmt.stex.rules.StringLiterals
import info.kwarc.mmt.stex.xhtml.HTMLParser.HTMLNode

import scala.xml._
import objects._

object STeX {
  val meta_dpath = DPath(URI.http colon "mathhub.info") / "sTeX" / "meta"
  val meta_path = meta_dpath ? "Metatheory"
  val mmtmeta_path = meta_dpath ? "MMTMeta"

  val string = mmtmeta_path ? "stringliteral"

  val meta_notation = mmtmeta_path ? "notation"

  val prop = meta_path ? "proposition"

  val notation = new {
    val tp = new {
      val sym = mmtmeta_path ? "notationtype"
      def apply(nsym : GlobalName, arity : String) = OMA(OMS(`sym`),List(OMS(nsym),StringLiterals(arity)))

      def unapply(tm : Term) = tm match {
        case OMA(OMS(`sym`),List(OMS(nsym),StringLiterals(arity))) => Some((nsym,arity))
        case _ => None
      }
    }
    def apply(node : Node, prec : String, frag : String) = {
      OMA(OMS(meta_notation),List(StringLiterals(prec),StringLiterals(frag),OMFOREIGN(node)))
    }
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`meta_notation`),List(StringLiterals(prec),StringLiterals(frag),OMFOREIGN(node))) =>
        Some((node,prec,frag))
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
      def apply(tp : Term) = OMA(OMS(sym),List(tp))
      def unapply(tm : Term) = tm match {
        case OMA(OMS(`sym`),List(tp)) => Some(tp)
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
    def apply(a : Term,seq : Term) = OMA(OMS(`sym`),List(a,seq))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(a,seq)) =>
        Some((a,seq))
      case _ => None
    }
  }

  val seqappend = new {
    val sym = meta_path ? "seqappend"
    def apply(seq : Term,a : Term) = OMA(OMS(`sym`),List(seq,a))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(seq,a)) =>
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
    def apply(seq : Term) = OMA(OMS(`sym`),List(seq))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(seq)) =>
        Some(seq)
      case _ => None
    }
  }

  val seqtail = new {
    val sym = meta_path ? "seqtail"
    def apply(seq : Term) = OMA(OMS(`sym`),List(seq))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(seq)) =>
        Some(seq)
      case _ => None
    }
  }

  val seqlast = new {
    val sym = meta_path ? "seqlast"
    def apply(seq : Term) = OMA(OMS(`sym`),List(seq))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(seq)) =>
        Some(seq)
      case _ => None
    }
  }

  val seqinit = new {
    val sym = meta_path ? "seqinit"
    def apply(seq : Term) = OMA(OMS(`sym`),List(seq))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(seq)) =>
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
    def apply(tm : Term) = OMA(OMS(sym),List(tm))
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(t)) => Some(t)
      case _ => None
    }
  }

  val meta_macro = mmtmeta_path ? "macroname"
  val meta_language = mmtmeta_path ? "language"
  val meta_arity = mmtmeta_path ? "arity"
  val meta_assoctype = mmtmeta_path ? "assoctype"
  val meta_reorder = mmtmeta_path ? "reorder"

  val meta_quantification = mmtmeta_path ? "quantification"
  val meta_qforall = mmtmeta_path ? "universal"
  val meta_qexists = mmtmeta_path ? "existential"
}