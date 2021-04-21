package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardDouble, StandardInt, StandardNat, StandardPositive, StandardRat, StandardString}
import info.kwarc.mmt.api.utils.{URI, XMLEscaping}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.odk.LFX
import info.kwarc.mmt.sequences.Sequences
import info.kwarc.mmt.stex.xhtml.{XHTML, XHTMLNode}

import scala.xml._
import objects._

object STeX {
  val fomid_dpath = DPath(utils.URI(Some("fomid"), None, abs=true))
  val foundation = fomid_dpath / "foundation"
  val core = fomid_dpath / "core"
  val metadata = foundation ? "Metadata"
  //val ur = DPath(URI.http colon "cds.omdoc.org")/ "urtheories"
  val meta = foundation ? "Meta"


  val meta_quantification = metadata ? "quantification"
  val meta_source = metadata ? "source"
  val meta_macro = metadata ? "macroname"
  val meta_notation = metadata ? "notation"
  val meta_arity = metadata ? "arity"
  val meta_vardecl = metadata ? "vardecl"
  val meta_language = metadata ? "language"



  val pos = (foundation / "literals") ? "PositiveNaturals" ? "positivenaturalliteral"
  val nat = (foundation / "literals") ? "Naturals" ? "naturalliteral"
  val int = (foundation / "literals") ? "Integers" ? "integerliteral"
  val rat = (foundation / "literals") ? "Rationals" ? "rationalliteral"
  val real = (foundation / "literals") ? "Reals" ? "realliteral"
  val string = (foundation / "literals") ? "Strings" ? "stringliteral"

  object PosLiterals extends RepresentedRealizedType(OMS(pos),StandardPositive)
  object NatLiterals extends RepresentedRealizedType(OMS(nat),StandardNat)
  object IntLiterals extends RepresentedRealizedType(OMS(int),StandardInt)
  object RatLiterals extends RepresentedRealizedType(OMS(rat),StandardRat)
  object RealLiterals extends RepresentedRealizedType(OMS(real),StandardDouble)
  object StringLiterals extends RepresentedRealizedType(OMS(string),StandardString)

  val notation = new {
    val sym = foundation ? "Notations" ? "notation"
    val tp = new {
      val sym = foundation ? "Notations" ? "notation-type"
      def apply(nsym : GlobalName, arity : String) = OMA(OMS(`sym`),List(OMS(nsym),StringLiterals(arity)))

        def unapply(tm : Term) = tm match {
          case OMA(OMS(`sym`),List(OMS(nsym),StringLiterals(arity))) => Some((nsym,arity))
          case _ => None
        }
    }
    def apply(node : Node, prec : String) = {
      OMA(OMS(sym),List(StringLiterals(prec),StringLiterals(node.toString())))
    }
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(StringLiterals(prec),StringLiterals(node))) =>
        Some((node,prec))
      case _ => None
    }
  }


  val informal = new {
    val sym = foundation ? "Informal" ? "informal-sym"
    val opsym = foundation ? "Informal" ? "informal-apply"
    def applyOp(label : String,args : List[Term]) = {
      OMA(OMS(opsym),StringLiterals(label) :: args)
    }
    def applySimple(n : Node) = OMA(OMS(sym),StringLiterals(n.toString()) :: Nil)
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),StringLiterals(n) :: Nil) =>
        Some(XHTML.applyString(XHTML.unescape(n)))
      case _ => None
    }
  }

  def language(l : String) = metadata ? l

  val symboldoc = new {
    val th = string.module
    val tp = th ? "symboldoc"
    val sym = th ? "symboldocfor"
    def apply(symbol : ContentPath,lang : String,doc : List[XHTMLNode]) = {
      OMA(OMS(sym),List(StringLiterals(symbol.toString),StringLiterals(lang),StringLiterals(XMLEscaping({<div>{doc.map(_.node)}</div>}.toString))))
    }
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),List(StringLiterals(s),StringLiterals(lang),StringLiterals(n))) =>
        Some((s,lang,n))
      case _ =>
        None
    }
  }

  import info.kwarc.mmt.api.objects.Conversions._

  val universal_quantifier = new {
    val path = foundation ? "Bindings" ? "universal"
    def apply(ctx : Context,body : Term) = OMBIND(OMS(path),ctx,body)
    def apply(ln : LocalName,tp : Option[Term],body : Term) = OMBIND(OMS(path),tp match {
      case Some(t) => OMV(ln) % t
      case None => VarDecl(ln)
    },body)
    def unapply(tm : Term) = tm match {
      case OMBIND(OMS(`path`),Context(vd, rest @_*),bd) =>
        if (rest.isEmpty) Some(vd.name,vd.tp,bd) else Some(vd.name,vd.tp,apply(Context(rest:_*),bd))
      case _ => None
    }
  }

  val existential_quantifier = new {
    val path = foundation ? "Bindings" ? "existential"
    def apply(ctx : Context,body : Term) = OMBIND(OMS(path),ctx,body)
    def apply(ln : LocalName,tp : Option[Term],body : Term) = OMBIND(OMS(path),tp match {
      case Some(t) => OMV(ln) % t
      case None => VarDecl(ln)
    },body)
    def unapply(tm : Term) = tm match {
      case OMBIND(OMS(`path`),Context(vd, rest @_*),bd) =>
        if (rest.isEmpty) Some(vd.name,vd.tp,bd) else Some(vd.name,vd.tp,apply(Context(rest:_*),bd))
      case _ => None
    }
  }

  val let = new {
    val path = foundation ? "Bindings" ? "let"
    def apply(ln : LocalName, defi : Term,body : Term) = OMBIND(OMS(path),Context(VarDecl(ln,None,None,Some(defi),None)),body)
    def unapply(tm : Term) = tm match {
      case OMBIND(OMS(`path`),Context(vd),bd) if vd.df.isDefined =>
        Some(vd.name,vd.df.get,bd)
      case _ => None
    }
  }

  val flatseq = new {
    val sym = foundation ? "Sequences" ? "seq"
    def apply(tms : Term*) = OMA(OMS(sym),tms.toList)
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),ls) => Some(ls)
      case _ => None
    }
    val tp = new {
      val sym = foundation ? "Sequences" ? "seqtype"
      def apply(tp : Term) = OMA(OMS(sym),List(tp))
      def unapply(tm : Term) = tm match {
        case OMA(OMS(`sym`),List(tp)) => Some(tp)
        case _ => None
      }
    }
  }

  val prop = (foundation / "foundations") ? "Foundation" ? "judgment"
  val ded = (foundation / "foundations") ? "Foundation" ? "holds"







  val set = (core / "sets") ? "Sets" ? "Set"
  val funtype = (core / "sets") ? "Functions" ? "FunctionType"
  object Forall {
    val path = (core / "logic") ? "UniversalQuantifier" ? "Forall"
    def apply(x : LocalName,tp : Option[Term], tm : Term) : OMBINDC = tm match {
      case Forall(ctx2,bd) =>
        OMBIND(OMS(path),ctx2 ++ (if (tp.isDefined) x%tp.get else VarDecl(x)),bd)
      case _ =>
        OMBIND(OMS(path),Context(VarDecl(x,None,tp,None,None)),tm)
    }
    def apply(ctx : Context,tm : Term) : OMBINDC = tm match {
      case Forall(ctx2,bd) =>
        OMBIND(OMS(path),ctx2 ++ ctx,bd)
      case _ =>
        OMBIND(OMS(path),ctx,tm)
    }
    def unapply(tm : Term) : Option[(Context,Term)] = tm match {
      case OMBIND(OMS(`path`),ctx,bd) if ctx.nonEmpty =>
        unapply(bd) match {
          case Some((ctx2,bd2)) =>
            Some(ctx2 ++ ctx,bd2)
          case _ => Some(ctx,bd)
        }
      case _ => None
    }
  }
  object Exists {
    val path = (core / "logic") ? "ExistentialQuantifier" ? "Exists"
    def apply(x : LocalName,tp : Option[Term], tm : Term) : OMBINDC = tm match {
      case Exists(ctx2,bd) =>
        OMBIND(OMS(path),ctx2 ++ (if (tp.isDefined) x%tp.get else VarDecl(x)),bd)
      case _ =>
        OMBIND(OMS(path),Context(VarDecl(x,None,tp,None,None)),tm)
    }
    def apply(ctx : Context,tm : Term) : OMBINDC = tm match {
      case Exists(ctx2,bd) =>
        OMBIND(OMS(path),ctx2 ++ ctx,bd)
      case _ =>
        OMBIND(OMS(path),ctx,tm)
    }
    def unapply(tm : Term) : Option[(Context,Term)] = tm match {
      case OMBIND(OMS(`path`),ctx,bd) if ctx.nonEmpty =>
        unapply(bd) match {
          case Some((ctx2,bd2)) =>
            Some(ctx2 ++ ctx,bd2)
          case _ => Some(ctx,bd)
        }
      case _ => None
    }
  }
}