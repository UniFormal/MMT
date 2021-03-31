package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardDouble, StandardInt, StandardNat, StandardPositive, StandardRat, StandardString}
import info.kwarc.mmt.api.utils.{URI, XMLEscaping}
import info.kwarc.mmt.lf.ApplySpine
import info.kwarc.mmt.stex.xhtml.{XHTML, XHTMLNode}

import scala.xml._
import objects._

object STeX {
  val fomid_dpath = DPath(utils.URI(Some("fomid"), None, abs=true))
  val foundation = fomid_dpath / "foundation"
  val core = fomid_dpath / "core"
  val metadata = foundation ? "Metadata"
  val ur = DPath(URI.http colon "cds.omdoc.org")/ "urtheories"
  val meta = foundation ? "Meta"


  val meta_quantification = metadata ? "quantification"
  val meta_source = metadata ? "source"
  val meta_macro = metadata ? "macroname"
  val meta_notation = metadata ? "notation"
  val meta_arity = metadata ? "arity"
  val meta_vardecl = metadata ? "vardecl"
  val meta_language = metadata ? "language"



  val pos = meta ? "POS"
  val nat = ur ? "NatSymbols" ? "NAT"
  val int = ur ? "IntSymbols" ? "INT"
  val rat = ur ? "RatSymbols" ? "RAT"
  val real = ur ? "RealSymbols" ? "REAL"
  val string = ur ? "Strings" ? "string"

  object PosLiterals extends RepresentedRealizedType(OMS(pos),StandardPositive)
  object NatLiterals extends RepresentedRealizedType(OMS(nat),StandardNat)
  object IntLiterals extends RepresentedRealizedType(OMS(int),StandardInt)
  object RatLiterals extends RepresentedRealizedType(OMS(rat),StandardRat)
  object RealLiterals extends RepresentedRealizedType(OMS(real),StandardDouble)
  object StringLiterals extends RepresentedRealizedType(OMS(string),StandardString)




  val informal = new {
    val sym = foundation ? "Informal" ? "informal-sym"
    val opsym = foundation ? "Informal" ? "informal-apply"
    def applyOp(label : String,args : List[Term]) = {
      OMA(OMS(opsym),StringLiterals(label) :: args)
    }
    def applySimple(n : Node) = OMA(OMS(sym),StringLiterals(n.toString()) :: Nil)
    def unapply(tm : Term) = tm match {
      case OMA(OMS(`sym`),StringLiterals(n) :: Nil) =>
        Some(XHTML.applyString(XHTML.unescape(n))(Nil).head)
      case _ => None
    }
  }

  def language(l : String) = metadata ? l

  val symboldoc = new {
    val th = string.module
    val tp = th ? "symboldoc"
    val sym = th ? "symboldocfor"
    def apply(symbol : ContentPath,lang : String,doc : List[XHTMLNode]) = {
      ApplySpine(OMS(sym),StringLiterals(symbol.toString),StringLiterals(lang),StringLiterals(XMLEscaping({<div>{doc.map(_.node)}</div>}.toString)))
    }
    def unapply(tm : Term) = tm match {
      case ApplySpine(OMS(`sym`),List(StringLiterals(s),StringLiterals(lang),StringLiterals(n))) =>
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






  val prop = ur ? "Bool" ? "BOOL"
  val ded = ur ? "Ded" ? "DED"


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