package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.uom.{RepresentedRealizedType, StandardDouble, StandardInt, StandardNat, StandardPositive, StandardRat, StandardString}
import parser._

import scala.xml._
import modules._
import symbols._
import objects._
import informal._
import utils._
import metadata._
import uom.OMLiteral.OMSTR

object STeX {
  val fomid_dpath = DPath(utils.URI(Some("fomid"), None, abs=true))
  val foundation = fomid_dpath / "foundation"
  val core = fomid_dpath / "core"
  val metadata = foundation ? "Metadata"
  val ur = DPath(URI.http colon "cds.omdoc.org")/ "urtheories"
  val meta = foundation ? "Meta"

  import info.kwarc.mmt.api.objects.Conversions._
  val prop = ur ? "Bool" ? "BOOL"
  val ded = ur ? "Ded" ? "DED"

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

  val meta_quantification = metadata ? "quantification"
  val meta_source = metadata ? "source"
  val meta_macro = metadata ? "macroname"
  val meta_notation = metadata ? "notation"
  val meta_arity = metadata ? "arity"
  val meta_vardecl = metadata ? "vardecl"

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

/*

object sTeXMetaData {
  val mod : MPath = DPath(URI("http://mathhub.info/metadata/stex.omdoc")) ? "stex"
  val primarySymbolPath = mod ? "primary-symbol"
  val rolePath = mod ? "role"

  val primarySymbol : MetaDatum = new MetaDatum(rolePath, OMSTR("primary"))
  val conservativeExtension : MetaDatum = new MetaDatum(rolePath, OMSTR("conservative-extension"))
}

object sTeX {
  def inSmglom(p : Path) : Boolean = {
    //group is smglom
    p.doc.uri.path.head == "smglom"
  }

  def getLanguage(p : Path) : Option[String] = {
    val name = p.dropComp match {
      case s : GlobalName => s.module.name.toPath
      case m : MPath => m.name.toPath
      case d : DPath => d.last.split('.').toList.init.mkString(".") // removing extension
    }
    name.split("\\.").toList match {
      case hd :: lang :: tl => Some(lang)
      case _ => None
    }
  }

  def getMasterPath(p : GlobalName) : GlobalName = _recursePath(p)(_getMasterName)
  def getMasterPath(p : MPath) : MPath = _recursePath(p)(_getMasterName)
  def getMasterPath(p : DPath) : DPath = _recursePath(p)(_getMasterName)

  def getLangPath(p : GlobalName, lang : String) : GlobalName = _recursePath(p)(_getLangName(lang))
  def getLangPath(p : MPath, lang : String) : MPath = _recursePath(p)(_getLangName(lang))
  def getLangPath(p : DPath, lang : String) : DPath = _recursePath(p)(_getLangName(lang))

  private def _recursePath(p : GlobalName)(f : String => String) : GlobalName = _recursePath(p.module)(f) ? p.name.toPath
  private def _recursePath(p : MPath)(f : String => String) : MPath = _recursePath(p.doc)(f) ? f(p.name.toPath)
  private def _recursePath(p : DPath)(f : String => String) : DPath = p.^! / f(p.last)

  private def _getLangName(lang : String)(s : String) : String = {
    s.split("\\.").toList match {
      case name  :: tl => (name :: lang :: tl).mkString(".")
      case _ => s
    }
  }

  private def _getMasterName(s : String) : String = {
    s.split("\\.").toList match {
      case name :: lang :: tl => (name :: tl).mkString(".")
      case _ => s
    }
  }
}
*/
