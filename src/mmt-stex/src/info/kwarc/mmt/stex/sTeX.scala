package info.kwarc.mmt.stex

import info.kwarc.mmt.api._
import parser._
import scala.xml._
import modules._
import symbols._
import objects._
import informal._
import utils._
import metadata._
import uom.OMLiteral.OMSTR
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
