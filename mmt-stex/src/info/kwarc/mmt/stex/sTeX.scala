package info.kwarc.mmt.stex

import info.kwarc.mmt.api._

object sTeX {
  def getLanguage(p : Path) : Option[String] = {
    val name = p match {
      case s : GlobalName => s.module.toMPath.name.toPath
      case m : MPath => m.name.toPath
      case d : DPath => d.last
      case c : CPath => c.parent.module.toMPath.name.toPath
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
  
  private def _recursePath(p : GlobalName)(f : String => String) : GlobalName = _recursePath(p.module.toMPath)(f) ? p.name.toPath
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