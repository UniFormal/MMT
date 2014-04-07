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
}