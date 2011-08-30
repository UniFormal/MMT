package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._

abstract class ArchiveCustomization {
  def mwsurl(p: Path) : String
}

class DefaultCustomization extends ArchiveCustomization {
  def mwsurl(p: Path) : String = p.toPath
}

class MML extends ArchiveCustomization {
  def mwsurl(p: Path) : String = p match {
    case par % ln => 
      val thyName = par.toMPath.name.flat
      val symbolName = ln.head.toPath
      "http://www.mizar.org/version/current/html/" + thyName.toLowerCase() + ".html#" + symbolName
    case _ => throw ImplementationError("Module level references not implemented yet")
     // "http://www.mizar.org/version/current/html/" + "%m.html#o"
  }
}