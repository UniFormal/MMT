package info.kwarc.mmt.api.backend
import info.kwarc.mmt.api._

abstract class ArchiveCustomization {
  def mwsurl(p: Path) : String
}

class DefaultCustomization extends ArchiveCustomization {
  def mwsurl(p: Path) : String = p.toPath
}

class MML extends ArchiveCustomization {
  def mwsurl(p: Path) : String = "http://www.mizar.org/version/current/html/" + "%m.html#o"
}