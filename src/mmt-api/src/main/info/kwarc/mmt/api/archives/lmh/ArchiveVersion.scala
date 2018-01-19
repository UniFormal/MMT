package info.kwarc.mmt.api.archives.lmh

/** represents hard-coded versions of MathHub archives */
object ArchiveVersion {
  /** gets the hard-coded version of an archive, if any */
  def apply(archive: String): Option[String] = None

  /** returns the default version of an archive, unless one is explicitly set */
  def apply(archive: String, version: Option[String]) : Option[String] = version match {
    case Some(v) => Some(v)
    case None => apply(archive)
  }
}
