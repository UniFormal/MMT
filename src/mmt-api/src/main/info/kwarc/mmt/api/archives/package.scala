package info.kwarc.mmt.api

import archives._

/**
  * This package holds all classes related to MMT [[Archive]]s (= project).
  *
  * The [[BuildManager]] allows defining [[BuildTarget]]s to run on archives, including in particular [[Importer]]s and [[Exporter]]s.
  *
  * Most archives are stored in hub of git repositories such as and MathHub or GitHub.
  * Such hubs are represented by the class [[LMHHub]], which allows for cloning, etc. archives.  
  *
  * The list of currently open archives (from which MMT will load content if needed) is maintained by the [[backend.Backend]].
  */
package object archives
{
  val source     : ArchiveDimension = RedirectableDimension("source")
  val content    : ArchiveDimension = RedirectableDimension("content")
  val narration  : ArchiveDimension = RedirectableDimension("narration")
  val relational : ArchiveDimension = RedirectableDimension("relational")
  val notational : ArchiveDimension = RedirectableDimension("notations")
  val errors     : ArchiveDimension = RedirectableDimension("errors")
  val `export`     : ArchiveDimension = RedirectableDimension("export")
  val flat       : ArchiveDimension = RedirectableDimension("flat")
}
