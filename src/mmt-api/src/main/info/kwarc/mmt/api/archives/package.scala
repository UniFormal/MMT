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
package object archives {
   val source = RedirectableDimension("source")
   val content = RedirectableDimension("content")
   val narration = RedirectableDimension("narration")
   val relational = RedirectableDimension("relational")
   val notational = RedirectableDimension("notations")
   val errors = RedirectableDimension("errors")
   val export = RedirectableDimension("export")
   val flat = RedirectableDimension("flat")
}
