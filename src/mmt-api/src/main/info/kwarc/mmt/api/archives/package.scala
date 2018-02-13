package info.kwarc.mmt.api

import archives._

/**
  * This package holds all classes related to MMT [[Archive]]s.
  *
  * Archives can be seen as projects.
  *
  * The list of open archives is maintained by the [[backend.Backend]].
  */
package object archives {
   val source = RedirectableDimension("source")
   val content = RedirectableDimension("content")
   val narration = RedirectableDimension("narration")
   val relational = RedirectableDimension("relational")
   val errors = RedirectableDimension("errors")
   val export = RedirectableDimension("export")
   val flat = RedirectableDimension("flat")
}
