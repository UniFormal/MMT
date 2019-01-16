package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import frontend._

/**
 * a change listener that call a function exactly once for each open archive
 */
abstract class ArchiveChangeListener extends ChangeListener {
   override def start(args: List[String]) {
      ensureAll
   }
   override def onArchiveOpen(a: Archive) {
      oncePerArchive(a)
   }

   private def ensureAll {
      controller.backend.getArchives foreach oncePerArchive
   }

   def oncePerArchive(a: Archive): Unit
}