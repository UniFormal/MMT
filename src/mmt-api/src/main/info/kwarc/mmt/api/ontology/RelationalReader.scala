package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import frontend._
import archives._

/**
 * a change listener that guarantees that relational is read for all archives
 */
class RelationalReader extends ArchiveChangeListener {
   private var read: List[Archive] = Nil

   def oncePerArchive(a: Archive): Unit = {
      if (! (read contains a)) {
         a.readRelational(Nil, controller, "rel")
         read ::= a
      }
   }
}
