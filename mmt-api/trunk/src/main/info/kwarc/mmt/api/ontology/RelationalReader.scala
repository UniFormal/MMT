package info.kwarc.mmt.api.ontology

import info.kwarc.mmt.api._
import frontend._
import archives._

/**
 * a change listener that guarantees that relational is read for all archives
 */
class RelationalReader extends ChangeListener {
   private var read: List[Archive] = Nil 
    
   override def start(args: List[String]) {
      ensureAllRead
   }

   override def onArchiveOpen(a: Archive) {
      ensureRead(a)
   }
   
   def ensureAllRead {
      controller.backend.getArchives foreach ensureRead
   }
   
   def ensureRead(a: Archive) {
      if (! (read contains a)) {
         a.readRelational(Nil, controller, "rel")
         read ::= a
      }
   }
}