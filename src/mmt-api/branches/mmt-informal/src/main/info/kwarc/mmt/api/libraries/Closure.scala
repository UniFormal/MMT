package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._
import patterns._

class Closer(controller: Controller) {
   private val lup = controller.globalLookup
   private val elab = new InstanceElaborator(controller)
   /**
    * recursive loads all theories included into p
    */
   def apply(p: MPath) {
       lup.get(p) match {
          case d: DeclaredTheory =>
             d.getIncludes foreach apply
             elab.elaborate(d)
          case _ =>
       }
   }
}