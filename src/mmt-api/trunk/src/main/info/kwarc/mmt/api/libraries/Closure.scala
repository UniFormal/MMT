package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._
import patterns._

class Closer(controller: Controller) {
   private val lup = controller.globalLookup
   /**
    * recursive loads all theories included into p
    */
   def apply(p: MPath) {
       val d = lup.getO(p)
       d match {
          case Some(d: DeclaredTheory) =>
             d.getIncludes foreach apply
          case _ =>
       }
   }
}