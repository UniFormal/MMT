package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._

class Closer(controller: Controller) {
   private val lup = controller.globalLookup
   /**
    * recursive loads all theories included into p
    */
   def apply(p: MPath) {
       lup.get(p) match {
          case d: DeclaredTheory =>
             d.meta foreach apply
             d.getPrimitiveDeclarations foreach {
                case PlainInclude(from, _) => apply(from)
                case _ =>
             }
          case _ =>
       }
   }
}