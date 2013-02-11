package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import documents._
import modules._
import symbols._
import objects._

/** This class can be used to conveniently apply the same operation to all terms in a structural element */
class StructureTraverser(controller: Controller) {
    /** applies f to all Terms in s */
    def apply(s: StructuralElement)(implicit f: (CPath,TermContainer) => Unit) {s match {
       case d: Document =>
          d.getItems foreach {
             case r if r.isGenerated =>
                val se = controller.get(r.target)
                apply(se)
          }
       //case m: DefinedModule =>
         // f(m.path $ DefComponent, m.df)
       case c: Constant =>
          f(c.path $ TypeComponent, c.tpC)
          f(c.path $ DefComponent, c.dfC)
       case _ => 
       }
    }
}