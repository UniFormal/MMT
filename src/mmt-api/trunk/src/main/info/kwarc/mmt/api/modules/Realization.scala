package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api._
import symbols._
import objects._
import uom._

/** A Realization is a special case of a DeclaredTheory constructed
 *  to make an unknown semantic entity (e.g., a model or an implementation) partially accessible to the syntax.
 *  
 *  It typically contains [[symbols.RealizedConstant]]s.
 */
class Realization(doc : DPath, name : LocalName, theory: MPath) extends DeclaredTheory(doc, name, None) {
   add(PlainInclude(theory, path))
}

/** adds all rules of Realization to the RuleStore */
class RealizationListener extends frontend.ChangeListener {
   override val logPrefix = "realization-listener"
   override def onAdd(e: ContentElement) {
       e match {
          case r: Realization =>
             r.getDeclarations.foreach {
                case rc: RealizedOperatorConstant =>
                   log("adding rule for " + rc.path)
                   controller.extman.ruleStore.add(rc.real.toRule(r.path))
                case _ =>
             }
          case _ =>
       }
   }
}