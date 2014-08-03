package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api._
import symbols._
import objects._
import uom._

/*


/** A Realization is a special case of a DeclaredTheory constructed
 *  to make an unknown semantic entity (e.g., a model or an implementation) partially accessible to the syntax.
 *  
 *  It typically contains [[symbols.RealizedConstant]]s.
 *  
 *  @param theory the theory that is modeled or implemented
 */
class Realization(doc : DPath, name : LocalName, theory: MPath) extends DeclaredTheory(doc, name, None) {
   add(PlainInclude(theory, path))
}

/** adds all rules of Realization to the RuleStore */
class RealizationListener extends frontend.ChangeListener {
   /** all rules added by this listener (used for deleting rules) */
   private var rules: List[UOMRule] = Nil
   override val logPrefix = "realization-listener"
   override def onAdd(e: ContentElement) {
       e match {
          case r: Realization =>
             r.getDeclarations.foreach {
                case rc: RealizedOperatorConstant =>
                   log("adding rule for " + rc.path)
                   val rule = rc.real.toRule(r.path)
                   rules ::= rule
                   controller.extman.ruleStore.add(rule)
                case _ =>
             }
          case _ =>
       }
   }
   override def onDelete(p: Path) {
      p match {
         case p: MPath =>
            val toBeDeleted = rules.filter(r => r.parent == OMMOD(p))
            if (toBeDeleted != Nil) {
               controller.extman.ruleStore.delete {r => toBeDeleted contains r}
               rules = rules filterNot {r => toBeDeleted contains r}
            }
         case _ => None
      }
   }
   override def onClear {
      controller.extman.ruleStore.delete {r => rules contains r}
      rules = Nil
   }
}*/