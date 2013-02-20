package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import frontend._

class Prover(controller: Controller) {
   val rules = controller.extman.ruleStore.provingRules
   def applicable(t: Term)(implicit stack: Stack) : List[ApplicableProvingRule] = {
      val head = t.head.getOrElse(return Nil)
      val possible = rules(head).toList
      possible.flatMap(r => r(t).toList)
   }
}