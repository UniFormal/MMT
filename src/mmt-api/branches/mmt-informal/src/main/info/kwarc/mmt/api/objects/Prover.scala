package info.kwarc.mmt.api.objects
import info.kwarc.mmt.api._
import frontend._
import symbols._
import objects.Conversions._

class Prover(controller: Controller) {
   val introRules = controller.extman.ruleStore.introProvingRules
   val elimRules = controller.extman.ruleStore.elimProvingRules
   def applicable(goal: Term)(implicit stack: Stack) : List[ApplicableProvingRule] = {
      val head = goal.head.getOrElse(return Nil)

      // first look for all intro rules, if any return them
      val possibleIntro = introRules(head).toList flatMap {r => r(goal).toList}
      if (possibleIntro != Nil) return possibleIntro

      // if none, look for applicable elim rules by inspecting the current theory and context
      // axioms holds the list of applicable axiom rules, which are found along the way
      var axioms : List[ApplicableProvingRule] = Nil
      
      val possibleElimVar = stack.context flatMap {v =>
         val tp = v.tp.getOrElse(return Nil)
         if (tp == goal) axioms ::= axiomRule(v.toTerm) 
         val tpH = tp.head.getOrElse(return Nil)
         elimRules(tpH).toList flatMap {r => r(v.toTerm, tp, goal).toList}
      }
      val tpath = stack.theory.toMPath
      val decls = controller.localLookup.getDeclaredTheory(tpath).getConstants
      val possibleElimCon = decls flatMap {c =>
         val tp = c.tp.getOrElse(return Nil)
         if (tp == goal) axioms ::= axiomRule(c.toTerm)
         val tpH = tp.head.getOrElse(return Nil)
         elimRules(tpH).toList flatMap {r => r(c.toTerm, tp, goal).toList}
      }
      axioms.reverse ::: possibleElimVar ::: possibleElimCon
   }
   
   private def axiomRule(ax: Term) = new ApplicableProvingRule {
      def label = ax.toString
      def apply() = ax
   }
}
