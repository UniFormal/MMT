package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import checking._
import objects._
import frontend._
import symbols._
import objects.Conversions._
import utils._

/**
 * A prover conducts the proof search. A new instance is created for each proof obligation.
 * 
 * @param goal the goal to prove
 * @param intros the backward tactics to use
 * @param elims  the forward  tactics to use
 * 
 * A prover greedily applies invertible tactics to each new goal (called the expansion phase).
 * Then forward and backward breadth-first searches are performed in parallel.
 */
class P(solver: Solver, val goal: Goal, rules: RuleSet) extends Logger {
   val report = solver.report
   def logPrefix = solver.logPrefix
   
   private implicit val presentObj = solver.presentObj
   
   private val invertibleBackward = rules.get(classOf[BackwardInvertible]).toList
   private val invertibleForward  = rules.get(classOf[ForwardInvertible]).toList
   private val searchBackward     = rules.get(classOf[BackwardSearch]).toList.sortBy(_.priority).reverse
   private val searchForward      = rules.get(classOf[ForwardSearch]).toList
   
   val facts = new FactsDB(2)
   private def initFacts {
      val imports = solver.controller.library.visibleDirect(ComplexTheory(goal.context))
      imports.foreach {
         case OMPMOD(p,_) =>
            solver.controller.globalLookup.getO(p) match {
               case Some(t:modules.DeclaredTheory) =>
                  t.getDeclarations.foreach {
                     case c: Constant if c.status == Active => c.tp.foreach {t =>
                        facts.addSymbolAtom(c.toTerm, t)
                     }
                     case _ =>
                  }
               case _ =>
            }
         case _ =>
      }
   }
   
   /**
    * tries to solve the goal
    * @param levels the depth of the breadth-first searches
    * @return true if the goal was solved
    */
   def apply(levels: Int): Boolean = {
      if (goal.isSolved) return true
      initFacts
      expand(goal)
      if (goal.isSolved) return true
      search(levels)
      goal.isSolved
   }
   
   private def search(levels: Int) {
      if (levels == 0) return
      backwardSearch(goal, levels)
      // forward search at all goals
      searchForward.foreach {e =>
         e.generate(this)
      }
      facts.integrateFutureFacts
      goal.newFacts(facts)
      if (goal.isSolved) return
      search(levels-1)
   }
   
   /**
    * applies backward search to all fully expanded goals
    * @param g the goal to apply tactics to
    * @param levels the depth of the breadth-first searches 
    */
   private def backwardSearch(g: Goal, levels: Int) {
      // recurse into subgoals first so that we do not recurse into freshly-added goals
      g.getAlternatives.foreach {case Alternative(sgs,_) =>
         sgs.foreach {ng => backwardSearch(ng,levels)}
         if (g.isSolved) return
      }
      // backward search at g
      // new goals are expanded immediately and are subject to forward/backward search in the next iteration
      g.getNextSearch(this).foreach {at =>
         applyAndExpand(at, g)
         if (g.isSolved) return
      }
   }   

   /**
    * applies one tactic to a goal and expands the resulting subgoals
    * @return true if the tactic made any progress
    */
   private def applyAndExpand(at: ApplicableTactic, g: Goal): Boolean = {
      val altOpt = at.apply()
      altOpt foreach {alt =>
         g.addAlternative(alt)
         log("************************* " + at.label + " at X **************************")
         log("\n" + goal.present(0)(presentObj, Some(g), Some(alt)))
         if (!g.isSolved) {
            // recursively process subgoals
            alt.subgoals.foreach {sg => expand(sg)}
         }
      }
      altOpt.isDefined
   }
   /** exhaustively applies invertible tactics to a goal */
   private def expand(g: Goal) {
      g.setExpansionTactics(this, invertibleBackward, invertibleForward)
      g.getNextExpansion match {
         case Some(at) =>
            // apply the next invertible tactic, if any
            val applicable = applyAndExpand(at, g)
            if (! applicable)
              // at not applicable, try next tactic
              expand(g)
         case None =>
            g.setSearchTactics(this, searchBackward)
      }
   }
}

class Prover(controller: Controller) {
   
   def getIntroRules(goal: Term)(implicit stack: Stack,rules: RuleSet) : List[ApplicableProvingRule] = {
      rules.get(classOf[IntroProvingRule]).filter(_.applicable(goal)).flatMap {r => r(goal).toList}.toList
   }
   
   def applicable(goal: Term)(implicit stack: Stack,rules: RuleSet) : List[ApplicableProvingRule] = {
      // first look for all intro rules, if any return them
      val possibleIntro = getIntroRules(goal)
      if (possibleIntro != Nil) return possibleIntro

      // if none, look for applicable elim rules by inspecting the current theory and context
      // axioms holds the list of applicable axiom rules, which are found along the way
      var axioms : List[ApplicableProvingRule] = Nil
      val elimProvingRules = rules.get(classOf[ElimProvingRule]).toList
      def doType(src: Term, tpOpt: Option[Term]) : List[ApplicableProvingRule] = {
            val tp = tpOpt.getOrElse(return Nil)
            if (tp == goal) axioms ::= axiomRule(src) 
            val tpH = tp.head.getOrElse(return Nil)
            elimProvingRules.filter(_.head == tpH).toList flatMap {r => r(src, tp, goal).toList}
      }
      val possibleElim = stack.context flatMap {
         case IncludeVarDecl(p,_) =>
            val decls = controller.localLookup.getDeclaredTheory(p).getConstants
            decls flatMap {c => doType(c.toTerm, c.tp)}
         case vd => doType(vd.toTerm, vd.tp)
      }
      axioms.reverse ::: possibleElim
   }
   
   private def axiomRule(ax: Term) = new ApplicableProvingRule {
      def label = ax.toString
      def apply() = ax
   }
}

object Prover {
   def indent(depth: Int) = (0 to depth).map(_ => "  ").mkString("")
}