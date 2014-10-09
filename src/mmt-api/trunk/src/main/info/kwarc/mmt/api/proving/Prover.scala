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
class Prover(controller: Controller, val goal: Goal, rules: RuleSet, outerLogPrefix: String) extends Logger {
   val report = controller.report
   def logPrefix = outerLogPrefix + "/prover"
   
   implicit val presentObj: Obj => String = o => controller.presenter.asString(o)
   
   private val invertibleBackward = rules.get(classOf[BackwardInvertible]).toList
   private val invertibleForward  = rules.get(classOf[ForwardInvertible]).toList
   private val searchBackward     = rules.get(classOf[BackwardSearch]).toList.sortBy(_.priority).reverse
   private val searchForward      = rules.get(classOf[ForwardSearch]).toList
   
   implicit val facts = new Facts(this, 2, outerLogPrefix)
   private def initFacts {
      val imports = controller.library.visibleDirect(ComplexTheory(goal.context))
      imports.foreach {
         case OMPMOD(p,_) =>
            controller.globalLookup.getO(p) match {
               case Some(t:modules.DeclaredTheory) =>
                  t.getDeclarations.foreach {
                     case c: Constant if c.status == Active => c.tp.foreach {tp =>
                        val a = Atom(c.toTerm, tp, c.rl)
                        facts.addConstantAtom(a)
                     }
                     case _ =>
                  }
               case _ =>
            }
         case _ =>
      }
   }

   /** convenience function to create a matcher in the current situation */
   def makeMatcher(context: Context, queryVars: Context) = new Matcher(controller, rules, context, queryVars)

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
   
   /**
    * a list of possible steps to be used in an interactive proof
    * @param levels the search depth for forward search 
    * @return a list of possible solutions (possibly with holes)
    */
   def interactive(levels: Int): List[Term] = {
      initFacts
      // apply all backward rules one step
      val backwardOptions = {
         val i = invertibleBackward.flatMap {r => r(this, goal).toList}
         val s = searchBackward.flatMap {r => r(this, goal)}
         (i ::: s).flatMap(_.apply().map(_.proof()).toList)
      }
      // apply all forward rules according to levels
      Range(0,levels) foreach {_ => forwardSearch(true)}
      val forwardOptions = facts.solutionsOfGoal(goal)
      (forwardOptions ::: backwardOptions).distinct
   }
   
   private def search(levels: Int) {
      if (levels == 0) return
      backwardSearch(goal)
      // forward search at all goals
      forwardSearch(false)
      goal.newFacts(facts)     
      if (goal.isSolved) return
      search(levels-1)
   }
   
   private def forwardSearch(interactive: Boolean) {
      searchForward.foreach {e =>
         e.generate(this, interactive)
      }
      facts.integrateFutureFacts
   }
   
   /**
    * applies backward search to all fully expanded goals
    * @param g the goal to apply tactics to
    */
   private def backwardSearch(g: Goal) {
      // recurse into subgoals first so that we do not recurse into freshly-added goals
      g.getAlternatives.foreach {case Alternative(sgs,_) =>
         sgs.foreach {sg => backwardSearch(sg)}
         if (g.isSolved) return
      }
      // backward search at g
      // new goals are expanded immediately and are subject to forward/backward search in the next iteration
      g.getNextSearch(this).foreach {at =>
         applyAndExpand(at, g)
         if (g.isSolved) return
      }
   }
   
   /** statefully changes g to a simpler goal */
   private def simplifyGoal(g: Goal) {
      g.setConc(controller.simplifier(g.conc, g.fullContext, rules))
   }
   /** simplify a fact */
   private[proving] def simplifyFact(f: Fact): Fact = {
      val tpS = controller.simplifier(f.tp, f.goal.fullContext, rules)
      f.copy(tp = tpS)
   }

   /**
    * applies one tactic to a goal and expands the resulting subgoals
    * @return true if the tactic made any progress
    */
   private def applyAndExpand(at: ApplicableTactic, g: Goal): Boolean = {
      val alt = at.apply().getOrElse(return false)
      // simplify the new goal
      alt.subgoals.foreach {sg =>
         sg.parent = Some(g) // need to set this before working with the goal
         simplifyGoal(sg)
      }
      // avoid cycles/redundancy: skip alternatives with subgoals that we already try to solve
      val path = g.path
      val alreadyOnPath = alt.subgoals.exists {sg =>
         // TODO stronger equality
         path.exists {ag => (ag.context hasheq sg.context) && (ag.conc hasheq sg.conc)}
      }
      if (alreadyOnPath)
         return false
      // add the alternative to the proof tree and expand the subgoals
      g.addAlternative(alt)
      log("************************* " + at.label + " at X **************************")
      log("\n" + goal.present(0)(presentObj, Some(g), Some(alt)))
      if (!g.isSolved) {
         // recursively process subgoals
         alt.subgoals.foreach {sg => expand(sg)}
      }
      true
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

object Prover {
   def indent(depth: Int) = (0 to depth).map(_ => "  ").mkString("")
}