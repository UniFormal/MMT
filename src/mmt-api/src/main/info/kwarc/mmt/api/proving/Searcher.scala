package info.kwarc.mmt.api.proving

import info.kwarc.mmt.api._
import checking._
import modules._
import objects._
import frontend._
import info.kwarc.mmt.api.uom.SimplificationUnit
import symbols._
import objects.Conversions._
import utils._

/**
 * Conducts proof search, e.g., as used by the [[RuleBasedProver]].
 * A new instance must be created for each proof obligation.
 *
 * @param controller the MMT controller to use for lookups, etc.
 * @param goal the goal to prove
 * @param rules the rules to use
 * @param provingUnit the proof task
 *
 * The Searcher works in two modes:
 *  first, it greedily applies invertible tactics to each new goal (called the expansion phase)
 *  second, forward and backward breadth-first searches are performed in parallel
 */
class Searcher(controller: Controller, val goal: Goal, rules: RuleSet, provingUnit: ProvingUnit) extends Logger {
   val report = controller.report
   def logPrefix = provingUnit.logPrefix + "#prover"

   implicit val presentObj: Obj => String = o => controller.presenter.asString(o)

   private val invertibleBackward = rules.getOrdered(classOf[BackwardInvertible])
   private val invertibleForward  = rules.getOrdered(classOf[ForwardInvertible])
   private val searchBackward     = rules.getOrdered(classOf[BackwardSearch])
   private val searchForward      = rules.getOrdered(classOf[ForwardSearch])

   implicit val facts = new Facts(this, 2, provingUnit.logPrefix)

   private def doTheory(t : Theory) = {
     t.getDeclarations.foreach {
        case c: Constant if !UncheckedElement.is(c) => c.tp.foreach { tp =>
           val a = Atom(c.toTerm, tp, c.rl)
           facts.addConstantAtom(a)
        }
        case _ =>
     }
   }
   private def getTheory(tm : Term) = {
      val mpath = provingUnit.component.flatMap(_.parent match {
         case mp : MPath => Some(mp)
         case gn : GlobalName => Some(gn.module)
         case _ => None
      })
      controller.simplifier.materialize(provingUnit.context,tm,mpath,None) match {
         case dt : Theory =>
            Some(dt)
         case _ =>
            None
      }
   }
   private def doTerm(t : Term) : Unit = t match {
      case tm@OMPMOD(p,_) =>
         getTheory(tm).foreach(doTheory)
      case ComplexTheory(body) => body foreach (v => v.tp match {
         case Some(tm @ OMPMOD(p,_)) => getTheory(tm).foreach(doTheory)
         case Some(tm) => facts.addConstantAtom(Atom(v.toTerm,tm,None))
         case _ =>
      })
      case _ =>
   }
   private def initFacts {
      val imports = controller.library.visibleDirect(ComplexTheory(goal.context))
      imports.map(OMMOD(_)).foreach(doTerm)
      // atoms from variables are collected during the first round of forward search (somewhat weirdly, by ForwardPiElimination)
      log("Initialized facts are:  \n"+facts)
   }

   /** convenience function to create a matcher in the current situation */
   def makeMatcher = new Matcher(controller, rules)

   /**
    * tries to solve the goal
    *
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
    *
    * @param levels the search depth for forward search
    * @return a list of possible solutions (possibly with holes)
    */
   def interactive(levels: Int): List[Term] = {
      initFacts
      // apply all backward rules one step
      val backwardOptions = {
         val i = invertibleBackward.flatMap {r => r(this, goal).toList}
         val s = searchBackward.flatMap {r => r(this, goal)}
         (i ::: s).flatMap {r =>
           r.apply().map(_.proof()).toList
         }
      }
      // apply all forward rules according to levels
      Range(0,levels) foreach {_ => forwardSearch(true)}
      val forwardOptions = facts.solutionsOfGoal(goal)
      (forwardOptions ::: backwardOptions).distinct
   }

   /* TODO
    * currently constantAtoms become facts only in ForardPiElimination (the motivation being that only non-functional atoms should be stored as facts)
    * therefore, the first time backward search is tried, the necessary facts are not present yet
    * but after one try, backward search rules of a goal are not tried again even if new facts become available at that goal
    * therefore, many proofs fails because the critical first step is never made
    */
   
   private def search(levels: Int) {
      if (provingUnit.isKilled) {
        return
      }
      if (levels == 0) return
      backwardSearch(goal)
      // forward search at all goals
      forwardSearch(false)
      goal.newFacts(facts)
      if (goal.isSolved) return
      search(levels-1)
   }

   private def forwardSearch(interactive: Boolean) {
      log("Performing forward search")
      searchForward.foreach {e =>
         e.generate(this, interactive)
      }
      facts.integrateFutureFacts
      log("Finished Search, facts are:  \n"+facts)
   }

   /**
    * applies backward search to all fully expanded goals
     *
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
      g.setConc(controller.simplifier(g.conc, SimplificationUnit(g.fullContext, false,false, true), rules))
   }
   /** simplify a fact */
   private[proving] def simplifyFact(f: Fact): Fact = {
      val tpS = controller.simplifier(f.tp, SimplificationUnit(f.goal.fullContext, false,false, true), rules)
      f.copy(tp = tpS)
   }

   /**
    * applies one tactic to a goal and expands the resulting subgoals
     *
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
      log("\n" + goal.presentHtml(0)(presentObj, Some(g), Some(alt)))
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
            log(g.conc)
            val applicable = applyAndExpand(at, g)
            if (! applicable)
              // at not applicable, try next tactic
              expand(g)
         case None =>
            g.setSearchTactics(this, searchBackward)
      }
   }
}

object Searcher {
   def indent(depth: Int) = (0 to depth).map(_ => "  ").mkString("")
}
