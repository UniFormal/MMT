
package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.MMTSystem

import info.kwarc.mmt.api.{Active, modules}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.leo.AgentSystem.Agent
/*
/**
 * Created by Mark on 7/22/2015.
 *
 * this holds the main implementations of the prover as a set of agents
 */
class LFAgent extends Agent{
  type BlackboardType = LFBlackboard

  def controller = blackboard.get.controller
  val rules = blackboard.get.rules
  def goal = blackboard.get.proofTree

  val report = controller.report
  override def logPrefix = "LFAgent"

  implicit val presentObj: Obj => String = o => controller.presenter.asString(o)

  private val invertibleBackward = rules.get(classOf[BackwardInvertible]).toList
  private val invertibleForward  = rules.get(classOf[ForwardInvertible]).toList
  private val searchBackward     = rules.get(classOf[BackwardSearch]).toList.sortBy(_.priority).reverse
  private val searchForward      = rules.get(classOf[ForwardSearch]).toList

  implicit val facts = new Facts(this.blackboard.get, 2)

  private def initFacts() {
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
    initFacts()
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
    initFacts()
    // apply all backward rules one step
    val backwardOptions = {
      val i = invertibleBackward.flatMap {r => r(this.blackboard.get, goal).toList}
      val s = searchBackward.flatMap {r => r(this.blackboard.get, goal)}
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
      e.generate(this.blackboard.get, interactive)
    }
    facts.integrateFutureFacts()
  }

  /**
   * applies backward search to all fully expanded goals
   * @param g the goal to apply tactics to
   */
  private def backwardSearch(g: LFProofTree) {
    // recurse into subgoals first so that we do not recurse into freshly-added goals
    g.children.foreach {sg => backwardSearch(sg)}
    if (g.isSolved) return


    // backward search at g
    // new goals are expanded immediately and are subject to forward/backward search in the next iteration
    g.getNextSearch(this.blackboard.get).foreach {at =>
      applyAndExpand(at, g)
      if (g.isSolved) return
    }
  }

  /** statefully changes g to a simpler goal */
  private def simplifyGoal(g: LFProofTree) {
    g.setConc(controller.simplifier(g.conc, g.fullContext, rules))
  }
  /** simplify a fact */
  def simplifyFact(f: Fact): Fact = {
    val tpS = controller.simplifier(f.tp, f.goal.fullContext, rules)
    f.copy(tp = tpS)
  }

  /**
   * applies one tactic to a goal and expands the resulting subgoals
   * @return true if the tactic made any progress
   */
  private def applyAndExpand(at: ApplicableTactic, g: LFProofTree): Boolean = {
    val alt = at.apply().getOrElse(return false)
    // simplify the new goal
    alt.children.foreach {sg =>
      sg.parent = Some(g) // need to set this before working with the goal
      simplifyGoal(sg)
    }

    // avoid cycles/redundancy: skip alternatives with subgoals that we already try to solve
    val path = g.path
    val alreadyOnPath = alt.children.exists {sg =>
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
        val applicable = applyAndExpand(at, g)
        if (! applicable)
        // at not applicable, try next tactic
          expand(g)
      case None =>
        g.setSearchTactics(this, searchBackward)
    }
  }

}

*/
