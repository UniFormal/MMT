package info.kwarc.mmt.leo.AgentSystem.MMTSystem

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.leo.AgentSystem.{Blackboard, Section, Task}


/**
 * Created by Mark on 7/24/2015.
 *
 * Classes for the Expansion and Search tasks
 */
abstract class MMTTask(agent:MMTAgent)(implicit controller: Controller,oLP:String) extends Task {
  override val name: String = "GoalTask"

  override def logPrefix = oLP + "#"+name
  override val sentBy: MMTAgent = agent

  val proofSection = sentBy.blackboard.get.goalSection
  def goal = proofSection.data
  val factSection = sentBy.blackboard.get.factSection
  def facts = factSection.data
  val termSection = sentBy.blackboard.get.termSection
  def terms = termSection.data

  def presentObj = sentBy.presentObj

  def rules = sentBy.rules

}

abstract class GoalTask(agent:MMTAgent,g:Goal)(implicit controller: Controller,oLP:String) extends MMTTask(agent) {

  //For now give all tasks simplification abilities
  /** statefully changes g to a simpler goal */
  protected def simplifyGoal(g: Goal) {
    g.setConc(controller.simplifier(g.conc, g.fullContext, rules), facts)
    proofSection.passiveChange(g)
  }

  /** simplify a fact */
  protected def simplifyFact(f: Fact): Fact = {
    val tpS = controller.simplifier(f.tp, f.goal.fullContext, rules)
    f.copy(tp = tpS)
  }

  /** Returns a set of all nodes, that will be written by the task. */
  //TODO get write and read sets working with florian's data structures
  override def writeSet(s: Section): Set[s.ObjectType] = {
    if (s == proofSection) return Set(g.asInstanceOf[s.ObjectType])
    Set.empty[s.ObjectType]
  }

  /** Returns a set of all nodes that are read for the task. */
  override def readSet(s: Section): Set[s.ObjectType] = {
    if (s == proofSection) return Set(g.asInstanceOf[s.ObjectType])
    Set.empty[s.ObjectType]
  }

  override def toString:String = {
    name+" Goal: "+g.toString
  }
  /**
   * applies one tactic to a goal and expands the resulting subgoals
   * @return true if the tactic made any progress
   */
  protected def applyAndExpand(at: ApplicableTactic, g: Goal): Boolean = {
    val alt = at.apply().getOrElse(return false)
    // simplify the new goal
    alt.subgoals.foreach { sg =>
      sg.parent = Some(g) // need to set this before working with the goal
      simplifyGoal(sg)
    }

    // avoid cycles/redundancy: skip alternatives with subgoals that we already try to solve
    val path = g.path
    val alreadyOnPath = alt.subgoals.exists { sg =>
      // TODO stronger equality
      path.exists { ag => (ag.context hasheq sg.context) && (ag.conc hasheq sg.conc) }
    }
    if (alreadyOnPath )
      return false

    // add the alternative to the proof tree and expand the subgoals
    g.addAlternative(alt, Some(proofSection))
    alt.subgoals.foreach(terms.addVarAtoms) //TODO work into goal addition

    log("************************* " + at.label + " at X **************************")
    log("\n" + goal.presentHtml(0)(presentObj, Some(g), Some(alt)))
    if (!g.isSolved) {
      // recursively process subgoals
      alt.subgoals.foreach { sg => expand(sg) }
    }
    true
  }


  /** exhaustively applies invertible tactics to a goal */
  protected def expand(g: Goal) {
    g.setExpansionTactics(blackboard.get, agent.invertibleBackward, agent.invertibleForward)
    g.getNextExpansion match {
      case Some(at) =>
        // apply the next invertible tactic, if any
        val applicable = applyAndExpand(at, g)
        if (!applicable)
        // at not applicable, try next tactic
          expand(g)
      case None =>
        g.setSearchTactics(blackboard.get, agent.searchBackward)
    }
  }

}



case class SearchBackwardTask(agent:SearchBackwardAgent,g:Goal)(implicit controller: Controller,oLP:String) extends GoalTask(agent,g) {
  override val name="SearchBackwardTask"
  /** Determines if a given task is applicable given the current blackboard */


  private def backwardSearch(g: Goal) {
    log("recursing at:" + g)
    // recurse into subgoals first so that we do not recurse into freshly-added goals
    g.getAlternatives.foreach {case Alternative(sgs,_) =>
      sgs.foreach {sg => backwardSearch(sg)}
      if (g.isSolved) return
    }
    // backward search at g
    // new goals are expanded immediately and are subject to forward/backward search in the next iteration

    log("Backward Search at:" + g)
    log("Facts are:" + facts)
    val tactics=g.getNextSearch(blackboard.get)
    log("Backward tacics Are:" + tactics)
    tactics.foreach {at =>
      applyAndExpand(at, g)
      if (g.isSolved) return
    }
  }

  def execute()={
    if (blackboard.get.cycle==0){expand(g); g.isSolved}
    val out=backwardSearch(g)
    true
  }
}

case class SearchForwardTask(agent:SearchForwardAgent)(implicit controller: Controller,oLP:String) extends MMTTask(agent) {
  override def logPrefix=oLP+"#SearchForwardTask"

  /** Determines if a given task is applicable given the current blackboard */
  override def isApplicable[BB <: Blackboard](b: BB): Boolean = super.isApplicable(b) && !goal.isSolved


  override def writeSet(s: Section): Set[s.ObjectType] = {
    Set.empty[s.ObjectType]
  }

  /** Returns a set of all nodes that are read for the task. */
  override def readSet(s: Section): Set[s.ObjectType] = {
    Set.empty[s.ObjectType]
  }

  def forwardSearch() {
    log("Performing forward search")
    agent.searchForward.foreach {e =>
      e.generate(blackboard.get,interactive = false)
    }
    facts.integrateFutureFacts(Some(factSection))
  }

  def execute()={
    log("Forward Search Executing")
    forwardSearch()
    log("Integrating new facts")
    goal.newFacts(facts)
    true
  }

}

abstract class NormalizingTask(agent:NormalizingAgent,g:Goal)(implicit controller: Controller,oLP:String) extends GoalTask(agent,g){

}


class TermGenerationTask(agent:TermGenerationAgent)(implicit controller: Controller,oLP:String) extends MMTTask(agent) {

  /** Determines if a given task is applicable given the current blackboard */
  override def isApplicable[BB <: Blackboard](b: BB): Boolean = !b.finished

  def termSearch() {
    agent.searchTerms.foreach { e =>
      e.generate(blackboard.get, interactive = false)
    }
  }

  def execute() = {
    log("Term Search Executing")
    termSearch()
    true
  }

  /** Returns a set of all nodes, that will be written by the task. */
  override def writeSet(s: Section): Set[s.ObjectType] = {
    if (s == blackboard.get.termSection) {
      Set(terms.asInstanceOf[s.ObjectType])
    } else {
      Set.empty[s.ObjectType]
    }
  }

  /** Returns a set of all nodes that are read for the task. */
  override def readSet(s: Section): Set[s.ObjectType] ={
    if (s == factSection) {
      Set(terms.asInstanceOf[s.ObjectType])
    } else {
      writeSet(s).asInstanceOf[Set[s.ObjectType]]
    }
  }

}

class TransitivityTask(agent:TransitivityAgent)(implicit controller: Controller,oLP:String) extends MMTTask(agent) {

  /** Determines if a given task is applicable given the current blackboard */
  override def isApplicable[BB <: Blackboard](b: BB): Boolean = !b.finished //TODO expand this

  def transitivitySearch() {
    agent.transitivityRules.foreach { e =>
      e.generate(blackboard.get, interactive = false)
    }
  }

  def execute() = {
    log("Term Search Executing")
    transitivitySearch()
    true
  }

  /** Returns a set of all nodes, that will be written by the task. */
  override def writeSet(s: Section): Set[s.ObjectType] = {
    if (s==blackboard.get.transitivitySection) {
      Set(blackboard.get.transitivitySection.data.asInstanceOf[s.ObjectType])
    }else{
      Set.empty[s.ObjectType]
    }
  }

  /** Returns a set of all nodes that are read for the task. */
  override def readSet(s: Section): Set[s.ObjectType] = writeSet(s).asInstanceOf[Set[s.ObjectType]]
}