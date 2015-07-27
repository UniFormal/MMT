package info.kwarc.mmt.leo.AgentSystem.GoalSystem

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.leo.AgentSystem.{Blackboard, Section, Task}

/**
 * Created by Mark on 7/24/2015.
 *
 * Classes for the Expansion and Search tasks
 */
abstract class GoalTask(agent:GoalAgent,g:Goal)(implicit controller: Controller) extends Task {
  override val name: String = "GoalTask"

  override val sentBy:GoalAgent = agent

  val proofSection = sentBy.blackboard.get.proofSection
  val factSection = sentBy.blackboard.get.factSection

  def presentObj = sentBy.presentObj

  //def report = sentBy.report


  def rules = sentBy.rules

  /** Determines if a given task is applicable given the current blackboard */
  override def isApplicable[BB <: Blackboard](b: BB): Boolean = !sentBy.ignoreGoal(g) //TODO expand this

  //For now give all tasks simplification abilities
  /** statefully changes g to a simpler goal */
  protected def simplifyGoal(g: Goal) {
    g.setConc(controller.simplifier(g.conc, g.fullContext, rules),factSection.data)
  }

  /** simplify a fact */
  protected def simplifyFact(f: Fact): Fact = {
    val tpS = controller.simplifier(f.tp, f.goal.fullContext, rules)
    f.copy(tp = tpS)
  }


  /** Returns a set of all nodes, that will be written by the task. */
  //TODO get write and read sets working with florian's data structures
  override def writeSet(s: Section): Set[s.ObjectType] = {
    if (s==proofSection) Set(g.asInstanceOf[s.ObjectType])
    Set.empty[s.ObjectType]
  }

  /** Returns a set of all nodes that are read for the task. */
  override def readSet(s: Section): Set[s.ObjectType] = {
    if (s == proofSection) Set(g.asInstanceOf[s.ObjectType])
    Set.empty[s.ObjectType]
  }

  /**
   * applies one tactic to a goal and expands the resulting subgoals
   * @return the generated alternative if the tactic succeded
   */
  def applyApplicableTactic(at: ApplicableTactic, g: Goal): Option[Alternative] = {
    val alt = at.apply().getOrElse(return None)
    // simplify the new goal
    alt.subgoals.foreach { sg =>
      sg.parent = Some(g) // need to set this before working with the goal
      simplifyGoal(sg)
    }

    //TODO can work this into an isApplicable
    // avoid cycles/redundancy: skip alternatives with subgoals that we already try to solve
    val path = g.path
    val alreadyOnPath = alt.subgoals.exists { sg =>
      // TODO stronger equality
      path.exists { ag => (ag.context hasheq sg.context) && (ag.conc hasheq sg.conc) }
    }
    if (alreadyOnPath)
    //TODO add a labeling system here to prevent recalculation of redundant tactics
      return None

    // add the alternative to the proof tree and expand the subgoals
    g.addAlternative(alt,Some(proofSection))
    log("************************* " + at.label + " at X **************************")
    log("\n" + g.presentHtml(0)(presentObj, Some(g), Some(alt)))
    Some(alt)
  }

}

class ExpansionTask(agent:GoalAgent,g:Goal)(implicit controller: Controller) extends GoalTask(agent,g) {
  override val name="ExpansionTask"

  /** applies invertible tactics to a goal and returns an alternative if an application was successful*/
  protected def expand(g: Goal,backw: List[BackwardInvertible], forw: List[ForwardInvertible]):Option[Alternative]= {
    g.setExpansionTactics(blackboard.get, backw, forw)
    g.getNextExpansion match {
      case Some(at) =>
        // apply the next invertible tactic, if any
        var newAlt = applyApplicableTactic(at, g)
        val applicable = newAlt.isDefined
        if (! applicable)
        // at not applicable, try next tactic
          newAlt=expand(g,backw,forw)
        newAlt
      case None => None
    }
  }

  /**apply invertible tactics until one works and repeat for generated alternative
    * direction = None => no direction, direction = Some(true)=> backwards, else forwards*/
  def fullExpand(g:Goal,backw:List[BackwardInvertible],forw: List[ForwardInvertible]):Boolean={
    val progress = expand(g,backw,forw)

    progress match {
      case Some(p) => p.subgoals.exists(fullExpand(_,backw,forw))
      case None =>
        if (backw.nonEmpty) {g.isBackwardExpanded=true}  //TODO be aware of this when rules are not full rules
        if (forw.nonEmpty) {g.isForwardExpanded=true}
        false
    }
  }


  def execute() ={
    fullExpand(g,agent.invertibleBackward,agent.invertibleForward)
  }

}

/**Expands Goal by applying invertible backwards rules to it then applying them again to all newly generated leaves*/
class InvertibleBackwardTask(agent:InvertibleBackwardAgent,g:Goal)(implicit controller: Controller) extends ExpansionTask(agent,g) {
  override val name="InvertibleBackwardTask"
  override def execute() ={
    fullExpand(g,agent.invertibleBackward,Nil) //TODO fix inheritance
  }

}

/**Expands Goal by applying invertible backwards rules to it then applying them again to all newly generated leaves*/
class InvertibleForwardTask(agent:InvertibleForwardAgent,g:Goal)(implicit controller: Controller) extends ExpansionTask(agent,g) {
  override val name="InvertibleForwardTask"
  override def execute() ={
    fullExpand(g,Nil,agent.invertibleForward)
  }

}

class SearchBackwardTask(agent:SearchBackwardAgent,g:Goal)(implicit controller: Controller) extends GoalTask(agent,g) {
  override val name="SearchBackwardTask"
  /** Determines if a given task is applicable given the current blackboard */


  /**
   * applies backward search to all fully expanded goals
   * @param g the goal to apply tactics to
   */
  private def backwardSearch(g: Goal):Boolean= {
    g.setSearchTactics(blackboard.get, agent.searchBackward)
    g.isBackwardSearched = true
    // backward search at g
    // new goals are expanded immediately and are subject to forward/backward search in the next iteration
    val tactics=g.getNextSearch(blackboard.get)
    if (tactics.isEmpty){return false}
    tactics.foreach {at =>
      applyApplicableTactic(at, g)
      if (g.isSolved) return true
    }
    true
  }

  def execute()={
    backwardSearch(g)
  }
}

class SearchForwardTask(agent:SearchForwardAgent,g:Goal)(implicit controller: Controller) extends GoalTask(agent,g) {

  def forwardSearch() {
    g.isForwardSearched = true
    agent.searchForward.foreach {e =>
      e.generate(blackboard.get,interactive = false)
    }
    blackboard.get.facts.integrateFutureFacts()
    //TODO make sure integration of future facts works with agent system
  }

  def execute()={
    forwardSearch()
    true
  }

}