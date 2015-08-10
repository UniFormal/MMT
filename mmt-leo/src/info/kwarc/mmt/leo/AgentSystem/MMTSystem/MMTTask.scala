package info.kwarc.mmt.leo.AgentSystem.MMTSystem

import info.kwarc.mmt.api.frontend.{Logger, Controller}
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

  val goalSection = sentBy.blackboard.get.goalSection
  def goal = goalSection.data
  val factSection = sentBy.blackboard.get.factSection
  def facts = factSection.data
  val termSection = sentBy.blackboard.get.termSection
  def terms = termSection.data

  def rules = sentBy.rules

}

abstract class GoalTask(agent:MMTAgent,g:Goal)(implicit controller: Controller,oLP:String) extends MMTTask(agent) {


  /** Returns a set of all nodes, that will be written by the task. */
  //TODO get write and read sets working with florian's data structures
  override def writeSet(s: Section): Set[s.ObjectType] = {
    if (s == goalSection) return Set(g.asInstanceOf[s.ObjectType])
    Set.empty[s.ObjectType]
  }

  /** Returns a set of all nodes that are read for the task. */
  override def readSet(s: Section): Set[s.ObjectType] = {
    if (s == goalSection) return Set(g.asInstanceOf[s.ObjectType])
    Set.empty[s.ObjectType]
  }

  override def toString:String = {
    name+" Goal: "+g.toString
  }

}


case class SearchBackwardTask(agent:SearchBackwardAgent,g:Goal)(implicit controller: Controller,oLP:String) extends GoalTask(agent,g) {
  override val name="SearchBackwardTask"
  /** Determines if a given task is applicable given the current blackboard */


  private def backwardSearch(g: Goal) {
    // backward search at g
    // new goals are expanded immediately and are subject to forward/backward search in the next iteration

    log("Backward Search at:" + g)
    val tactics=g.getNextSearch(blackboard.get)
    log("Backward tactics Are:" + tactics)
    tactics.foreach {at =>
      log("Applying Search Tactic:"+at)
      goalSection.applyAndExpand(at, g)
      if (g.isSolved) return
    }
  }

  def execute()={
    backwardSearch(g)
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

  /** simplify a fact */
  protected def simplifyFact(f: Fact): Fact = {
    val tpS = controller.simplifier(f.tp, f.goal.fullContext, rules)
    f.copy(tp = tpS)
  }

  def forwardSearch() {
    log("Performing forward search")
    agent.searchForward.foreach {e =>
      e.generate(blackboard.get,interactive = false)
    }
    facts.integrateFutureFacts(Some(factSection))
    log("Finished Search, facts are:  \n"+facts)
  }

  def execute()={
    forwardSearch()
    goal.newFacts(facts)
    true
  }

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

/*  def transitivitySearch() {
    agent.transitivityRules.foreach { e =>
      e.generate(blackboard.get, interactive = false)
    }
  }*/

  def execute() = {
    log("Term Search Executing")
    //transitivitySearch()
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