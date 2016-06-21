package info.kwarc.mmt.leo.AgentSystem.MMTSystem

import info.kwarc.mmt.leo.AgentSystem.{Agent, Change}

/**
 * Created by Mark on 7/23/2015.
 *
 * this class represents the structure for an Agent of the MMT system
 */

/*
class InductionAgent(tp: GlobalName, constructors: List[(GlobalName,Term)]) {
  x:tp in Context of goal g
  //split g on x
  val cases = constructors.map {case (c, FunType(args,_)) =>
      val newCon = FunType.argsAsContext(args)  makeFresh..., make sure all args named
      replace x:tp in context with newCon
      new subgoal built from g by substitute x with ApplySpine(OMS(c), newCon.map(OMS(_.name))) in context and goal
  }
  add cases as conjunctive subgoals to g
}
*/

abstract class MMTAgent(blackboardParam: MMTBlackboard) extends Agent(blackboardParam) {

  override val name: String = "MMTAgent"

  lazy val presentObj = blackboard.presentObj
  lazy val rules = blackboard.rules

  lazy val invertibleBackward = blackboard.invertibleBackward
  lazy val invertibleForward = blackboard.invertibleForward
  lazy val searchBackward = blackboard.searchBackward
  lazy val searchForward = blackboard.searchForward
  lazy val searchTerms = blackboard.searchTerms
  lazy val transitivityRules = blackboard.transitivityRules

  lazy val goalSection = blackboard.goalSection
  def goal = goalSection.data
  lazy val factSection = blackboard.factSection
  def facts = factSection.data

  override val blackboard:MMTBlackboard = blackboardParam
  if (blackboard!=null) {blackboard.registerAgent(this)}else{ print("WARNING NO Blackboard in MMTagent class")}
}

class SearchBackwardAgent(blackboard: MMTBlackboard) extends MMTAgent(blackboard) {
  val priority = 1

  override val name = "SearchBackwardAgent"

  def wantToSubscribeTo = List(blackboard.factSection)

  override val interests = List("ADD")


  def goal_recurse(g: Goal):Unit ={

    log("recursing at:" + g)
    // recurse into subgoals first so that we do not recurse into freshly-added goals
    g.getAlternatives.foreach { case Alternative(sgs, _) =>
      sgs.foreach { sg => goal_recurse(sg) }
      if (g.isSolved) return
    }

    addTask(g)
  }

  def addTask(g:Goal) = taskQueue.enqueue(new SearchBackwardTask(this,g))

  override def respond() = {
    log("responding to: " + mailbox.length + " message(s)")
    readMail.foreach{
      case Change(s,data,flag) if taskQueue.isEmpty => goal_recurse(blackboard.goalSection.data)
      case _ if blackboard.cycle==0 && taskQueue.isEmpty => goal_recurse(blackboard.goalSection.data)
      case _ =>
    }
    if (taskQueue.isEmpty) log("NO TASKS FOUND") else log("Found "+taskQueue.size+" task(s)")
  }




}

class SearchForwardAgent(blackboard: MMTBlackboard) extends MMTAgent(blackboard) {
  val priority = 0

  override val name =  "SearchForwardAgent"
  def wantToSubscribeTo = List(blackboard.factSection)
  override val interests = Nil

  def addTask() = taskQueue += new SearchForwardTask(this)

  override def respond() = {
    log("responding to: " + mailbox.length + " message(s)")
    if (!goal.isFinished) {addTask()}
    if (taskQueue.isEmpty) log("NO TASKS FOUND") else log("Found "+taskQueue.size+" task(s)")
  }

}


class TermGenerationAgent(blackboard: MMTBlackboard) extends MMTAgent(blackboard) {

  val priority = 0

  override val name =  "TermGeneratingAgent"
  def wantToSubscribeTo = List(blackboard.factSection)
  override val interests = List("ADD")

  def addTask() = taskQueue+=new TermGenerationTask(this)

  override def respond() = {
    log("responding to: " + mailbox.length + " message(s)")
    if (!goal.isFinished) {addTask()}
    if (taskQueue.isEmpty) log("NO TASKS FOUND") else log("Found "+taskQueue.size+" task(s)")
  }

}

class TransitivityAgent(blackboard: MMTBlackboard) extends MMTAgent(blackboard) {

  val priority = 0
  override val name =  "TransitivityAgent"
  def wantToSubscribeTo = List(blackboard.factSection)
  override val interests = List("ADD") //TODO make it interested in the addition of relation shaped facts
  val transitivitySection = blackboard.transitivitySection

  def addTask():Unit = taskQueue+=new TransitivityTask(this)

  override def respond() = {
    log("responding to: " + mailbox.length + " message(s)")
    if (!goal.isFinished) {addTask()}
    if (taskQueue.isEmpty) log("NO TASKS FOUND") else log("Found "+taskQueue.size+" task(s)")
  }

}




