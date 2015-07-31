package info.kwarc.mmt.leo.AgentSystem.GoalSystem

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.leo.AgentSystem.{Change, Listener, Agent}

/**
 * Created by Mark on 7/23/2015.
 */


abstract class GoalAgent(implicit controller: Controller,oLP:String) extends Agent {

  override val interests = List("ADD","CHANGE")
  override type BBType = GoalBlackboard


  override val name: String = "GoalAgent"
  override var subscribers: List[Listener] = Nil

  lazy val presentObj = blackboard.get.presentObj
  lazy val rules = blackboard.get.rules

  lazy val invertibleBackward = blackboard.get.invertibleBackward
  lazy val invertibleForward = blackboard.get.invertibleForward
  lazy val searchBackward = blackboard.get.searchBackward
  lazy val searchForward = blackboard.get.searchForward

  lazy val proofSection = blackboard.get.proofSection
  def goal = proofSection.data
  lazy val factSection = blackboard.get.factSection
  def facts = factSection.data

  def ignoreGoal(node:Goal):Boolean

}

class SearchBackwardAgent(implicit controller: Controller,oLP:String) extends GoalAgent {
  override val priority=1
  override val name =  "SearchBackwardAgent"
  override val interests = List("ADD")

  def ignoreGoal(g:Goal) = false
  def addTask(g:Goal) = taskSet+=new SearchBackwardTask(this,g)

  override def respond() = {
    log("responding to: " + mailbox.length + " message(s)")
    readMail.foreach{
      case Change(s,data,flag) if blackboard.get.cycle==0 || s==factSection=> addTask(blackboard.get.proofSection.data)
      case _ =>
    }
    if (taskSet.isEmpty) log("NO TASKS FOUND") else log("Found "+taskSet.size+" task(s)")
  }
}

class SearchForwardAgent(implicit controller: Controller,oLP:String) extends GoalAgent {
  override val name =  "SearchForwardAgent"
  override val interests = Nil

  def ignoreGoal(g:Goal) = false
  def addTask() = taskSet+=new SearchForwardTask(this)

  override def respond() = {
    log("responding to: " + mailbox.length + " message(s)")
    if (!goal.isFinished) {addTask()}
    if (taskSet.isEmpty) log("NO TASKS FOUND") else log("Found "+taskSet.size+" task(s)")
  }

}

abstract class SimplifyingAgent(implicit controller: Controller,oLP:String) extends GoalAgent {
}




