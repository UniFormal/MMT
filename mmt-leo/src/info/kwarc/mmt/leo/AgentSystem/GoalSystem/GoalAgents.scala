package info.kwarc.mmt.leo.AgentSystem.GoalSystem

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.leo.AgentSystem.{Change, Listener, Agent}

/**
 * Created by Mark on 7/23/2015.
 */


abstract class GoalAgent(implicit controller: Controller) extends Agent {
  override val interests = List("ADD","CHANGE")
  override type BBType = GoalBlackboard

  //override def respond(): Unit = ???

  override def logPrefix = "GoalAgent"

  override val name: String = "GoalAgent"
  override var subscribers: List[Listener] = Nil

  lazy val presentObj = blackboard.get.presentObj
  //lazy val report = blackboard.get.report
  lazy val rules = blackboard.get.rules

  lazy val invertibleBackward = blackboard.get.invertibleBackward
  lazy val invertibleForward = blackboard.get.invertibleForward
  lazy val searchBackward = blackboard.get.searchBackward
  lazy val searchForward = blackboard.get.searchForward



  def ignoreGoal(node: Goal):Boolean ={
    if (node.isFinished){return true}
    if (!node.isLeaf){return true}
    false
  }

  def addTask(node:Goal):Unit

  def respond() = {
    log("invertible Backwards rule at responetime:" + invertibleBackward)
    log("responding to: " + mailbox)
    readMail.foreach {
      case Change(section,data,flags) =>
        data match {
          case g:Goal if !ignoreGoal(g) => addTask(g)
          case g:Goal => log("ignoring new goal")
          case a:Alternative => a.subgoals.filter(!ignoreGoal(_)).foreach(addTask)
          case _ => throw new IllegalArgumentException("unknown data type")
        }
      case _ => throw new IllegalArgumentException("unknown change type")
    }
    if (taskSet.isEmpty) log("NO TASKS FOUND") else log("Found "+taskSet.size+" task(s)")
  }

}

class ExpansionAgent(implicit controller: Controller) extends GoalAgent {
  override def logPrefix = "ExpansionAgent"
  override def ignoreGoal(g:Goal) = super.ignoreGoal(g) || g.isFullyExpanded
  def addTask(g:Goal) = taskSet+=new ExpansionTask(this,g)
}

class InvertibleBackwardAgent(implicit controller: Controller) extends GoalAgent {
  override def logPrefix = "InvertibleBackwardAgent"
  override def ignoreGoal(g:Goal) = super.ignoreGoal(g) || g.isBackwardExpanded
  override def addTask(g:Goal) = taskSet+=new InvertibleBackwardTask(this,g)
}

class InvertibleForwardAgent(implicit controller: Controller) extends GoalAgent {
  override def logPrefix = "InvertibleForwardAgent"
  override def ignoreGoal(g:Goal) = super.ignoreGoal(g) || g.isForwardExpanded
  override def addTask(g:Goal) = taskSet+=new InvertibleForwardTask(this,g)
}

class SearchBackwardAgent(implicit controller: Controller) extends GoalAgent {
  override def logPrefix = "SearchBackwardAgent"
  override def ignoreGoal(g:Goal) = super.ignoreGoal(g) || !g.isFullyExpanded
  override def addTask(g:Goal) = taskSet+=new SearchBackwardTask(this,g)
}

class SearchForwardAgent(implicit controller: Controller) extends GoalAgent {
  override def logPrefix = "SearchForwardAgent"
  override def ignoreGoal(g:Goal) = super.ignoreGoal(g) || !g.isFullyExpanded
  override def addTask(g:Goal) = taskSet+=new SearchForwardTask(this,g)
}

abstract class SimplifyingAgent(implicit controller: Controller) extends GoalAgent {
}




