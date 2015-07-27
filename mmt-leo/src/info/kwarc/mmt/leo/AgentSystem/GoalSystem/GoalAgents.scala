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

  override def logPrefix = "Goal Agent"

  override val name: String = "Goal Agent"
  override var subscribers: List[Listener] = Nil

  lazy val presentObj = blackboard.get.presentObj
  //lazy val report = blackboard.get.report
  lazy val rules = blackboard.get.rules


  def ignoreGoal(node: Goal):Boolean ={
    if (node.isFinished){return true}
    if (!node.isLeaf){return true}
    false
  }

  def addTask(node:Goal):Unit

  def respond() = {
    log("responding to: " + mailbox,Some("debug"))
    readMail.foreach {
      case Change(section,data,flags) =>
        data match {
          case g:Goal if ignoreGoal(g) => addTask(g)
          case a:Alternative => a.subgoals.filter(!ignoreGoal(_)).foreach(addTask)
        }
      case _ => throw new IllegalArgumentException("unknown change type")
    }
    if (taskSet.isEmpty) log("NO TASKS FOUND") else log("Found "+taskSet.size+" task(s)")
  }

  lazy val invertibleBackward = blackboard.get.rules.get(classOf[BackwardInvertible]).toList
  lazy val invertibleForward = blackboard.get.rules.get(classOf[ForwardInvertible]).toList
  lazy val searchBackward = blackboard.get.rules.get(classOf[BackwardSearch]).toList.sortBy(_.priority).reverse
  lazy val searchForward = blackboard.get.rules.get(classOf[ForwardSearch]).toList
}

class ExpansionAgent(implicit controller: Controller) extends GoalAgent {
  override def ignoreGoal(g:Goal) = super.ignoreGoal(g) || g.isFullyExpanded
  def addTask(g:Goal) = taskSet+=new ExpansionTask(this,g)
}

class InvertibleBackwardAgent(implicit controller: Controller) extends GoalAgent {
  override def ignoreGoal(g:Goal) = super.ignoreGoal(g) || g.isBackwardExpanded
  override def addTask(g:Goal) = taskSet+=new InvertibleBackwardTask(this,g)
}

class InvertibleForwardAgent(implicit controller: Controller) extends GoalAgent {
  override def ignoreGoal(g:Goal) = super.ignoreGoal(g) || g.isForwardExpanded
  override def addTask(g:Goal) = taskSet+=new InvertibleForwardTask(this,g)
}

class SearchBackwardAgent(implicit controller: Controller) extends GoalAgent {
  override def ignoreGoal(g:Goal) = super.ignoreGoal(g) || !g.isFullyExpanded
  override def addTask(g:Goal) = taskSet+=new SearchBackwardTask(this,g)
}

class SearchForwardAgent(implicit controller: Controller) extends GoalAgent {
  override def ignoreGoal(g:Goal) = super.ignoreGoal(g) || !g.isFullyExpanded
  override def addTask(g:Goal) = taskSet+=new SearchForwardTask(this,g)
}

abstract class SimplifyingAgent(implicit controller: Controller) extends GoalAgent {
}




