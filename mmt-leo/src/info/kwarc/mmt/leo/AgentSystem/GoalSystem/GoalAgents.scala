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
  override type BBType = GoalBlackboard

  //override def respond(): Unit = ???

  override def logPrefix = "Goal Agent"

  override val name: String = "Goal Agent"
  override var subscribers: List[Listener] = Nil

  lazy val presentObj = blackboard.get.presentObj
  //lazy val report = blackboard.get.report
  lazy val rules = blackboard.get.rules

}

abstract class InvertibleAgent(implicit controller: Controller) extends GoalAgent {
  lazy val invertibleBackward = blackboard.get.rules.get(classOf[BackwardInvertible]).toList
  lazy val invertibleForward = blackboard.get.rules.get(classOf[ForwardInvertible]).toList

/*  def respond() = readMail.foreach {
    case Change(s,data,flags) if flags.contains("ADD") =>


  }*/

}
abstract class InvertibleBackwardAgent(implicit controller: Controller) extends InvertibleAgent {}
abstract class InvertibleForwardAgent(implicit controller: Controller) extends InvertibleAgent {}

abstract class SearchBackwardAgent(implicit controller: Controller) extends GoalAgent {
  lazy val searchBackward = blackboard.get.rules.get(classOf[BackwardSearch]).toList.sortBy(_.priority).reverse
}

abstract class SearchForwardAgent(implicit controller: Controller) extends GoalAgent {
  lazy val searchForward = blackboard.get.rules.get(classOf[ForwardSearch]).toList
}

abstract class SimplifyingAgent(implicit controller: Controller) extends GoalAgent {
}




