package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.PartitionImpl

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.{DataTree, DataTreeSection}
import info.kwarc.mmt.leo.AgentSystem.{Speaker, Listener, Change, Agent}

/**
 * Created by Mark on 7/21/2015.
 */
class PartitionAgent(numbersVar: List[Int])(implicit c: Controller,oLP:String) extends Agent {
  type BBType = IntBlackboard

  val numbers=numbersVar
  val name = "PartitionAgent"
  /** list of speakers that the object would like to subscribe to */
  override def wantToSubscribeTo: List[Speaker] = List(blackboard.get.auctionAgent.get)
  override val interests = List("ADD")


  def ignoreNode(node: DataTree[_]):Boolean ={
    if (node.isDeleted){return true}
    if (node.isBelowSatisfied){return true}
    false
  }

  def respond() = {
    log("responding to: " + mailbox,Some("debug"))
    readMail.foreach {
    case Change(section,data,flags) =>
      data match {
        case node: DataTree[_] if ignoreNode(node) => //TODO fix with reflection?
        case node: DataTree[_] =>
          node.openLeaves.foreach(pt => taskSet += createTask(pt.asInstanceOf[DataTree[Int]]))
        case _ => throw new IllegalArgumentException("unknown change type")
      }
    case _ => throw new IllegalArgumentException("unknown message type")
    }
    if (taskSet.isEmpty) log("NO TASKS FOUND") else log("Found "+taskSet.size+" task(s)")
  }

  def createTask(pt: DataTree[Int]): PartitionTask =  { new PartitionTask(pt,this)}

}
