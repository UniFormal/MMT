package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.PartitionImpl

import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.{DataTree, DataTreeSection}
import info.kwarc.mmt.leo.AgentSystem.{Listener, Change, Agent}

/**
 * Created by Mark on 7/21/2015.
 */
class PartitionAgent(numbersVar: List[Int]) extends Agent {
  type BBType = IntBlackboard

  var subscribers:List[Listener] = Nil
  val numbers=numbersVar
  val name = "PartitionAgent"
  override val interests = List("ADD")


  def respond() = {
    log("responding to: " + mailbox,4)
    readMail.foreach {
    case Change(section,data,flags) =>
      data match {
        case node:DataTree[_] if node.isDeleted => //TODO fix with reflection?
        case node:DataTree[_] if node.isBelowSatisfied =>
        case node:DataTree[_] =>
          node.openLeaves.foreach (pt => taskSet += createTask (pt.asInstanceOf[DataTree[Int]] ) )
        }
        case _ => throw new IllegalArgumentException("unknown change type")
    case _ => throw new IllegalArgumentException("unknown message type")
    }
    if (taskSet.isEmpty) log("NO TASKS FOUND") else log("Found "+taskSet.size+" task(s)")
  }

  def createTask(pt: DataTree[Int]): PartitionTask =  { new PartitionTask(pt,this)}


}
