package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.PartitionImpl

import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.DataTree
import info.kwarc.mmt.leo.AgentSystem.{RuleAgent, RuleTask}

/**
 * Created by Mark on 7/21/2015.
 */
class PartitionAgent(numbersVar: List[Int]) extends RuleAgent {
  type BlackboardType = IntBlackboard
  val numbers=numbersVar
  val name = "PartitionAgent"
  val interests = List("ADD")

  def run(): Unit ={
    blackboard.get.proofTree.openLeaves.foreach(pt=>taskQueue.enqueue(createTask(pt)))
    if (taskQueue.isEmpty) log("NO TASKS FOUND") else log("Found "+taskQueue.length+" task(s)")
  }

  def createTask(pt: DataTree[Int]): PartitionTask =  { new PartitionTask(pt,this)}

  def executeTask(rt: RuleTask) = {
    log("executing: "+ rt,3)
    log("TREE BEFORE: " + addIndent(blackboard.get.proofTree.toString),2)
    rt match {
      case ptt:PartitionTask if ptt.isExpansion =>
        ptt.node.conj=false
        ptt.addBranches()
      case _ => println("Error: Need a PartitionTask")
    }
    log("TREE AFTER: " + addIndent(blackboard.get.proofTree.toString),2)
  }

}
