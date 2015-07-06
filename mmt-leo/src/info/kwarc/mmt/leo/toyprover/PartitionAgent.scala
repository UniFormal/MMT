package info.kwarc.mmt.leo.toyprover

import info.kwarc.mmt.leo.datastructures._

/**
 * Created by mark on 7/4/15.
 */
class PartitionAgent extends RuleAgent[Int] {
  val name = "PartitionAgent"
  val interests = List("ADD")

  def run(): Unit ={
    blackboard.proofTree.leaves.foreach(pt=>taskQueue.enqueue(new PartitionTask(pt)))
    println(this + " resulting task Queue: "+taskQueue)
  }

  def executeTask(rt: RuleTask[Int]) = {
    println("executing: "+ rt)
    rt match {
      case ptt:PartitionTask if ptt.isExpansion =>
        ptt.node.proofData.conjunctive=false
        ptt.addBranches()
      case _ => println("Error: Need a PartitionTask")
    }
  }

}



class PartitionTask(nodeVar: ProofTree[Int]) extends RuleTask[Int] {
  val node = nodeVar
  var isExpansion = true
  val allNumbers = List(2,3,5,7)
  def usableNumbers = allNumbers.filter(_ <= node.data)
  def addBranches() = {
    usableNumbers.foreach(int=>{
        val add = mkNode(node.data-int)
        val min = usableNumbers.min
        node.addChild(add)
        if (add.data-int==0) {add.setSatisfiability(true); add.percolateAndTrim()}
        else if  (add.data < min) {add.setSatisfiability(false); add.percolateAndTrim()}
      }
    )
  }

}
