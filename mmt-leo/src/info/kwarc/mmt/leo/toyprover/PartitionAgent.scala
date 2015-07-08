package info.kwarc.mmt.leo.toyprover

import info.kwarc.mmt.leo.datastructures._

/**
 * Created by mark on 7/4/15.
 */
class PartitionAgent extends RuleAgent[Int] {
  val name = "PartitionAgent"
  val interests = List("ADD")

  def run(): Unit ={
    blackboard.proofTree.openLeaves.foreach(pt=>taskQueue.enqueue(createTask(pt)))
    if (taskQueue.isEmpty) log("NO TASKS FOUND") else log("Found "+taskQueue.length+" task(s)")
  }

  def createTask(pt: ProofTree[Int]): PartitionTask =  { new PartitionTask(pt,this)}

  def executeTask(rt: RuleTask[Int]) = {
    log("executing: "+ rt,3)
    log("TREE BEFORE: " + addIndent(blackboard.proofTree.toString),2)
    rt match {
      case ptt:PartitionTask if ptt.isExpansion =>
        ptt.node.proofData.conjunctive=false
        ptt.addBranches()
      case _ => println("Error: Need a PartitionTask")
    }
    log("TREE AFTER: " + addIndent(blackboard.proofTree.toString),2)
  }

}



class PartitionTask(nodeVar: ProofTree[Int], agent: RuleAgent[Int]) extends StdRuleTask[Int](agent,"PartitionTask") {
  lazy val blackboard = byAgent.blackboard
  readSet()
  val node = nodeVar
  override def readSet(): Set[ProofTree[Int]] = Set(node)
  override def writeSet(): Set[ProofTree[Int]] = Set(node)

  var isExpansion = true
  val allNumbers = List(2,3,5,7)

  def usableNumbers = allNumbers.filter(_ <= node.data)

  def addBranches(): Unit = {
    usableNumbers.foreach(int=>{
        val add = mkNode(node.data - int)
        val min: Int = usableNumbers.min
        if (isApplicable(blackboard)) {
          node.addChild(add)
          if (add.data - int == 0) {
            add.setSatisfiability(true)
            add.percolateAndTrim()
          }
        }
        else if  (add.data < min) {add.setSatisfiability(false); add.percolateAndTrim()}
      }
    )
  }

}
