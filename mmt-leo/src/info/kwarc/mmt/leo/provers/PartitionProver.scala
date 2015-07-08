package info.kwarc.mmt.leo.provers

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
          node.addChild(add)
          if (add.data - int == 0) {
            add.setSatisfiability(true)
            add.percolateAndTrim()
            return
          }
        else if  (add.data < min) {add.setSatisfiability(false); add.percolateAndTrim()}
      }
    )
  }

}

object PartitionPresenter extends Presenter[Int] {
  def present(pt: ProofTree[Int]) = {
    /** @return a list of numbers solving the problem*/
    def getNumbers(node: ProofTree[Int]): List[Any] ={ //TODO figure out why List[Int] doesn't work
      if (node.children.isEmpty) {
        List(node.data)
      }else {
        val next = node.children.filter(_.isSolved).head
        (node.data - next.data)::getNumbers(next)
      }
    }

    pt.isSatisfiable match {
      case Some(true) => println("Solution: "+getNumbers(pt))
      case Some(false) => println("Contradiction Derived: No partition is possible")
      case None => println("Proof not found")
    }
  }
}