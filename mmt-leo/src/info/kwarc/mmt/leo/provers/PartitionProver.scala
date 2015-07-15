package info.kwarc.mmt.leo.provers

import info.kwarc.mmt.leo.datastructures._

/**
 * Created by mark on 7/4/15.
 */
class PartitionAgent(numbersVar: List[Int]) extends RuleAgent[Int] {
  val numbers=numbersVar
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



class PartitionTask(nodeVar: ProofTree[Int], agent: PartitionAgent) extends StdRuleTask[Int](agent,"PartitionTask") {
  lazy val blackboard = byAgent.blackboard
  readSet()
  val node = nodeVar
  override def readSet(): Set[ProofTree[Int]] = Set(node)
  override def writeSet(): Set[ProofTree[Int]] = Set(node)

  var isExpansion = true
  val allNumbers = agent.numbers

  def usableNumbers = allNumbers.filter(_ <= node.data)

  def addBranches(): Unit = {
    usableNumbers.foreach(int=>{
        val add = mkNode(node.data - int)
        val min: Int = usableNumbers.min
          node.addChild(add)
          if (add.data - int == 0) {
            add.setSatisfiability(true)
            add.percolate()
            return
          }
        else if  (add.data < min) {add.setSatisfiability(false); add.percolate()}
      }
    )
  }

}

object PartitionPresenter extends Presenter[Int] {
  def present(pt: ProofTree[Int]): String = {
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
      case Some(true) => "Solution: "+getNumbers(pt)
      case Some(false) => "Contradiction Derived, no partition is possible. Outputting tree:" + pt
      case None => "Proof not found"
    }
  }
}

class PartitionProver(target: Int , usableNumbers: List[Int], cycles: Int = 5) {

  def mkNode[A](data:A, cong:Boolean, sat: Option[Boolean]=None):ProofTree[A]={
    val pd= new ProofData(data,cong,sat)
    new ProofTree(pd)
  }

  val goal = mkNode(target,cong = true)
  val blackboard = new Blackboard(goal)
  val ra = new PartitionAgent(usableNumbers)
  val pa = new SingletonProofAgent[Int](ra)
  val ma = new AuctionAgent[Int]

  blackboard.registerAgent(ra)
  blackboard.registerAgent(pa)
  blackboard.registerAgent(ma)

  def run():String = {
    blackboard.run(cycles)
    PartitionPresenter.present(goal)
  }

}