package info.kwarc.mmt.leo.provers

import info.kwarc.mmt.leo.datastructures._

/**
 * Created by mark on 7/4/15.
 */

class Def[C](implicit desired : Manifest[C]) {
  def unapply[X](c : X)(implicit m : Manifest[X]) : Option[C] = {
     def sameArgs = desired.typeArguments.zip(m.typeArguments).forall {case (desired,actual) => desired >:> actual}
     if (desired >:> m && sameArgs) Some(c.asInstanceOf[C])
     else None
  }
}


class IntBlackboard(g:DataTree[Int]) extends AndOrBlackboard[DataTree[Int]](g) {
  override val proofSection = new DataTreeSection[Int](g)
  log("Added Goal of type: " + g.getClass + g)
  log(proofSection.toString)
}

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



class PartitionTask(nodeVar: DataTree[Int], agent: PartitionAgent) extends PTRuleTask(agent,"PartitionTask") {

  val node = nodeVar

  val IntTree = new Def[DataTreeSection[Int]]
  override def readList(s: Section): List[s.ObjectType] = {s match {
    case IntTree(t) => List(node.asInstanceOf[s.ObjectType])
    case _ => Nil.asInstanceOf[List[s.ObjectType]]}
  }

  override def writeList(s: Section):List[s.ObjectType] = {s match {
    case IntTree(t) => List(node.asInstanceOf[s.ObjectType])
    case _ => Nil.asInstanceOf[List[s.ObjectType]]}
  }

  var isExpansion = true
  val allNumbers = agent.numbers

  def usableNumbers = allNumbers.filter(_ <= node.data)

  def addBranches(): Unit = {
    usableNumbers.foreach(int=>{
        val add = new DataTree(node.data - int,conjVar=false,None)
        val min: Int = usableNumbers.min
          node.addChild(add)
          if (add.data - int == 0) {
            add.setSat(true)
            add.percolate()
            return
          }
        else if  (add.data < min) {add.setSat(false); add.percolate()}
      }
    )
  }

}

object PartitionPresenter extends Presenter {
  type ObjectType= DataTree[Int]

  def present(pt: DataTree[Int]): String = {
    /** @return a list of numbers solving the problem*/
    def getNumbers(node: DataTree[Int]): List[Any] ={ //TODO figure out why List[Int] doesn't work
      if (node.children.isEmpty) {
        List(node.data)
      }else {
        val next = node.children.filter(_.isSolved).head
        (node.data - next.data)::getNumbers(next)
      }
    }

    pt.isSat match {
      case Some(true) => "Solution: "+getNumbers(pt)
      case Some(false) => "Contradiction Derived, no partition is possible. Outputting tree:" + pt
      case None => "Proof not found"
    }
  }
}


class PartitionProver(target: Int , usableNumbers: List[Int], cycles: Int = 5) {


  val goal = new DataTree(target,conjVar = true,None)
  val blackboard = new IntBlackboard(goal)
  val ra = new PartitionAgent(usableNumbers)
  val pa = new SingletonProofAgent(ra)
  val ma = new AuctionAgent

  blackboard.registerAgent(ra)
  blackboard.registerAgent(pa)
  blackboard.registerAgent(ma)

  def run():String = {
    blackboard.run(cycles)
    PartitionPresenter.present(goal)
  }

}