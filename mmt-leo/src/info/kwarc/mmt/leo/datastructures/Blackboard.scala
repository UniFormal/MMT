package info.kwarc.mmt.leo.datastructures

import info.kwarc.mmt.leo.provers.PartitionPresenter


/**
 * A blackboard is a central data collection object that supports
 * synchronized access between multiple processes.
 *
 * The implementation decides over the fairness and order of execution of the
 * processes.
 *
 * Taken heavily from the LeoPARD system
 */
class Blackboard[A](goal: ProofTree[A]) extends ProofTreeBlackboard[A] with EventBlackboard[A] with Debugger {
  def logPrefix = "Blackboard"

  var ruleAgents:List[RuleAgent[A]] = Nil
  var proofAgents:List[ProofAgent[A]] = Nil
  var metaAgents:List[MetaAgent[A]] = Nil
  
  var proofTree= goal
  var changeSeq = Seq(new Change(goal,List("ADD")))
  var ruleSeq: Seq[RuleTask[A]] = Nil
  var proofSeq: Seq[ProofTask[A]] = Nil
  var metaSeq: Seq[MetaTask[A]] = Nil
  
  def registerAgent(a : RuleAgent[A]) : Unit = {ruleAgents=a::ruleAgents; a.blackboard = this}
  def unregisterAgent(a : RuleAgent[A]) : Unit = {ruleAgents=ruleAgents.diff(List(a)); a.blackboard = null}


  def registerAgent(a : ProofAgent[A]) : Unit = {proofAgents=a::proofAgents; a.blackboard = this}
  def unregisterAgent(a : ProofAgent[A]) : Unit = {proofAgents=proofAgents.diff(List(a)); a.blackboard = null}
  def registerAgent(a : MetaAgent[A]) : Unit = {metaAgents=a::metaAgents; a.blackboard = this}
  def unregisterAgent(a : MetaAgent[A]) : Unit = {metaAgents=metaAgents.diff(List(a)); a.blackboard = null}

  /** This function runs the specific agent on the registered Blackboard. */
  def runCycle():Unit = {
    log("running rule agents")
    ruleAgents.foreach(_.run())
    log("running proof agents")
    proofAgents.foreach(_.run())
    log("running meta agents")
    metaAgents.foreach(_.run())
    log("finished cycle")
  }

  /** runs the blackboard for a given number of cycles, stopping if a solution is found*/
  def run(cycles:Int=3):Unit ={
    var i = 0
    while (proofTree.isSatisfiable.isEmpty && i<cycles){
      runCycle()
      i=i+1
    }
    log("Final tree:" + proofTree)
  }

  override def toString: String = {
    "Blackboard: \n Tree: " + proofTree +
      "\n RuleAgents: " + ruleAgents +
      "\n ProofAgents: " + proofAgents +
      "\n MetaAgents: " + metaAgents
  }

}

/**
 * This trait capsules the formulas responsible for the formula manipulation of the
 * blackboard.
 */
trait ProofTreeBlackboard[A] {

  var proofTree: ProofTree[A]
  /**
   * Adds a ProofTree to the blackboard, if it does not exist. If it exists
   * the old formula is returned.
   *
   * @param root root node to attach new proof
   * @param tree tree to attach to root
   * @return true if successful addition
   */
  def addTree(root: ProofTree[A], tree : ProofTree[A]) : Unit = root.addChild(tree)

  /**
   * Removes a formula from the Set fo formulas of the Blackboard.
   */
  def removeTree(tree : ProofTree[A]) : Unit = tree.disconnect()

  /** Returns a List of all nodes of the Blackboard's proof tree 
   * @return All formulas of the blackboard.
   */
  def getNodes : Iterable[ProofTree[A]] = proofTree.preDepthFlatten

  def lockNodes(task: Task[A]):Boolean = {
    val resultsW = task.writeSet().map(_.placeLock(readLock=true,writeLock=true))
    val resultsR = task.readSet().map(_.placeLock(readLock=false,writeLock=true))
    (resultsW++resultsR).forall(b=>b)
  }

  def unlockNodes(task: Task[A]):Unit = {
    task.writeSet().foreach(_.liftLock(readLock=false,writeLock=false))
    task.readSet().foreach(_.liftLock(readLock=false,writeLock=false))
  }
}

/**
 * This trait capsules the message handling for the blackboard
 */
trait EventBlackboard[A] {
  var changeSeq : Seq[Change[A]]
  var ruleSeq: Seq[RuleTask[A]]
  var proofSeq: Seq[ProofTask[A]]
}

abstract class Presenter[A] extends Debugger {
  def logPrefix="Presenter"
  def present(pt:ProofTree[A])
}



  

