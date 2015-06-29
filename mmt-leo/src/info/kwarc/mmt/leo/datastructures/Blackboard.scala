package info.kwarc.mmt.leo.datastructures


/**
 * A blackboard is a central data collection object that supports
 * synchronized access between multiple processes.
 *
 * The implementation decides over the fairness and order of execution of the
 * processes.
 *
 * Taken heavily from the LeoPARD system
 */
class Blackboard[A](goal: ProofTree[A]) extends ProofTreeBlackboard[A] with EventBlackboard[A] {
  var agents:List[Agent[A]] = Nil
  var proofTree= goal
  var eventSeq = Seq(Event(goal,List("ADD")))
  
  var scheduleAgent: ScheduleAgent[A]= null
  var auctionAgent: AuctionAgent[A]= null
  var executionAgent: ExecutionAgent[A]= null
  def registerScheduleAgent(a : ScheduleAgent[A])={scheduleAgent=a}
  def registerAuctionAgent(a : AuctionAgent[A])={auctionAgent=a}
  def registerExecutionAgent(a : ExecutionAgent[A])={executionAgent=a}
  
  def registerAgent(a : Agent[A]) : Unit = {agents=List(a):::agents}
  def registerAgent(l : List[Agent[A]] ) : Unit =  {agents=l:::agents}
  def unregisterAgent(a : Agent[A]) : Unit = {agents=agents.diff(List(a))}
  def unregisterAgent(l : List[Agent[A]]) : Unit = {agents=agents.diff(l)}

  def run(): Unit ={
    scheduleAgent.run()
  }
  /**
   *
   * Returns for debugging and interactive use the agent work
   *
   * @return all registered agents and their budget
   */

}

/**
 * This trait capsules the formulas responsible for the formula manipulation of the
 * blackboard.
 */
trait ProofTreeBlackboard[A] {

  var proofTree: ProofTree[A]
  /**
   * Adds a prooftree to the blackboard, if it does not exist. If it exists
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

}

/**
 * This trait capsules the message handling for the blackboard
 */
trait EventBlackboard[A] {
  var eventSeq : Seq[Event[A]]
}






  

