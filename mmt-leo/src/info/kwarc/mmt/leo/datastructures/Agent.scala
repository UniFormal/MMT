package info.kwarc.mmt.leo.datastructures

import scala.collection.mutable


/**
 * <p>
 * Interface for all Agent Implementations.
 *
 * Taken Heavily from the LeoPARD project
 */
abstract class Agent[A, T <: Task[A], E <: Event[A]] {
  /** @return the name of the agent */
  def name: String

  override def toString: String= {
    name + "::numTasks:" + numTasks
  }

  /** whether the agent is active or not */
  var isActive: Boolean = false

  /** main blackboard that the agent has access to */
  var blackboard: Blackboard[A] = null

  /** Specifies what an agents interests are: "ADD", "CHANGE", "REMOVE", "CLOSE", "DELETE"*/
  val interests: List[String]
  def hasInterest(i: String): Boolean = {interests.contains(i)}
  def hasInterest(l: List[String]): Boolean = {l.exists(interests.contains(_))}

  /** @return number of tasks, the agent can currently work on */
  def numTasks: Int = taskQueue.size

  /** This function runs the specific agent on the registered Blackboard. */
  def run(): Unit
  
  /**
   * This method is called when an agent is killed by the scheduler
   * during execution. This method does standardized nothing.
   *
   * In the case an external Process / Thread is created during the
   * execution of the agent, this method can clean up the processes.
   */
  def kill(): Unit = {}
  
  /** Removes all Tasks */
  def clearTasks(): Unit = taskQueue.synchronized(taskQueue.clear())

  /** Queue holding the interesting Events*/
  val eventQueue: mutable.Queue[E] = new mutable.Queue[E]()

  /** Queue holding the tasks to be bid on */
  val taskQueue: mutable.Queue[T] = new mutable.Queue[T]()

  /** Code for the sending a task down the hierarchy to changing the proof tree*/
  def executeTask(t:T): Unit

  /**
   * As getTasks with an infinite budget
   * @return - All Tasks that the current agent wants to execute.
   */
  def getAllTasks: Iterable[T] = taskQueue.synchronized(taskQueue.iterator.toIterable)

  /**
   * Given a set of (newly) executing tasks, remove all colliding tasks.
   *
   * @param nExec - The newly executing tasks
   */
  def removeColliding(nExec: Iterable[T]): Unit = taskQueue.synchronized(taskQueue.dequeueAll{tbe =>
    nExec.exists{e =>
      val rem = e.writeSet().intersect(tbe.writeSet()).nonEmpty ||
        e.writeSet().intersect(tbe.writeSet()).nonEmpty ||
        e == tbe // Remove only tasks depending on written (changed) data.
      if(rem && e != tbe) println("The task\n  $tbe\n collided with\n  $e\n and was removed.")
      rem
    }
  })
  
}

abstract class RuleAgent[A] extends Agent[A, RuleTask[A], Event[A]]{
  /**Register agent to blackboard,    */
  def register(blackboard: Blackboard[A]) {
    blackboard.registerAgent(this)
  }

  def unregister(blackboard: Blackboard[A]): Unit = {
    blackboard.unregisterAgent(this)
    taskQueue.synchronized(taskQueue.clear())
  }

}

abstract class ProofAgent[A] extends Agent[A, ProofTask[A], RuleTask[A]]{

  /**
   * In this method the Agent gets the Blackboard it will work on.
   * Registration for Triggers should be done in here.
   */
  def register(blackboard: Blackboard[A]) {
    blackboard.registerAgent(this)
  }

  def unregister(blackboard: Blackboard[A]): Unit = {
    blackboard.unregisterAgent(this)
    taskQueue.synchronized(taskQueue.clear())
  }

  def executeTask(pt: ProofTask[A]) = {
    pt.ruleSets.foreach(rs=>rs.foreach(rt=>rt.byAgent.executeTask(rt)))
  }
}

abstract class MetaAgent[A] extends Agent[A, MetaTask[A], ProofTask[A]]{

  /**
   * In this method the Agent gets the Blackboard it will work on.
   * Registration for Triggers should be done in here.
   */
  def register(blackboard: Blackboard[A]) {
    blackboard.registerAgent(this)
  }

  def unregister(blackboard: Blackboard[A]): Unit = {
    blackboard.unregisterAgent(this)
    taskQueue.synchronized(taskQueue.clear())
  }

  def executeTask(mt: MetaTask[A]) = {
    mt.proofSets.foreach(ps=>ps.foreach(pt=>pt.byAgent.executeTask(pt)))
  }

  def makeMetaTask(q: mutable.Queue[Set[ProofTask[A]]]): MetaTask[A] = {
    val out = new MetaTask[A]
    q.foreach(out.proofSets.enqueue(_))
    out.name = "AuctionMetaTask"
    out.flags = List("ADD")
    out.byAgent = this
    out
  }

  def makeMetaTask(s: Set[ProofTask[A]]): MetaTask[A] = {
    val out = new MetaTask[A]
    out.proofSets.enqueue(s)
    out.name = "AuctionMetaTask"
    out.flags = List("ADD")
    out.byAgent = this
    out
  }

}

class SingletonProofAgent[A](ruleAgent: RuleAgent[A]) extends ProofAgent[A] {

  val name = "SingletonProofAgent"
  val interests = Nil

  def ruleTaskToProofTask(rt: RuleTask[A]): ProofTask[A] = {
    val out = new ProofTask[A]
    out.ruleSets.enqueue(Set(rt))
    out.name = "SingletonProofTask"
    out.flags = List("ADD")
    out.byAgent = this
    out
  }

  def run():Unit = {
    ruleAgent.taskQueue.foreach(rt=>this.taskQueue.enqueue(ruleTaskToProofTask(rt)))
    println(this + " was run")
  }

}


class AuctionAgent[A] extends MetaAgent[A] {

  val name = "AuctionAgent"

  val interests = Nil

  def proofAgents() = blackboard.proofAgents

  def runAgents() = proofAgents().foreach(_.run())

  def getAuctionedTasks: mutable.Queue[ProofTask[A]] ={
    val allTasks = new mutable.Queue[ProofTask[A]]()
    proofAgents().foreach(allTasks++_.taskQueue)
    println("Proof agents: " +proofAgents) //TODO left off here
    println("got auctioned tasks" + allTasks)
    allTasks
  }

  def run():Unit ={
    executeTask(makeMetaTask(getTaskSet))
  }

  /**
   * Starts a new auction for agents to buy computation time
   * for their tasks.
   *
   * The result is a set of tasks, that can be executed in parallel
   *
   * @return Not yet executed noncolliding set of tasks
   */


  def getTaskSet : Set[ProofTask[A]] = {
    val allTasks = new mutable.Queue[ProofTask[A]]()
    proofAgents().foreach(allTasks++_.taskQueue)
/*
    def removeColliding(nExec: Iterable[ProofTask[A]]): Unit = allTasks.synchronized(allTasks.dequeueAll{tbe =>
      nExec.exists{e =>
        val rem = e.writeSet().intersect(tbe.writeSet()).nonEmpty ||
          e.writeSet().intersect(tbe.writeSet()).nonEmpty ||
          e == tbe // Remove only tasks depending on written (changed) data.
        if(rem && e != tbe) println("The task\n  $tbe\n collided with\n  $e\n and was removed.")
        rem
      }
    })
    removeColliding(allTasks)
*/ //TODO investigate why this does not work

    println("got auctioned tasks" + allTasks)
    allTasks.toSet
  }
  
}

