package info.kwarc.mmt.leo.datastructures

import scala.collection.mutable

/**
 * <p>
 * Interface for all Agent Implementations.
 *
 * Taken Heavily from the LeoPARD project
 *
 *
 */
//
trait Agent extends Debugger {

  type TaskType <: Task
  type EventType <: Event
  type BlackboardType <: Blackboard

  /** the name of the agent */
  val name: String

  def logPrefix = name

  override def toString: String= {name + "::numTasks:" + numTasks}

  /** whether the agent is active or not */
  var isActive: Boolean = false

  /** main blackboard that the agent has access to */
  var blackboard: Option[BlackboardType] = None

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
  val eventQueue: mutable.Queue[EventType] = new mutable.Queue[EventType]()

  /** Queue holding the tasks to be bid on */
  val taskQueue: mutable.Queue[TaskType] = new mutable.Queue[TaskType]()

  /** Code for the sending a task down the hierarchy to changing the proof tree*/
  def executeTask(t:TaskType): Unit

  /**
   * As getTasks with an infinite budget
   * @return - All Tasks that the current agent wants to execute.
   */
  def getAllTasks: Iterable[TaskType] = taskQueue.synchronized(taskQueue.iterator.toIterable)

  /**
   * Given a set of (newly) executing tasks, remove all colliding tasks.
   *
   * @param nExec - The newly executing tasks
   */
  def removeColliding(nExec: Iterable[TaskType]): Unit = taskQueue.synchronized(taskQueue.dequeueAll{tbe =>
    nExec.exists{e =>
      blackboard.get.sections.forall({ s =>
        val rem = e.writeList(s).intersect(tbe.writeList(s)).nonEmpty ||
          e.writeList(s).intersect(tbe.writeList(s)).nonEmpty ||
          e == tbe // Remove only tasks depending on written (changed) data.
        if (rem && e != tbe) println("The task\n  $tbe\n collided with\n  $e\n and was removed.")
        rem
      })
    }
  })

  /**Register agent to blackboard,    */
  def register(blackboard: Blackboard) {
    blackboard.registerAgent(this)
  }

  def unregister(blackboard: Blackboard): Unit = {
    blackboard.unregisterAgent(this)
    taskQueue.synchronized(taskQueue.clear())
  }
  
}

abstract class RuleAgent extends Agent{
  type TaskType = RuleTask
  type EventType = Event

}

abstract class ProofAgent extends Agent{
  type TaskType = ProofTask
  type EventType = Event


  def executeTask(pt: ProofTask) = {
    log("Executing Task",2)
    pt.ruleLists.foreach(rs=>rs.filter(_.isApplicable(blackboard.get)).foreach({rt =>
      rt.byAgent.executeTask(rt)
      rt.byAgent.taskQueue.dequeueFirst(_==rt) //to remove completed task from list
    }))
    taskQueue.dequeueFirst(_==pt) //to remove completed task from list
  }
}

abstract class MetaAgent extends Agent{

  type TaskType = MetaTask
  type EventType = Event

  def executeTask(mt: MetaTask) = {
    log("Executing MetaTask",2)
    mt.proofLists.foreach(ps=>ps.filter(_.isApplicable(blackboard.get)).foreach(pt=>pt.byAgent.executeTask(pt)))
  }

  def makeMetaTask(q: mutable.Queue[List[ProofTask]]): PTMetaTask = {
    val out = new PTMetaTask(this, name+"MetaTask")
    q.foreach(out.proofLists.enqueue(_))
    //out.flags = List("ADD")
    out
  }

  def makeMetaTask(s: List[ProofTask]): MetaTask = {
    val out = new PTMetaTask(this, name+"MetaTask")
    out.proofLists.enqueue(s)
    //out.flags = List("ADD")
    out
  }

}

class SingletonProofAgent(ruleAgent: RuleAgent)  extends ProofAgent {

  val name = "Singleton"+ruleAgent.name
  val interests = Nil

  def ruleTaskToProofTask(rt: RuleTask): ProofTask = {
    log("Converting: "+rt,3)
    val out = new PTProofTask(this, "Singleton"+rt.name)
    out.ruleLists.enqueue(List(rt))
    //out.flags = List("ADD")
    out
  }

  def run():Unit = {
    ruleAgent.taskQueue.foreach(rt=>this.taskQueue.enqueue(ruleTaskToProofTask(rt)))
    log("Found "+ taskQueue.length + " task(s)" )
  }

}

class AuctionAgent extends MetaAgent {

  val name = "AuctionAgent"

  val interests = Nil

  def proofAgents() = blackboard.get.proofAgents

  //def runAgents() = proofAgents().foreach(_.run())

/*  def getAuctionedTasks: mutable.Queue[ProofTask] ={
    var allTasks = new mutable.Queue[ProofTask]()
    proofAgents().foreach(pa=>allTasks=allTasks++pa.taskQueue)
    println("Proof agents: " +proofAgents)
    println("got auctioned tasks" + allTasks)
    allTasks
  }*/

  def run():Unit ={
    val taskSet = getTaskSet
    if (taskSet.isEmpty){blackboard.get.finished=true}
    else{executeTask(makeMetaTask(getTaskSet))}
  }

  /**
   * Starts a new auction for agents to buy computation time
   * for their tasks.
   *
   * The result is a set of tasks, that can be executed in parallel
   *
   * @return Not yet executed noncolliding set of tasks
   */
  def getTaskSet : List[ProofTask] = {
    var allTasks = new mutable.Queue[ProofTask]()
    proofAgents().foreach(pa=>allTasks=allTasks++pa.taskQueue)

    //Eliminate collisions
    for( t1 <- allTasks ){allTasks=allTasks.filter(t2 => !t1.collide(t2)||t1==t2)}

    log("Selected "+allTasks.length+" task(s) for execution")
    allTasks.foreach(_.log(this.toString, 3))

    allTasks.toList
  }
  
}

