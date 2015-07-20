package info.kwarc.mmt.leo.datastructures

import scala.collection.mutable

/**
 * Trait representing Agents which houses most of their general functionality
 */
trait Agent extends Debugger {

  /** These types represent the types various objects that they interact with*/
  type TaskType <: Task
  type EventType <: Event
  type BlackboardType <: Blackboard

  /** the name of the agent */
  val name: String

  def logPrefix = name

  override def toString: String= {name + "::numTasks:" + numTasks}

  /** main blackboard that the agent has access to */
  var blackboard: Option[BlackboardType] = None

  /** Specifies what an agents interests are: "ADD", "DEL", "CHANGE", "CLOSE", */
  val interests: List[String]

  /**Function for determining whether the agent has a particular interest*/
  def hasInterest(i: String): Boolean = {interests.contains(i)}

  /**Function for determining whether the agent has interest in any of the input interests*/
  def hasInterest(l: List[String]): Boolean = {l.exists(interests.contains(_))}

  /** @return number of tasks, the agent can currently work on */
  def numTasks: Int = taskQueue.size

  /** This function runs the specific agent,
    * this function is called by the higher agents and the blackboard
    */
  def run(): Unit

  /** Removes all Tasks from the task queue */
  def clearTasks(): Unit = taskQueue.synchronized(taskQueue.clear())

  /** Queue holding the interesting Events*/
  val eventQueue: mutable.Queue[EventType] = new mutable.Queue[EventType]()

  /** Queue holding the tasks to be bid on */
  val taskQueue: mutable.Queue[TaskType] = new mutable.Queue[TaskType]()

  /** Code for the sending a task down the hierarchy to eventually change the proof tree*/
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

  /**Registers an agent to the blackboard */
  def register(blackboard: BlackboardType) {
    blackboard.registerAgent(this)
  }

  /**Unregisters an agent from the blackboard */
  def unregister(blackboard: BlackboardType): Unit = {
    blackboard.unregisterAgent(this)
    taskQueue.synchronized(taskQueue.clear())
  }
  
}

/**Class for Rule agents*/
abstract class RuleAgent extends Agent{
  type TaskType = RuleTask
  type EventType = Event
}

/**Class for Proof agents*/
//TODO make this into trait
abstract class ProofAgent extends Agent{
  type TaskType = ProofTask
  type EventType = Event

  /** executes a proof task by delegating responsibility to rule agents*/
  def executeTask(pt: ProofTask) = {
    log("Executing Task",2)
    pt.ruleLists.foreach(rs=>rs.filter(_.isApplicable(blackboard.get)).foreach({rt =>
      rt.byAgent.executeTask(rt)
      rt.byAgent.taskQueue.dequeueFirst(_==rt) //remove completed RuleTask from target agent's taskQueue
    }))
    taskQueue.dequeueFirst(_==pt) //remove completed ProofTask from this agent's taskQueue
  }
}

/**Class for Meta agents*/
abstract class MetaAgent extends Agent{
  type TaskType = MetaTask
  type EventType = Event //TODO generalize event handling to change handling

  /** executes a meta task by checking applicability and
    * delegating proof tasks to corresponding proof agents*/
  def executeTask(mt: MetaTask) = {
    log("Executing MetaTask",2)
    mt.proofLists.foreach(ps=>
      ps.filter(_.isApplicable(blackboard.get)).foreach(pt=>
        pt.byAgent.executeTask(pt)))
    taskQueue.dequeueFirst(_==mt) //remove completed MetaTask from this agent's taskQueue

  }

  /** Makes a meta task out of a Queue of Lists of proof tasks
    * the list of ProofTasks should be parallelizable and noncolliding
    */
  def makeMetaTask(q: mutable.Queue[List[ProofTask]]): PTMetaTask = {
    val out = new PTMetaTask(this, name+"MetaTask")
    q.foreach(out.proofLists.enqueue(_))
    //out.flags = List("ADD")
    out
  }

  /** Makes a meta task out of a Lists of proof tasks
    * the list of ProofTasks should be parallelizable and noncolliding
    */
  def makeMetaTask(s: List[ProofTask]): MetaTask = {
    val out = new PTMetaTask(this, name+"MetaTask")
    out.proofLists.enqueue(s)
    //out.flags = List("ADD")
    out
  }

}

/** this class represents a proof agent which just sends the relevant information
  * up the chain to the meta agent. No strategy or proof planning is involved
  */
class SingletonProofAgent(ruleAgent: RuleAgent)  extends ProofAgent {
  val name = "Singleton"+ruleAgent.name
  val interests = Nil

  /** Converts a rule task to a proof task*/
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

/** Class of the auction agent which is responsible for
  * getting all of the available proof tasks and choosing
  * a proper non-colliding subset which hopefully maximizes
  * utility in the proof
  */
class AuctionAgent extends MetaAgent {

  val name = "AuctionAgent"

  val interests = Nil

  /** @return A list of all of the registered proof agents*/
  def proofAgents() = blackboard.get.proofAgents

  def run():Unit ={
    val taskSet = getTaskSet
    if (taskSet.isEmpty){blackboard.get.finished=true} //TODO add running agent check
    else{executeTask(makeMetaTask(getTaskSet))}
  }

  /**
   * Starts a new auction for agents to buy computation time
   * for their tasks. The result is a set of tasks,
   * that can be executed in parallel
   * @return Not yet executed non-colliding set of tasks
   */
  //TODO add auctioning
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

