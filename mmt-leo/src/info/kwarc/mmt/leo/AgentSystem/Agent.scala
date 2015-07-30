package info.kwarc.mmt.leo.AgentSystem

import util.control.Breaks._


import info.kwarc.mmt.api.frontend.{Controller, Logger}

import scala.collection.mutable
//TODO reimplement partition prover and andOr infastructure
/**
 * Trait representing Agents which houses most of their general functionality
 */

trait Speaker {
  /** a list of subscribers*/
  var subscribers : List[Listener]

  def addSubscriber(l:Listener)=subscribers::=l

  def removeSubscriber(l:Listener)=subscribers=subscribers.diff(List(l))
  /** sends a message to a listener*/
  def sendMessage(m:Message,a:Listener) = a.mailbox.enqueue(m)
  
  /**sends messages to all subscribers*/
  def sendToSubscribers(m:Message,onlyInterested:Boolean=false) = {
    subscribers.foreach(s =>
      if (onlyInterested) {
        if (s.hasInterest(m.flags)){sendMessage(m,s)}
      }else sendMessage(m,s)
    )
  }

}

trait Listener {
  /** Queue holding the interesting Events*/
  val mailbox: mutable.Queue[Message] = new mutable.Queue[Message]()

  def subscribeTo(s:Speaker) = s.addSubscriber(this)
  /** @return number of messages, the agent can currently work on */
  def numMessages: Int = mailbox.size
  /** Specifies what an agents interests are for the blackboard: "ADD", "DEL", "CHANGE", "CLOSE", */
  val interests: List[String] = Nil
  /**Function for determining whether the agent has a particular interest*/
  def hasInterest(i: String): Boolean = {interests.contains(i)}
  /**Function for determining whether the agent has interest in any of the input interests*/
  def hasInterest(l: List[String]): Boolean = {l.exists(interests.contains(_))}
}

trait Communicator extends Listener with Speaker

abstract class Agent(implicit controller: Controller,oLP:String) extends Logger with Communicator {
  type BBType <:Blackboard

  var blackboard:Option[BBType]=None

  /** the name of the agent */
  val name: String

  val report = controller.report
  def logPrefix = oLP +"#"+name

  override def toString: String= {name + "::numTasks:" + numTasks}

  protected def readMail:Seq[Message] = mailbox.dequeueAll(m=>true)

  def respond(): Unit
  
  /** @return number of tasks, the agent can currently work on */
  def numTasks: Int = taskSet.size

  /** Removes all Tasks from the task queue */
  def clearTasks(): Unit = {taskSet=taskSet.empty}
  
  /** Queue holding the tasks to be bid on */
  var taskSet: Set[Task] = Set.empty[Task]

  def removeTask(t:Task)= taskSet-=t

  /**
   * As getTasks with an infinite budget
   * @return - All Tasks that the current agent wants to execute.
   */
  def getAllTasks: Iterable[Task] = taskSet.synchronized(taskSet.iterator.toIterable)

  /**
   * Given a set of (newly) executing tasks, remove all colliding tasks.
   *
   * @param nExec - The newly executing tasks
   */
  def removeColliding(nExec: Iterable[Task]): Unit = taskSet.synchronized(taskSet.forall{tbe =>
    nExec.exists{e =>
      blackboard.get.sections.forall({ s =>
        val rem = e.writeSet(s).intersect(tbe.writeSet(s)).nonEmpty ||
          e.writeSet(s).intersect(tbe.writeSet(s)).nonEmpty ||
          e == tbe // Remove only tasks depending on written (changed) data.
        if (rem && e != tbe) println("The task\n  $tbe\n collided with\n  $e\n and was removed.")
        rem
      })
    }
  })

  /**Register agent to blackboard,    */
  def register(blackboard: BBType) {
    blackboard.registerAgent(this)
    this.blackboard=Some(blackboard)
  }

  def unregister(blackboard: Blackboard): Unit = {
    blackboard.unregisterAgent(this)
    clearTasks()
  }
}

/** Class of the auction agent which is responsible for
  * getting all of the available proof tasks and choosing
  * a proper non-colliding subset which hopefully maximizes
  * utility in the proof
  */
class AuctionAgent(implicit controller: Controller,oLP:String) extends Agent {
  var subscribers:List[Listener] = Nil

  lazy val executionAgent = blackboard.get.executionAgent.get

  val name = "AuctionAgent"

  def updateSubscribers() = subscribers:::=blackboard.get.agents

  val metaTaskQueue = new mutable.Queue[Task]()
  
  /** @return A list of all of the registered proof agents*/
  def subAgents() = blackboard.get.agents.diff(List(this))
  
  def respond() ={
    readMail.foreach {
      case t: Task => taskSet+=t
      case _ => throw new IllegalArgumentException("Unknown type of message")
    }
  }


  def concistencyCheck(tasks:mutable.Queue[Task]): Boolean ={
    val sections = blackboard.get.sections
    sections.forall({ s =>
      val wsList = tasks.flatMap(t => t.writeSet(s).toList)
      if (!(wsList.length==wsList.distinct.length)) {return false}
      true
    })
  }



  /**
   * Starts a new auction for agents to buy computation time
   * for their tasks. The result is a set of tasks,
   * that can be executed in parallel
   * @return Not yet executed non-colliding set of tasks
   */
  //TODO add auctioning
  def runAuction() : Unit = {
    updateSubscribers()
    var tasks = new mutable.Queue[Task]()
    subAgents().foreach(a=>tasks++=a.taskSet)
    val allTasks = tasks

    log("Found "+allTasks.length+" task(s)")

    //Eliminate collisions
    def filterTasks(tasks:mutable.Queue[Task], t1:Task):mutable.Queue[Task] = {
      tasks.filter(t2 => (!t1.collide(t2)) || (t1==t2) )
    }
    breakable { //TODO find a more elegant way to do this
      for (i <- allTasks.indices) {
        if (i >= tasks.length) break()
        tasks = filterTasks(tasks, tasks(i))
      }
    }



    //remove task from agent's queue if successful
    //TODO improve this interface
    tasks.foreach(t=>t.sentBy.removeTask(t))

    if (!concistencyCheck(tasks)){
      println("TASKS: "+tasks)
      throw new IllegalArgumentException("Selected tasks are colliding")
    }


    //Alert agents of tasks being dropped
    //allTasks.diff(tasks).foreach(t=>sendMessage(new AuctionFailure(this,t),t.sentBy))
    
    log("Selected "+tasks.length+" task(s) for execution")

    sendMessage(new MetaTask(tasks.toSet,this,"Parallelizable MetaTask"),executionAgent)
  }
}





class ExecutionAgent(implicit controller: Controller,oLP:String) extends Agent {
  val name = "ExecutionAgent"
  var subscribers:List[Listener] = Nil

  val metaTaskQueue = new mutable.Queue[Task]()

  def respond() = { readMail.foreach {
    case t: Task => log("Executing MetaTask: "+t);parallelExecute(t)
    case _ => throw new IllegalArgumentException("Unknown type of message")}
  }

  //TODO add parallelization
  def parallelExecute(t:Task) = if (t.isApplicable(this.blackboard.get)){
    t.execute()
  }else{
    log("MetaTask inapplicable")
  }

}