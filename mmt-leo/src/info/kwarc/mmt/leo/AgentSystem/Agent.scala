package info.kwarc.mmt.leo.AgentSystem

import info.kwarc.mmt.api.frontend.Logger

import scala.collection.mutable
import scala.util.control.Breaks._


/**
 * Trait representing Agents which houses most of their general functionality
 */

trait Speaker {

  /** a list of subscribers*/
  var subscribers : List[Listener] = Nil

  /** adds a listener to  this.subscribers and updates the listener's subscribedTo list*/
  def addSubscriber(l:Listener)= {
    if (!subscribers.contains(l)){
      subscribers ::= l
    }
    if (!l.subscribedTo.contains(this)) {
      l.subscribedTo ::= this
    }
  }

  /** removes a listener from this.subscribers and updates the listener's subscribedTo list*/
  def removeSubscriber(l:Listener)={
    subscribers=subscribers.diff(List(l))
    l.subscribedTo=l.subscribedTo.diff(List(this))
  }

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

  /** Specifies what an agents interests are for the blackboard: "ADD", "DEL", "CHANGE", "CLOSE", */
  val interests: List[String]

  /**list of speakers that the object is subscribed to*/
  var subscribedTo: List[Speaker] = Nil

  /**list of speakers that the object would like to subscribe to*/
  def wantToSubscribeTo: List[Speaker]

  /** creates a connection between the speaker and the listener*/
  def subscribeTo(s:Speaker) = s.addSubscriber(this)

  /**establishes a connection between listeners and speakers*/
  def initConnection() = wantToSubscribeTo.foreach(subscribeTo)

  /** @return number of messages, the agent can currently work on */
  def numMessages: Int = mailbox.size

  /**Function for determining whether the agent has a particular interest*/
  def hasInterest(i: String): Boolean = {interests.contains(i)}
  /**Function for determining whether the agent has interest in any of the input interests*/
  def hasInterest(l: List[String]): Boolean = {l.exists(interests.contains(_))}
}

trait Communicator extends Listener with Speaker

abstract class Agent(blackboardParam: Blackboard) extends Communicator with Logger {


  def report = blackboard.report
  def logPrefix = blackboard.OLP +"#"+name

  val priority: Int

  /** the name of the agent */
  val name: String

  var terminated = false
  def terminate()= {terminated=true}

  override def toString: String= {name + "::numTasks:" + numTasks}

  protected def readMail:Seq[Message] = mailbox.dequeueAll(m=>true)

  def respond(): Unit
  
  /** @return number of tasks, the agent can currently work on */
  def numTasks: Int = taskQueue.size

  /** Removes all Tasks from the task queue */
  def clearTasks(): Unit = taskQueue.clear()
  
  /** Queue holding the tasks to be bid on */
  var taskQueue: mutable.Queue[Task] = new mutable.Queue[Task]()

  def removeTask(t:Task):Unit = taskQueue.dequeueFirst(_==t)

  /**
   * As getTasks with an infinite budget
   * @return - All Tasks that the current agent wants to execute.
   */
  def getAllTasks: Iterable[Task] = taskQueue.synchronized(taskQueue.iterator.toIterable)

  /**
   * Given a set of (newly) executing tasks, remove all colliding tasks.
   *
   * @param nExec - The newly executing tasks
   */
  def removeColliding(nExec: Iterable[Task]): Unit = taskQueue.synchronized(taskQueue.forall{tbe =>
    nExec.exists{e =>
      blackboard.sections.forall({ s =>
        val rem = e.writeSet(s).intersect(tbe.writeSet(s)).nonEmpty ||
          e.writeSet(s).intersect(tbe.writeSet(s)).nonEmpty ||
          e == tbe // Remove only tasks depending on written (changed) data.
        if (rem && e != tbe) println("The task\n  $tbe\n collided with\n  $e\n and was removed.")
        rem
      })
    }
  })

  def unregister(): Unit = {
    blackboard.unregisterAgent(this)
    clearTasks()
  }


  val blackboard = blackboardParam
  if (blackboard!=null) blackboard.registerAgent(this)

}



/** Class of the auction agent which is responsible for
  * getting all of the available proof tasks and choosing
  * a proper non-colliding subset which hopefully maximizes
  * utility in the proof
  */
class AuctionAgent(blackboard: Blackboard) extends Agent(blackboard) {
  val priority = 0

  val name = "AuctionAgent"

  override val interests: List[String] = List("BID") //TODO implement agent sending bids
  
  def wantToSubscribeTo:List[Speaker] = blackboard.agents

  //def updateSubscribers() = subscribers:::=blackboard.get.agents

  lazy val executionAgent = blackboard.executionAgent.get

  val metaTaskQueue = new mutable.Queue[Task]()
  
  /** @return A list of all of the registered proof agents*/
  def subAgents() = blackboard.agents.sortBy(-_.priority)


  def respond() ={
    if (!terminated) {
      readMail.foreach {
        case t: Task => taskQueue += t
        case _ => throw new IllegalArgumentException("Unknown type of message")
      }
    }
  }
  
  def consistencyCheck(tasks:List[Task]): Boolean ={
    val sections = blackboard.sections
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
    var tasks: List[Task]=Nil
    subAgents().foreach({a=> tasks :::= a.taskQueue.dequeueAll(t=>true).toList} )

    tasks = tasks.sortBy(-_.priority)
    val allTasks = tasks

    log("Found "+allTasks.length+" task(s)")


    //Eliminate collisions
    def filterTasks(tasks:List[Task], t1:Task):List[Task] = {
      tasks.filter(t2 => (!t1.collide(t2)) || (t1==t2) )
    }
    breakable { //TODO find a more elegant way to do this
      for (i <- allTasks.indices) {
        if (i >= tasks.length) break()
        tasks = filterTasks(tasks, tasks(i))
      }
    }

    //Alert the agent if task was unsuccessful
    allTasks.diff(tasks).foreach( t=>sendMessage(AuctionFailure(this,t),t.sentBy)   )

    if (!consistencyCheck(tasks)){
      println("TASKS: "+tasks)
      throw new IllegalArgumentException("Selected tasks are colliding")
    }

    log("Selected "+tasks.length+" task(s) for execution")

    sendMessage(new MetaTask(tasks,this),executionAgent)
  }
  
}


class ExecutionAgent(blackboard: Blackboard) extends Agent(blackboard) {
  val priority = 0

  val name = "ExecutionAgent"
  val interests = Nil

  def wantToSubscribeTo:List[Speaker] = List(blackboard.auctionAgent.get)

  val metaTaskQueue = new mutable.Queue[Task]()

  def respond() = {
    if (!terminated) {
      readMail.foreach {
        case t: Task => log("Executing MetaTask: " + t); parallelExecute(t)
        case _ => throw new IllegalArgumentException("Unknown type of message")
      }
    }
  }

  //TODO add parallelization
  def parallelExecute(t:Task) = if (t.isApplicable(this.blackboard)){
    t.execute()
  }else{
    log("MetaTask inapplicable")
  }

}