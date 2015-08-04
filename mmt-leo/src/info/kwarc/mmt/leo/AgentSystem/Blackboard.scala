package info.kwarc.mmt.leo.AgentSystem

import info.kwarc.mmt.api.frontend.{Controller, Report, Logger}

/**
 * A blackboard is a central data collection object that supports
 * synchronized access between multiple processes.
 * The blackboard acts as the data storage unit which the agents can access.
 * this class is the most abstract incarnation of the blackboard with only the
 * key features. Further extensions support different sections aka data containers
 * with change management. This allows for uniform access and eliminates code reuse.
 *
 */
abstract class Blackboard(implicit controller: Controller) extends Logger with Communicator{
  lazy val report = controller.report
  def logPrefix = "Blackboard"

  def wantToSubscribeTo:List[Speaker] = Nil
  val interests = Nil

  /**Boolean representing the status of the prof goal*/
  def finished: Boolean

  /**Lists of agents currently registered to the blackboard*/
  var agents: List[Agent] = Nil
  var auctionAgent: Option[AuctionAgent] = None
  var executionAgent: Option[ExecutionAgent] = None

  /**Function that registers agents to the blackboard*/
  def registerAgent(a: Agent): Boolean = {
    try {
      a match {
        case aa:AuctionAgent => auctionAgent = Some(aa)
        case ea:ExecutionAgent => executionAgent = Some(ea)
        case _ =>
          agents=a::agents
          //log(sections.flatMap(_.changes).toString())
          sections.foreach(s=>s.addSubscriber(a))
      }
      addSubscriber(a)
      log("Registered Agent: " + a);true
    }catch{
      case _:Throwable =>throw new IllegalArgumentException("Agent-BB type mis-match")
    }
  }

  /** Function that unregisters agents from the blackboard*/
  def unregisterAgent(a: Agent): Unit = {
    a match {
      case aa: AuctionAgent => auctionAgent = None
      case ea:ExecutionAgent => executionAgent = None
      case _ =>agents.diff(List(this))
    }
  }

  /** This function runs the specific agent on the registered Blackboard. */
  def runCycle(): Unit = {
    log("running agents")
    agents.foreach(_.respond())
    log("running auction agent")
    auctionAgent.get.runAuction()
    log("running execution agent")
    executionAgent.get.respond()
    log("finished cycle")
  }

  var cycle = 0
  /** runs the blackboard for a given number of cycles, stopping if a solution is found */
  def run(numCycles: Int = 3): Unit = {
    agents.foreach(_.initConnection())
    auctionAgent.get.initConnection()
    executionAgent.get.initConnection()
    while (!finished && cycle < numCycles) {
      runCycle()
      cycle = cycle + 1
    }
    if (finished) {log("PROOF COMPLETED!")}else{log("GIVING UP")}
  }

  override def toString: String = {
    "Blackboard: \n Sections: " + sections +
      "\n Agents: " + agents +
      "\n AuctionAgent: " + auctionAgent
  }

  def addSection(s: Section) = sections::=s
  /** the list which contains all of the sections(data areas) of the blackboard*/
  var sections :List[Section] = Nil

}

/** this class presents the final solution of a specific section of the blackboard*/
abstract class Presenter(implicit controller: Controller) extends Logger{
  type ObjectType
  def present(pt:ObjectType): String

  val report = controller.report
  def logPrefix = "Presenter"

}
