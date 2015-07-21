package info.kwarc.mmt.leo.AgentSystem

/**
 * A blackboard is a central data collection object that supports
 * synchronized access between multiple processes.
 * The blackboard acts as the data storage unit which the agents can access.
 * this class is the most abstract incarnation of the blackboard with only the
 * key features. Further extensions support different sections aka data containers
 * with change management. This allows for uniform access and eliminates code reuse.
 *
 */
abstract class Blackboard extends Debugger {
  def logPrefix = "Blackboard"

  /**Boolean representing the status of the prof goal*/
  var finished: Boolean = false

  /**Lists of agents currently registered to the blackboard*/
  var ruleAgents: List[RuleAgent] = Nil
  var proofAgents: List[ProofAgent] = Nil
  var metaAgents: List[MetaAgent] = Nil

  /**Function that registers agents to the blackboard*/
  def registerAgent(a: Agent): Unit = {
    try {
      a.blackboard = Some(this.asInstanceOf[a.BlackboardType])
      a match {
        case a: RuleAgent => ruleAgents = a :: ruleAgents
        case a: ProofAgent => proofAgents = a :: proofAgents
        case a: MetaAgent => metaAgents = a :: metaAgents
        case _ => a.blackboard = null
          throw new IllegalArgumentException("Cannot register: Unknown agent type")
      }
      log("Registered Agent: " + a)
    }catch{
      case _:Throwable =>
        throw new IllegalArgumentException("Agent Blackboard type does not correspond to current BB")
    }
  }

  /** Function that unregisters agents from the blackboard*/
  def unregisterAgent(a: Agent): Unit = {
    a.blackboard = None
    a match {
      case a: RuleAgent => ruleAgents = ruleAgents.diff(List(a))
      case a: ProofAgent => proofAgents = proofAgents.diff(List(a))
      case a: MetaAgent => metaAgents = metaAgents.diff(List(a))
      case _ => a.blackboard = Some(this.asInstanceOf[a.BlackboardType])
        throw new IllegalArgumentException("Cannot unregister: Unknown agent type")
    }
  }

  /** This function runs the specific agent on the registered Blackboard. */
  def runCycle(): Unit = {
    log("running rule agents")
    ruleAgents.foreach(_.run())
    log("running proof agents")
    proofAgents.foreach(_.run())
    log("running meta agents")
    metaAgents.foreach(_.run())
    log("finished cycle")
  }

  /** runs the blackboard for a given number of cycles, stopping if a solution is found */
  def run(cycles: Int = 3): Unit = {
    var i = 0
    while (!finished && i < cycles) {
      runCycle()
      i = i + 1
    }
  }

  override def toString: String = {
    "Blackboard: \n Sections: " + sections +
      "\n RuleAgents: " + ruleAgents +
      "\n ProofAgents: " + proofAgents +
      "\n MetaAgents: " + metaAgents
  }

  /**These are the basic sections which correspond to the various types of tasks*/
  val ruleTaskSection = new RuleTaskSection
  def ruleTasks = ruleTaskSection.data
  val proofTaskSection = new ProofTaskSection
  def proofTasks = proofTaskSection.data
  val metaTaskSection =  new MetaTaskSection
  def metaTasks = metaTaskSection.data

  /** the list which contains all of the sections(data areas) of the blackboard*/
  var sections :List[Section] = List(ruleTaskSection,proofTaskSection,metaTaskSection)

}



/** this class presents the final solution of a specific section of the blackboard*/
abstract class Presenter extends Debugger {
  type ObjectType
  def logPrefix="Presenter"
  def present(pt:ObjectType): String
}
