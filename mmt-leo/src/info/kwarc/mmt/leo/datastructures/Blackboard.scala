package info.kwarc.mmt.leo.datastructures

/**
 * A blackboard is a central data collection object that supports
 * synchronized access between multiple processes.
 *
 * The implementation decides over the fairness and order of execution of the
 * processes.
 */
abstract class Blackboard extends Debugger {
  def logPrefix = "Blackboard"

  var finished: Boolean = false


  var ruleAgents: List[RuleAgent] = Nil
  var proofAgents: List[ProofAgent] = Nil
  var metaAgents: List[MetaAgent] = Nil

  var ruleSeq: Seq[RuleTask] = Nil
  var proofSeq: Seq[ProofTask] = Nil
  var metaSeq: Seq[MetaTask] = Nil

  def registerAgent(a: Agent): Unit = {
    a.blackboard = Some(this.asInstanceOf[a.BlackboardType])
    a match {
      case a: RuleAgent => ruleAgents = a :: ruleAgents
      case a: ProofAgent => proofAgents = a :: proofAgents
      case a: MetaAgent => metaAgents = a :: metaAgents
      case _ => a.blackboard = null
        throw new IllegalArgumentException("Cannot register: Unknown agent type")
    }
    log("Registered Agent: " + a)
  }

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

  var sections: List[Section]
}


class AndOrBlackboard[T>:Null <:AndOr[T]](g:T) extends Blackboard {

  val proofSection = new AndOrSection{type ObjectType = T; var data=g}

  def proofTree = proofSection.data

  val ruleTaskSection = new RuleTaskSection
  def ruleTasks = ruleTaskSection.data
  val proofTaskSection = new ProofTaskSection
  def proofTasks = proofTaskSection.data
  val metaTaskSection =  new MetaTaskSection
  def metaTasks = metaTaskSection.data

  var sections :List[Section] = List(proofSection,ruleTaskSection,proofTaskSection,metaTaskSection)

}



/** the abstract type of a section of the Blackboard,
  * intended to hold and monitor changes in the stored*/
trait Section extends Debugger {
  val logPrefix = "Section"
  type ObjectType
  var data : ObjectType
  var changes : List[Change[_]]
  //def interestedAgents: List[Agent]
  override def toString: String = {data.getClass.toString + " Section"}

  def update(newData:ObjectType, flags:List[String] = List("CHANGE")) = {
    changes = new Change((data,newData),flags) :: changes
    data = newData
  }

}


/**
 * This trait capsules the formulas responsible for the formula manipulation of the
 * blackboard.
 *
 */
abstract class AndOrSection extends Section {
  override val logPrefix ="AndOrSection"
  type ObjectType>: Null <:AndOr[ObjectType]

  type PTType=ObjectType //Convenient alias
  var data:PTType
  var changes: List[Change[_]] = Nil

  def update(oldNode:PTType,newNode:PTType) = {
    oldNode.parent match {
      case Some(p) => oldNode.disconnect(); p.addChild(newNode)
      case None => data=newNode
    }
    changes = new Change((oldNode,newNode),List("CHANGE")) :: changes
  }

  def apply(goal: PTType) = {
    data = goal
    changes = List(new Change(goal,List("ADD")))
  }


  /**
   * Adds a ProofTree to the blackboard, if it does not exist. If it exists
   * the old formula is returned.
   *
   * @param root root node to attach new proof
   * @param tree tree to attach to root
   */
  def addTree(root: PTType, tree : PTType) : Unit = root.addChild(tree)

  /**
   * Removes a formula from the Set fo formulas of the Blackboard.
   */
  def removeTree(tree : PTType) : Unit = tree.disconnect()

  /** Returns a List of all nodes of the Blackboard's proof tree 
   * @return All formulas of the blackboard.
   */
  def getNodes : List[PTType] = data.preDepthFlatten

  def lockNodes[T<:Task](task: T):Boolean = {
    val resultsW = task.writeList(this).map(_.placeLock(readLockVar=true,writeLockVar=true))
    val resultsR = task.readList(this).map(_.placeLock(readLockVar=false,writeLockVar=true))
    (resultsW++resultsR).forall(b=>b)
  }

  def unlockNodes[T<:Task](task: T):Unit = {
    task.writeList(this).foreach(_.liftLock(readLockVar=false,writeLockVar=false))
    task.readList(this).foreach(_.liftLock(readLockVar=false,writeLockVar=false))
  }

  def isApplicable(t: PTRuleTask):Boolean = {
    val wS = t.writeList(this).exists(!_.isBelowSatisfied) || t.writeList(this).isEmpty
    val wC = data.isAbove(t.writeList(this))
    val rC = data.isAbove(t.readList(this))
    val out = wS && wC && rC
    if (!out) {
      log(this.toString + "\n is not applicable because...")
      if (!wS) {
        log("All of the write nodes are below a solved goal")
      }
      if (!wC) {
        log("write nodes not contained in goal")
      }
      if (!rC) {
        log("read nodes not contained in goal")
      }
    }
    out
  }


}

class ProofTreeSection(g:AndOrTree) extends AndOrSection {
  type ObjectType = AndOrTree
  var data = g
}

class DataTreeSection[D](g:DataTree[D]) extends AndOrSection {
  type ObjectType = DataTree[D]
  var data = g
}
/**
 * This trait capsules the message handling for the blackboard
 */
class TaskSection extends Section {

  type TaskType<:Task
  type ObjectType = List[TaskType]

  var data : ObjectType = Nil
  var changes: List[Change[_]] = Nil

  def add(task: TaskType) = {
    data = task::data
    changes = List(new Change(task,List("ADD")))
  }
}

class RuleTaskSection extends TaskSection { type TaskType = RuleTask}
class ProofTaskSection extends TaskSection { type TaskType = ProofTask}
class MetaTaskSection extends TaskSection { type TaskType = MetaTask}

abstract class Presenter extends Debugger {
  type ObjectType
  def logPrefix="Presenter"
  def present(pt:ObjectType): String
}



  

