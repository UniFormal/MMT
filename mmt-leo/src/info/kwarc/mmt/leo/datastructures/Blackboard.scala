package info.kwarc.mmt.leo.datastructures

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


/**
 * This class represents a blackboard specialized for proofTrees of any type
 *
 * @param g goal for the blackboard to solve
 * @tparam T type of proof tree that the goal represents
 */
class AndOrBlackboard[T>:Null <:AndOr[T]](g:T) extends Blackboard {

  /** this is the proof section which houses the proof treee
    * beginning with the goal node
    */
  val proofSection = new AndOrSection{type ObjectType = T; var data=g}
  def proofTree = proofSection.data
  sections =proofSection::sections
}

/** the abstract type of a section of the Blackboard,
  * intended to hold and monitor changes in the stored data
  */
trait Section extends Debugger {
  val logPrefix = "Section"

  /** the type of data that the section holds*/
  type ObjectType
  var data : ObjectType

  /**the list of changes which mark operations on the stored data */
  var changes : List[Change[_]]

  override def toString: String = {data.getClass.toString + " Section"}

  /** function that updates the stored data and adds a change to the changes list*/
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

  /** this type of section only stores data which is a subtype of the AndOr tree type*/
  type ObjectType>: Null <:AndOr[ObjectType]

  type PTType=ObjectType //Meaningful alias for object type
  var data:PTType
  var changes: List[Change[_]] = Nil

  /** function that updates a node of the proof tree and adds a change to the changelist*/
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
   * Appends a tree to the specified root
   * @param root root node to attach new proof
   * @param tree tree to attach to root
   */
  def addTree(root: PTType, tree : PTType) : Unit = {
    root.addChild(tree)
    changes = new Change(tree,List("ADD")) :: changes
  }

  /**
   * Removes the target node from the proof tree
   */
  def removeTree(tree : PTType) : Unit = {
    tree.disconnect()
    changes = new Change(tree,List("DEL")) :: changes
  }

  /** Returns a List of all nodes of the sections's proof tree
   * @return All nodes in the sections data.
   */
  def getNodes : List[PTType] = data.preDepthFlatten

  /**Function that locks nodes affected by a given task*/
  def lockNodes[T<:Task](task: T):Boolean = {
    val resultsW = task.writeList(this).map(_.placeLock(readLockVar=true,writeLockVar=true))
    val resultsR = task.readList(this).map(_.placeLock(readLockVar=false,writeLockVar=true))
    (resultsW++resultsR).forall(b=>b)
  }

  /**Function that unlocks nodes affected by a given task*/
  def unlockNodes[T<:Task](task: T):Unit = {
    task.writeList(this).foreach(_.liftLock(readLockVar=false,writeLockVar=false))
    task.readList(this).foreach(_.liftLock(readLockVar=false,writeLockVar=false))
  }

  /** function that determines if a task is still applicable to the given tree
    * if it is not applicable it logs why
    * @param t a rule task which may or may not be applicable to the given tree
    * @return boolean representing applicability
    */
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

/** Class for the section specific to the AndOrTree class*/
class ProofTreeSection(g:AndOrTree) extends AndOrSection {
  type ObjectType = AndOrTree
  var data = g
}

/** Class for the section specific to the DataTree[D] class*/
class DataTreeSection[D](g:DataTree[D]) extends AndOrSection {
  type ObjectType = DataTree[D]
  var data = g
}
/*object DataTreeSection{
  def unapply[B](t:DataTreeSection[B]) = {
    Some(t.data)
  }
}*/

/**
 * This section handles the lists of various tasks
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

/** these classes are the specific Sections representing the various tasks*/
class RuleTaskSection extends TaskSection { type TaskType = RuleTask}
class ProofTaskSection extends TaskSection { type TaskType = ProofTask}
class MetaTaskSection extends TaskSection { type TaskType = MetaTask}

/** this class presents the final solution of a specific section of the blackboard*/
abstract class Presenter extends Debugger {
  type ObjectType
  def logPrefix="Presenter"
  def present(pt:ObjectType): String
}



  

