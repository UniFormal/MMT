package info.kwarc.mmt.leo.datastructures


import scala.collection.mutable

/**
 * Created by mark on 6/27/15.
 *
 * Common trait for all Agent Task's. Each agent specifies the
 * work it can do.
 * The specific fields and accessors for the real task will be in
 * the implementation.
 *
 */
trait Task extends Debugger{

  /** Prints a short name of the task */
  val name: String
  def logPrefix = name

  /**Determines if a given task is applicable given the current blackboard*/
  def isApplicable[BB<:Blackboard](b: BB):Boolean

  /** Returns a set of all nodes that are read for the task. */
  def readList(s:Section): List[s.ObjectType]

  /** Returns a set of all nodes, that will be written by the task. */
  def writeList(s:Section): List[s.ObjectType]

  /**
   * Checks for two tasks, if they are in conflict with each other.
   *
   * @param that - Second Task
   * @return true, iff they collide
   */
  def collide(that: Task): Boolean = {
    if (that.blackboard == this.blackboard) {
      this.blackboard.sections.forall(s=>
        if (this equals that) true
        else {
          this.readList(s).intersect(that.writeList(s)).nonEmpty ||
            that.readList(s).intersect(this.writeList(s)).nonEmpty ||
            that.writeList(s).intersect(this.writeList(s)).nonEmpty
        }
      )
    }else{false}
  }

  /** The agent which created the task*/
  val byAgent: Agent

  /** The blackboard to which the agent is registered,
    * lazy to avoid null pointer errors
    */
  lazy val blackboard = byAgent.blackboard.get

}

/** Trait which encapsulates an event occurring on the blackboard*/
trait Event{
  var flags: List[String] = Nil
  def hasFlag(f:String): Boolean = flags.contains(f)
  def hasFlag(l:List[String]): Boolean = l.exists(flags.contains(_))
  var readBy: List[Agent] = Nil
  def wasReadBy(a: Agent): Boolean = readBy.contains(a)
}

/** Trait which encapsulates a change in data*/
class Change[T](dataVar:T, flagsVar: List[String]) extends Event{
  val data = dataVar
  flags =flagsVar
}

/** Class which represents a rule task which changes the data on the blackboard*/
abstract class RuleTask extends Task  {

  /** The nodes which the task is operating on
    * the definitions should be overridden in specific implementations
    */
  def readList(s: Section):List[s.ObjectType]
  def writeList(s: Section):List[s.ObjectType]

  override val byAgent:RuleAgent

  override def toString: String = {//TODO make more intuitive toString function
    listDisplay(blackboard.sections.flatMap({s=>try{s.data.toString}catch{case _: Throwable =>""}}),"ReadSets")
  }
}

/** Class which represents a Proof task which calls on rule tasks*/
abstract class ProofTask extends Task {
  override val byAgent: ProofAgent

  /** Queue of lists of rule tasks. Lists of rule taks are parallelizable*/
  val ruleLists: mutable.Queue[List[RuleTask]] = new mutable.Queue[List[RuleTask]]()

  /** @return union of the constituent rule task readLists*/
  //TODO change back to sets it is more natural
  def readList(s: Section):List[s.ObjectType] ={
    var out:List[s.ObjectType] = Nil
    ruleLists.foreach(rs=>out=out.union(rs.flatMap(_.readList(s)).asInstanceOf[List[s.ObjectType]]))
    out.distinct
  }

  /** @return the union of constituent writeLists*/
  def writeList(s: Section):List[s.ObjectType] ={
    var out:List[s.ObjectType] = Nil
    ruleLists.foreach(rs=>out=out.union(rs.flatMap(_.writeList(s)).asInstanceOf[List[s.ObjectType]]))
    out.distinct
  }

  override def toString: String = {
    QueueListDisplay(ruleLists,"ProofTask","RuleSet")
  }

  /** checks if a given task is applicable on the blackboard*/
  def isApplicable[BB<:Blackboard](b:BB) = {
    ruleLists.forall(rl=>rl.forall(_.isApplicable(b)))
  }

}

/** Class which represents a Meta task which calls on proof tasks*/
abstract class MetaTask extends Task  {
  override val byAgent: MetaAgent

  /** List of constituent proof tasks*/
  val proofLists: mutable.Queue[List[ProofTask]] = new mutable.Queue[List[ProofTask]]()

  def readList(s: Section):List[s.ObjectType] ={
    var out:List[s.ObjectType] = Nil
    proofLists.foreach(ps=>out=out.union(ps.flatMap(_.readList(s)).asInstanceOf[List[s.ObjectType]]))
    out.distinct
  }

  def writeList(s: Section):List[s.ObjectType] ={
    var out:List[s.ObjectType] = Nil
    proofLists.foreach(ps=>out=out.union(ps.flatMap(_.writeList(s)).asInstanceOf[List[s.ObjectType]]))
    out.distinct
  }

  override def toString: String = {
    name + QueueListDisplay(proofLists,"MetaTask","ProofTask")
  }

  def isApplicable[BB<:Blackboard](b:BB) = {
    proofLists.forall(pl=>pl.forall(_.isApplicable(b)))
  }
}

abstract class PTRuleTask(byAgentVar: RuleAgent, nameVar: String) extends RuleTask{
  val name = nameVar
  val byAgent = byAgentVar

  def isApplicable[BB<:Blackboard](bb:BB):Boolean ={
    bb match {
      case b:AndOrBlackboard[_] =>
        b.proofSection.isApplicable(this)
      case _ => throw new IllegalArgumentException("Not a valid blackboard type")
    }
  }

}

class PTProofTask(byAgentVar: ProofAgent, nameVar: String) extends ProofTask{
  val name = nameVar
  val byAgent = byAgentVar

}

class PTMetaTask(byAgentVar: MetaAgent, nameVar: String) extends MetaTask{
  val name = nameVar
  val byAgent = byAgentVar

}








