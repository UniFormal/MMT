package info.kwarc.mmt.leo.AgentSystem

import info.kwarc.mmt.api.frontend.Logger


abstract class Message extends Logger{
  val flags: List[String]

  def hasFlag(f:String): Boolean = flags.contains(f)
  def hasFlag(l:List[String]): Boolean = l.exists(flags.contains(_))
  val sentBy: Speaker
}

/** Sent to agents whose bid failed in the auction*/
case class AuctionFailure(sentByVar:AuctionAgent, task:Task, flagsVar: List[String]=Nil) extends Message {
  def report = sentByVar.blackboard.report
  def logPrefix = sentByVar.blackboard.OLP+"#AuctionFailure"

  val flags: List[String] = flagsVar
  override val sentBy = sentByVar
}

/** Trait which encapsulates a change in data*/
case class Change[T](s: Section, dataVar:T, flagsVar: List[String]) extends Message {
  def report = s.report
  def logPrefix = s.OLP+"#Change"

  val flags: List[String] = flagsVar
  val data = dataVar
  override val sentBy: Section = s
}


/**
 * Created by mark on 6/27/15.
 *
 * Common trait for all Agent Task's. Each agent specifies the
 * work it can do.
 * The specific fields and accessors for the real task will be in
 * the implementation.
 *
 */
abstract class Task extends Message{

  val name: String
  val flags = Nil

  def report = blackboard.report
  def logPrefix = blackboard.OLP+"#"+name

  /**Determines if a given task is applicable given the current blackboard*/
  def isApplicable[BB<:Blackboard](b: BB):Boolean = !b.isTerminated

  /** Returns a set of all nodes that are read for the task. */
  def readSet(s:Section): Set[s.ObjectType]

  /** Returns a set of all nodes, that will be written by the task. */
  def writeSet(s:Section): Set[s.ObjectType]
  
  def execute():Boolean

  /**
   * Checks for two tasks, if they are in conflict with each other.
   *
   * @param that - Second Task
   * @return true, iff they collide
   */
  def collide(that: Task): Boolean = {
    if (that.blackboard == this.blackboard) {
      this.blackboard.sections.exists(s=>
        if (this equals that) true
        else {
          this.readSet(s).intersect(that.writeSet(s)).nonEmpty ||
           that.readSet(s).intersect(this.writeSet(s)).nonEmpty ||
           that.writeSet(s).intersect(this.writeSet(s)).nonEmpty
        }
      )
    }else{false}
  }

  /** The agent which created the task*/
  val sentBy: Agent

  def priority = sentBy.priority

  /** The blackboard to which the agent is registered,
    * lazy to avoid null pointer errors
    */
  def blackboard = sentBy.blackboard

}

/** Class which represents a Meta task which calls on proof tasks*/
case class MetaTask(taskList:List[Task], byAgentVar: Agent) extends Task  {

  val name = "MetaTask"
  override val sentBy:Agent = byAgentVar

  //TODO implement parallelization
  def execute():Boolean = taskList.map({t=>
    if (t.isApplicable(this.blackboard) && !t.sentBy.terminated){
      log("Executing Task: " +t)
      t.execute()
    }else{
      log("Task inapplicable")
      false
    }
  }).forall(b=>b)

  def readSet(s: Section):Set[s.ObjectType] ={
    var out:Set[s.ObjectType] = Set.empty[s.ObjectType]
    taskList.foreach(t=>out=out.union(t.readSet(s).asInstanceOf[Set[s.ObjectType]]))
    out
  }

  def writeSet(s: Section):Set[s.ObjectType] ={
    var out:Set[s.ObjectType] = Set.empty[s.ObjectType]
    taskList.foreach(t=>out=out.union(t.writeSet(s).asInstanceOf[Set[s.ObjectType]]))
    out
  }

  override def toString: String = {
    name + Display.listDisplay(taskList,"Tasks")
  }

  override def isApplicable[BB<:Blackboard](b:BB) = {
    super.isApplicable(b) &&  taskList.forall(_.isApplicable(b))
  }
}
