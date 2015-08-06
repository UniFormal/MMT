package info.kwarc.mmt.leo.AgentSystem

import info.kwarc.mmt.api.frontend.{Controller, Logger}


abstract class Message(implicit c: Controller,oLP:String) extends Logger{
  val flags: List[String]
  val report = c.report
  def logPrefix = oLP+"#Message"

  def hasFlag(f:String): Boolean = flags.contains(f)
  def hasFlag(l:List[String]): Boolean = l.exists(flags.contains(_))
  val sentBy: Any
}

/** Sent to agents whose bid failed in the auction*/
case class AuctionFailure(sentByVar:AuctionAgent, task:Task, flagsVar: List[String]=Nil)(implicit c: Controller,oLP:String) extends Message {
  override def logPrefix = oLP+"#AuctionFailure"
  val flags: List[String] = flagsVar
  override val sentBy = sentByVar
}

/** Trait which encapsulates a change in data*/
case class Change[T](s: Section, dataVar:T, flagsVar: List[String])(implicit c: Controller,oLP:String) extends Message {
  override def logPrefix = oLP+"#Change"

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
abstract class Task(implicit c: Controller,oLP:String) extends Message{

  val flags = Nil

  /** Prints a short name of the task */
  val name: String
  override def logPrefix = oLP+"#"+name

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
      this.blackboard.get.sections.exists(s=>
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
  override val sentBy: Agent

  lazy val priority = sentBy.priority

  /** The blackboard to which the agent is registered,
    * lazy to avoid null pointer errors
    */
  lazy val blackboard = sentBy.blackboard

}

/** Class which represents a Meta task which calls on proof tasks*/
case class MetaTask(taskSet:Set[Task], byAgentVar: Agent, nameVar: String)(implicit c: Controller,oLP:String) extends Task  {
  override def logPrefix = oLP+"#MetaTask"
  
  val name = nameVar
  override val sentBy:Agent = byAgentVar

  //TODO implement parallelization
  def execute():Boolean = taskSet.toList.sortBy(-_.priority).map({t=> //TODO ordering is a hack so fix this
    if (t.isApplicable(this.blackboard.get)){
      log("Executing Task: " +t)
      t.execute()
    }else{
      log("Task inapplicable")
      false
    }
  }).forall(b=>b)

  def readSet(s: Section):Set[s.ObjectType] ={
    var out:Set[s.ObjectType] = Set.empty[s.ObjectType]
    taskSet.foreach(t=>out=out.union(t.readSet(s).asInstanceOf[Set[s.ObjectType]]))
    out
  }

  def writeSet(s: Section):Set[s.ObjectType] ={
    var out:Set[s.ObjectType] = Set.empty[s.ObjectType]
    taskSet.foreach(t=>out=out.union(t.writeSet(s).asInstanceOf[Set[s.ObjectType]]))
    out
  }

  override def toString: String = {
    name + Display.setDisplay(taskSet,"Tasks")
  }

  override def isApplicable[BB<:Blackboard](b:BB) = {
    super.isApplicable(b) &&  taskSet.forall(_.isApplicable(b))
  }
}
