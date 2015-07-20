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

  val byAgent: Agent
  lazy val blackboard = byAgent.blackboard.get

/*  protected def mkNode(data:A, cong:Boolean=false, sat: Option[Boolean]=None):AndOrTree={
    val pd= new ProofData(data,cong,sat)
    new AndOrTree(pd)
  }*/

}

trait Event{
  var flags: List[String] = Nil
  def hasFlag(f:String): Boolean = flags.contains(f)
  def hasFlag(l:List[String]): Boolean = l.exists(flags.contains(_))
  var readBy: List[Agent] = Nil
  def wasReadBy(a: Agent): Boolean = readBy.contains(a)
}

class Change[T](dataVar:T, flagsVar: List[String]) extends Event{
  val data = dataVar
  flags =flagsVar
}

abstract class RuleTask extends Task  {

  def readList(s: Section):List[s.ObjectType] = Nil
  def writeList(s: Section):List[s.ObjectType] = Nil

  override val byAgent:RuleAgent

  override def toString: String = {//TODO make more intuitice toString function
    listDisplay(blackboard.sections.flatMap({s=>try{s.data.toString}catch{case _: Throwable =>""}}),"ReadSets")
  }
}

abstract class ProofTask extends Task {
  override val byAgent: ProofAgent

  val ruleLists: mutable.Queue[List[RuleTask]] = new mutable.Queue[List[RuleTask]]()

  def readList(s: Section):List[s.ObjectType] ={
    var out:List[s.ObjectType] = Nil
    ruleLists.foreach(rs=>out=out.intersect(rs.flatMap(_.readList(s))))
    out
  }

  def writeList(s: Section):List[s.ObjectType] ={
    var out:List[s.ObjectType] = Nil
    ruleLists.foreach(rs=>out=out.intersect(rs.flatMap(_.writeList(s))))
    out
  }

  override def toString: String = {
    QueueListDisplay(ruleLists,"ProofTask","RuleSet")
  }

  def isApplicable[BB<:Blackboard](b:BB) = {
    ruleLists.forall(rl=>rl.forall(_.isApplicable(b)))
  }

}

abstract class MetaTask extends Task  {
  override val byAgent: MetaAgent

  val proofLists: mutable.Queue[List[ProofTask]] = new mutable.Queue[List[ProofTask]]()

  def readList(s: Section):List[s.ObjectType] ={
    var out:List[s.ObjectType] = Nil
    proofLists.foreach(ps=>out=out.intersect(ps.flatMap(_.readList(s))))
    out
  }

  def writeList(s: Section):List[s.ObjectType] ={
    var out:List[s.ObjectType] = Nil
    proofLists.foreach(ps=>out=out.intersect(ps.flatMap(_.writeList(s))))
    out
  }

  override def toString: String = {
    name + QueueListDisplay(proofLists,"MetaTask","ProofTask")
  }

  def isApplicable[BB<:Blackboard](b:BB) = {
    proofLists.forall(pl=>pl.forall(_.isApplicable(b)))
  }
}

class PTRuleTask(byAgentVar: RuleAgent, nameVar: String) extends RuleTask{
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








