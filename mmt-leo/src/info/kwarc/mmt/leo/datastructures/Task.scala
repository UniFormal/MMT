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
 * Taken heavily from the LeoPARD implementation
 */
trait Task[A] extends Debugger{
  /** Prints a short name of the task */
  val name: String

  def logPrefix = name

  def isApplicable(b:Blackboard[A]):Boolean={
    val wS =  writeSet().exists(!_.isBelowSatisfied)||writeSet().isEmpty
    val wC =  b.proofTree.contains(this.writeSet())
    val rC =  b.proofTree.contains(this.readSet())
    val out = wS&&wC&&rC
    if (!out) {
      log(this.toString+ " is not applicable" )
      if (!wS) {log("All of the write nodes are below a solved goal" )}
      if (!wC) {log("write nodes not contained in goal")}
      if (!rC) {log("read nodes not contained in goal")}
    }
    out
  }

  /** Returns a set of all nodes that are read for the task. */
  def readSet(): Set[ProofTree[A]]

  /** Returns a set of all nodes, that will be written by the task. */
  def writeSet(): Set[ProofTree[A]]

  /**
   * Checks for two tasks, if they are in conflict with each other.
   *
   * @param t2 - Second Task
   * @return true, iff they collide
   */
  def collide(t2: Task[A]): Boolean = {
    val t1 = this
    if (t1 equals t2) true
    else {
      t1.readSet().intersect(t2.writeSet()).nonEmpty ||
        t2.readSet().intersect(t1.writeSet()).nonEmpty ||
        t2.writeSet().intersect(t1.writeSet()).nonEmpty
    }
  }

  protected def mkNode(data:A, cong:Boolean=false, sat: Option[Boolean]=None):ProofTree[A]={
    val pd= new ProofData(data,cong,sat)
    new ProofTree(pd)
  }

}

trait Event[A]{
  var flags: List[String]
  def hasFlag(f:String): Boolean = flags.contains(f)
  def hasFlag(l:List[String]): Boolean = l.exists(flags.contains(_))
}

class Change[A](nodeVar : ProofTree[A], flagsVar: List[String]) extends Event[A]{
  var flags =flagsVar
  var readBy: List[RuleAgent[A]]=Nil
  def wasReadBy(a: RuleAgent[A]): Boolean = readBy.contains(a)
}

abstract class RuleTask[A] extends Task[A] with Event[A] {
  val byAgent: RuleAgent[A]
  var flags: List[String] = Nil
  def readSet(): Set[ProofTree[A]] = Set()
  def writeSet(): Set[ProofTree[A]] = Set()
  var readBy: List[ProofAgent[A]]=Nil
  def wasReadBy(a: ProofAgent[A]): Boolean = readBy.contains(a)

  override def toString: String = {
    name + " \n \t \t \t readSet: " + readSet()
  }
}

abstract class ProofTask[A] extends Task[A] with Event[A] {
  val byAgent: ProofAgent[A]
  var flags:List[String] = Nil
  var readBy: List[MetaAgent[A]]=Nil
  def wasReadBy(a: MetaAgent[A]): Boolean = readBy.contains(a)

  val ruleSets: mutable.Queue[Set[RuleTask[A]]] = new mutable.Queue[Set[RuleTask[A]]]()

  def readSet(): Set[ProofTree[A]] ={
    var out:Set[ProofTree[A]] = Set()
    ruleSets.foreach(s=>out=out.intersect(s.flatMap(_.readSet())))
    out
  }

  def writeSet(): Set[ProofTree[A]] ={
    var out:Set[ProofTree[A]] = Set()
    ruleSets.foreach(s=>out=out.intersect(s.flatMap(_.writeSet())))
    out
  }

  override def toString: String = {
    name + " \n \t \t ruleSets:" + ruleSets
  }
}

abstract class MetaTask[A] extends Task[A] with Event[A] {
  val byAgent: MetaAgent[A]
  var flags: List[String] = Nil

  val proofSets: mutable.Queue[Set[ProofTask[A]]] = new mutable.Queue[Set[ProofTask[A]]]()

  def readSet(): Set[ProofTree[A]] ={
    var out:Set[ProofTree[A]] = Set()
    proofSets.foreach(s=>out=out.intersect(s.flatMap(_.readSet())))
    out
  }

  def writeSet(): Set[ProofTree[A]] ={
    var out:Set[ProofTree[A]] = Set()
    proofSets.foreach(s=>out=out.intersect(s.flatMap(_.writeSet())))
    out
  }

  override def toString: String = {
    name + " \n \t proofSets:" + proofSets
  }
}

class StdRuleTask[A](byAgentVar: RuleAgent[A], nameVar: String) extends RuleTask[A]{
  val name = nameVar
  val byAgent = byAgentVar
}

class StdProofTask[A](byAgentVar: ProofAgent[A], nameVar: String) extends ProofTask[A]{
  val name = nameVar
  val byAgent = byAgentVar
}

class StdMetaTask[A](byAgentVar: MetaAgent[A], nameVar: String) extends MetaTask[A]{
  val name = nameVar
  val byAgent = byAgentVar
}








