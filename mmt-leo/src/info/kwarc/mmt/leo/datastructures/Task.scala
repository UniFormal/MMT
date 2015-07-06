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
trait Task[A] {
  /** Prints a short name of the task */
  var name: String

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

class RuleTask[A] extends Task[A] with Event[A] {
  var name = "Not yet named"
  var byAgent: RuleAgent[A] = null
  var flags: List[String] = Nil
  def readSet(): Set[ProofTree[A]] = Set()
  def writeSet(): Set[ProofTree[A]] = Set()
  var readBy: List[ProofAgent[A]]=Nil
  def wasReadBy(a: ProofAgent[A]): Boolean = readBy.contains(a)
}

class ProofTask[A] extends Task[A] with Event[A] {
  var name = "Not yet named"
  var byAgent: ProofAgent[A] = null
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
}

class MetaTask[A] extends Task[A] with Event[A] {
  var name = "Not yet named"
  var byAgent: MetaAgent[A] = null
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
}






