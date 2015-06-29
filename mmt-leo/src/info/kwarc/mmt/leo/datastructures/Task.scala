package info.kwarc.mmt.leo.datastructures

/**
 * Common trait for all Agent Task's. Each agent specifies the
 * work it can do.
 * The specific fields and accessors for the real task will be in
 * the implementation.
 *
 * Taken heavily from the LeoPARD implementation
 */
abstract class Task[A] {
  /** Prints a short name of the task */
  def name: String

  /** the level of the task: 0 is for a proof based task, 1 is for an agent based task */
  def level: Int

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

  /**
   * Defines the gain of a Task, defined for
   * a specific agent.
   *
   * @return - Possible profit, if the task is executed
   */
  def bid(budget: Double): Double

  def byAgent: Agent[A]
}




trait Result[A] {


  /**
   * A set of new formulas created by the task.
   * @return New formulas to add. The first coordinate is the intended parent of the second coordinate
   */
  def newFormula(): Set[(ProofTree[A], ProofTree[A])]

  /** A mapping of formulas to be changed. */
  def updateFormula(): Map[ProofTree[A], ProofTree[A]]

  /**
   * A set of formulas to be removed.
   * @return Deleted formulas
   */
  def removeFormula(): Set[ProofTree[A]]

}

/**
 * Simple container for the implementation of result.
 * @param nf - New formulas
 * @param uf - Update formulas
 * @param rf - remove Formulas
 */
class StdResult[A](nf : Set[(ProofTree[A],ProofTree[A])], uf : Map[ProofTree[A],ProofTree[A]], rf : Set[ProofTree[A]]) extends Result[A]{
  override def newFormula() : Set[(ProofTree[A],ProofTree[A])] = nf
  override def updateFormula() : Map[ProofTree[A],ProofTree[A]] = uf
  override def removeFormula() : Set[ProofTree[A]] = rf
}

class EmptyResult[A] extends Result[A]{
  override def newFormula() : Set[(ProofTree[A],ProofTree[A])] = Set.empty
  override def updateFormula() : Map[ProofTree[A],ProofTree[A]] = Map.empty
  override def removeFormula() : Set[ProofTree[A]] = Set.empty
}





