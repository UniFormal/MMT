package info.kwarc.mmt.leo.AgentSystem.AndOrSystem

import info.kwarc.mmt.leo.AgentSystem.{Section, Task, Change}

/**
 * This trait capsules the formulas responsible for the formula manipulation of the
 * blackboard.
 *
 */
class AndOrSection[G>: Null <:AndOr[G]](blackboard: AndOrBlackboard[_],g:G) extends Section(blackboard) {

  override val logPrefix ="AndOrSection"

  /** this type of section only stores data which is a subtype of the AndOr tree type*/
  type ObjectType = G
  type PTType=ObjectType //Meaningful alias for object type
  var data:PTType = g
  var changes: List[Change[_]] = List(new Change(this,g,List("ADD")))

  /** function that updates a node of the proof tree and adds a change to the changelist*/
  def update(oldNode:PTType,newNode:PTType): Unit = {
    oldNode.parent match {
      case Some(p) => oldNode.disconnect(); p.addChild(newNode)
      case None => data=newNode
    }
    handleChange(new Change(this,(oldNode,newNode),List("CHANGE")))
  }

  def update(node:PTType,f:PTType=>PTType):Unit = {
    update(node,f(node))
  }


  /**
   * Appends a tree to the specified root
   * @param root root node to attach new proof
   * @param tree tree to attach to root
   */
  def addTree(root: PTType, tree : PTType) : Unit = {
    root.addChild(tree)
    handleChange(new Change(this,tree,List("ADD")))
  }

  /**
   * Removes the target node from the proof tree
   */
  def removeTree(tree : PTType) : Unit = {
    tree.disconnect()
    handleChange(new Change(this,tree,List("DEL")))
  }

  /** Returns a List of all nodes of the sections's proof tree
   * @return All nodes in the sections data.
   */
  def getNodes : List[PTType] = data.preDepthFlatten

  /**Function that locks nodes affected by a given task*/
  def lockNodes[T<:Task](task: T):Boolean = {
    val resultsW = task.writeSet(this).map(_.placeLock(readLockVar=true,writeLockVar=true))
    val resultsR = task.readSet(this).map(_.placeLock(readLockVar=false,writeLockVar=true))
    (resultsW++resultsR).forall(b=>b)
  }

  /**Function that unlocks nodes affected by a given task*/
  def unlockNodes[T<:Task](task: T):Unit = {
    task.writeSet(this).foreach(_.liftLock(readLockVar=false,writeLockVar=false))
    task.readSet(this).foreach(_.liftLock(readLockVar=false,writeLockVar=false))
  }

  /** function that determines if a task is still applicable to the given tree
    * if it is not applicable it logs why
    * @param t a rule task which may or may not be applicable to the given tree
    * @return boolean representing applicability
    */
  def isApplicable[T<:PTApplicability](t: T):Boolean = {
    val wS = t.writeSet(this).exists(!_.isBelowSatisfied) || t.writeSet(this).isEmpty
    val wC = data.isAbove(t.writeSet(this))
    val rC = data.isAbove(t.readSet(this))
    val d = t.writeSet(this).forall(!_.isDeleted) || t.readSet(this).forall(!_.isDeleted)
    val applicable = wS && wC && rC && d
    if (!applicable) {
      log(t.toString + "\n is not applicable.")
      if (!wS) {
        log("all of the write nodes are below a solved goal",2)
      }
      if (!wC) {
        log("write nodes not contained in goal",2)
      }
      if (!rC) {
        log("read nodes not contained in goal",2)
      }
      if (!d) {
        log("some nodes have been deleted",2)
      }
    }
    applicable
  }
}

