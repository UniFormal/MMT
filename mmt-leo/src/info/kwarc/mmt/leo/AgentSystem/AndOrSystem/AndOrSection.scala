package info.kwarc.mmt.leo.AgentSystem.AndOrSystem

import info.kwarc.mmt.leo.AgentSystem.{Section, Task, Change}

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
  def isApplicable[T<:PTApplicability](t: T):Boolean = {
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
class AndOrTreeSection(g:AndOrTree) extends AndOrSection {
  type ObjectType = AndOrTree
  var data = g
}

