package info.kwarc.mmt.leo.AgentSystem.AndOrSystem

import info.kwarc.mmt.api.frontend.{Controller, Logger}
import info.kwarc.mmt.leo.AgentSystem.{Display, Debugger}

/**
 * This trait represents thread safe capabilities.
 * //TODO functions still need to be modified in tree to utilize thread safety
 */

abstract class Tree[T<:Tree[T]](implicit controller: Controller) extends Logger with Lockable {self: T =>
  val report = controller.report
  def logPrefix = "Tree"

  var parent: Option[T] = None
  var children: List[T] = Nil

  var isDeleted = false

  def getParent = parent

  /**sets the parent, safety = true involves a check if the parentVar is above current node*/
  def setParent(parentVar:Option[T], safety:Boolean=true):Unit = {
    (parent,parentVar) match {
      case (Some(p1),Some(p2)) =>
        if (safety && this.isAbove(p2)){throw new IllegalArgumentException("New Parent below current")}
        parent=parentVar; p1.children.diff(List(this))
      case (Some(p1),None) => parent=parentVar; p1.children=p1.children.diff(List(this))
      case (None,Some(p2)) =>
        if (safety && isAbove(p2)){throw new IllegalArgumentException("New Parent below current")}
        parent=parentVar
      case (None,None) =>
    }
  }

  /**set parent for parent node instead of option*/
  private def setParent(parentVar:T):Unit = setParent(Option(parentVar))

  def getChildren = children
  def setChildren(c:List[T]) = c.foreach{_.setParent(this)}

  def setDeleted() = isDeleted=true; children.foreach(_.setDeleted())

  /** @return the root node of the tree, aka the node with no parents
    * Does not use getters and setters because may be called inside
    * of a locked subtree*/
  def root:T = {
    parent match {
      case Some(p) => p.root
      case None => this
    }
  }

  /** @return the siblings of the current node aka the nodes that share its parent*/
  def siblings: List[T] = {parent match{
      case None => Nil
      case Some(p) => p.getChildren diff List(this)}
  }

  /** @return the path of nodes to the root of the tree*/
  def path:List[T] = {parent match {
    case Some(p) => this::p.path
    case None => Nil}
  }

  /** @return the number of nodes above the current node*/
  //TODO improve speed
  def depth: Int = {path.length}

  /** checks if current node is equal to or below target node*/
  def isBelow(that: T): Boolean = this == that || parent.exists(_ isBelow that)
  /** checks if current node is equal to or above target node*/
  def isAbove(that: T): Boolean = this == that || that.isBelow(this)
  /** checks if current node is equal to or above a set of nodes  */
  def isAbove(s:Set[T]):Boolean = {s.forall(n => this isAbove n)}
  /** checks if current node is the child of another*/
  def isChildOf(that: T): Boolean = parent.get==that
  /** checks if current node is a parent of another*/
  def isParentOf(that: T): Boolean = that.isChildOf(this)


  /** Adds a child to the node*/
  def addChild(child : T):Unit = {
    if (child.isAbove(this)) {
      throw new IllegalArgumentException("Child node above the current")
    }
    child.parent match {
      case Some(c) => child.parent.get.disconnectChild(child)
      case None =>
    }
    child.setParent(this)
    children = child::children
  }

  /** Adds a list of children to the node*/
  def addChildren(lChild : List[T]):Unit = {lChild.foreach(this.addChild)}

  /** disconnects target from the node*/
  def disconnectChild(child : T):Unit={
    val newChildren = children.filter( c => c != child)
    if (newChildren.length == children.length) {
      throw new IllegalArgumentException("target child not found")
    }
    children = newChildren
    child.setDeleted()
    child.parent = None
  }

  /** disconnects all children from the node*/
  def disconnectChildren(lChild : List[T] = children): Unit = {lChild.foreach(this.disconnectChild)}

  /** deletes current node**/
  def disconnect() = {
    parent match {
      case Some(p)=>p.disconnectChild(this)
      case _ =>
    }
    parent = None
    this.setDeleted()
  }


  /** traverses the tree depth first performs an action as it comes to the node*/
  def preDepthTraverse(visit: T => Unit): Unit = {
    def recur(n: T ): Unit = {
      visit(n)
      for (r <- n.children) {
        recur(r)
      }
    }
    recur(this)
  }

  /** traverses the tree depth first and performs an action after it reaches the leaves */
  def postDepthTraverse(visit: T => Unit): Unit = {
    def recur(n: T ): Unit = {
      for (r <- n.children) {
        recur(r)
      }
      visit(n)
    }
    recur(this)
  }

  /** flattens the tree into the preDepth order*/
  def preDepthFlatten:List[T] ={
    var out:List[T] = Nil
    preDepthTraverse({n=>out=n::out; Unit})
    out
  }

  /** @return a list of leaves, aka nodes without children*/
  def leaves: List[T] = {preDepthFlatten.filter(_.children==Nil)}

  /** @return the height of the tree*/
  def height: Int = {
    children.foldLeft(-1) { (h, c) => h max c.height } + 1
  }

  /** @return the number of nodes in the tree */
  def size: Int = {
    children.foldLeft(0) { (s, c) => s + c.size } + 1
  }

  /** the definition of node to node equivalence used in @isEquivTo*/
  def nodeEquivalence(that : T): Boolean = {
    that.children.length == this.children.length
  }

  /** structural and component-wise equivalence
    * returns true if the two trees have equivalent nodes and connections*/
  def isEquivTo(that : T): Boolean ={
    if (this.nodeEquivalence(that)) {
      (this.children zip that.children).forall(p=>p._1.isEquivTo(p._2))
    }else{
      false
    }
  }

  /** @return a tree with the given root and given children*/
  def mkTree[B<:Tree[B]](root: B, lChild: List[B]): B ={
    root.addChildren(lChild)
    root
  }

  /** @return a tree with mapping applied to each node*/
  def map[B<:Tree[B]](f: T => B): B =
    f(this).mkTree(f(this), children map (_ map f))

  /** @return a copy of the current tree*/
  def copy:T={map({n:T=>n})}
}

/**
 * This trait represents and/or capabilities.
 */
trait AndOr[T<:AndOr[T]] extends Tree[T] with Lockable { Self: T =>
  override def logPrefix = "AndOrTree"

  /** conj=true -> an AND node*/
  var conj: Boolean

  /** sat = None -> unknown, isSat= True/False -> Solved or Proven Unsolvable*/
  var sat: Option[Boolean]

  def isAnd = conj
  def isOr = !conj


  def isUnsat = !this.sat.getOrElse(true)
  def isSolved = sat.getOrElse(false)


  def update(conjVar: Boolean, isSatVar: Option[Boolean]) ={
    this.conj = conjVar
    this.sat = isSatVar
  }

  /** Returns true if two nodes are equivalent with respect to their values*/
  override def nodeEquivalence(that: T): Boolean = {
    super.nodeEquivalence(that) &&
    that.conj == this.conj &&
    that.sat == this.sat
  }

  /** changes the data field in the node*/
  def setSat( bool: Boolean) {
    sat = Some(bool)
  }

  /** converts only a single node to a string */
  def present: String ={
    "\t"*depth + "isAnd: "+isAnd.toString+" isSatisfiable: "+sat.toString
  }

  /** returns a nice looking printable string **/
  override def toString: String = {
    var out = ""
    preDepthTraverse{sn=>out = out+"\n"+ sn.present}
    out
  }

  /** returns leaves that are not satisfied or proven unsatisfiable*/
  def openLeaves: List[T] = leaves.filter(_.sat.isEmpty)

  /** determines if this node is below a satisfied node*/
  def isBelowSatisfied:Boolean = path.exists(_.sat.isDefined)

  /** simplifies the ProofTree for presentation*/
  def simplify():Unit = {
    if (this.children==Nil) {return}
    if ((isAnd && isUnsat) || (isOr && isSolved)) {
      val applicableChildren = children.filter(c => c.sat==this.sat)
      val smallest = applicableChildren.map(_.depth).min
      val bestChild = applicableChildren.filter(_.depth==smallest).head
      (children diff List(bestChild)).foreach(_.disconnect())
    }
    children.foreach(_.simplify())
  }

  /** propagates the effect of the solved node up the tree, then simplifies the resulting part of the tree*/
  def percolate(trim:Boolean=true):Unit = {
    log("P Called on" + this,Some("3"))
    if (isSolved) {
      this.parent match {
        case Some(p) if p.isOr || p.children.forall(_.isSolved) =>
          p.setSat(true)
          p.percolate()
        case _ if trim => simplify()
        case _ => return
      }
    }
    if (isUnsat) {
      this.parent match {
        case Some(p) if p.isAnd || p.children.forall(_.isUnsat) =>
          p.setSat(false)
          p.percolate()
        case _ if trim => simplify()
        case _ => return
      }
    }
  }

}

/**
 * Companion class for AndOr
 *
 * @param conjVar if true this signifies an and node and all subnodes
 *                        must be solved for the node to be satisfied. If false
 *                        it is an or node and only one subnode needs to be satisfied
 * @param isSatVar is true if it is solved, is None if it is unknown, is false if it is unsatasfiable
 */
class AndOrTree(conjVar:Boolean, isSatVar: Option[Boolean]=None)(implicit controller: Controller) extends AndOr[AndOrTree] {
  var conj=conjVar
  var sat=isSatVar

  def apply(conjVar: Boolean, isSatVar: Option[Boolean]) ={
    this.conj = conjVar
    this.sat = isSatVar
  }

  def unapply(t:AndOrTree):Option[(Boolean,Option[Boolean])] = {
    Some((t.conj, t.sat))
  }

}







