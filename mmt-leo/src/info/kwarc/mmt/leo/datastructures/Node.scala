package datastructures

import scala.collection.immutable.Queue

/**
 * The node class represents a n-tree with arbitrary data at the node.
 *
 * @param dataVar is the data stored at the node
 *
 */
class Node[A](var dataVar: A) {
  var root: Option[Node[A]] = None
  var children: List[Node[A]] = Nil
  var data = dataVar
  /** unapply function matches data */
  def unapply(n:Node[A]):A = n.data

  /** returns the path of nodes to the root of the tree*/
  def path: List[Node[A]] = this :: root.map(_.path).getOrElse(Nil)

  /** returns the number of nodes above the current node*/
  def depth: Int = {
    this.path.length-1
  }

  /** componentwise equivalence*/
  def isEquivTo(n: Node[A]):Boolean ={
    (this.children,n.children) match {
      case (Nil,Nil) => this.data == n.data
      case (l1,l2) =>
        if (l1.length != l2.length) {return false}
        for(x <- 0 to l1.length-1){
          if (l1(x).isEquivTo(l2(x))== false) {return false}
        }
        return true
      case _ => false
    }
  }

  /** checks if a node is equal to or below the current node*/
  def isBelow(that: Node[A]): Boolean = this == that || root.exists(_ isBelow that)
  /** checks if a node is equal to or above the current node*/
  def isAbove(that: Node[A]): Boolean = this == that || children.exists(_ isAbove that)
  /** checks if a node is the child of another*/
  def isChildOf(that: Node[A]): Boolean = root.get==that
  /** checks if a node is a parent of another*/
  def isParentOf(that: Node[A]): Boolean = this.children.contains(that)

  /** changes the data field in the node*/
  def setData(newData: A) {
    this.data = newData
  }

  /** Adds a child from the node*/
  def addChild(child :Node[A]):Unit={
    child.root match{
      case Some(c) => child.root.get.removeChild(child)
      case None =>
    }
    child.root = Some(this)
    this.children = this.children ::: List(child)
  }

  /** removes a specific child from the node*/
  def removeChild(child :Node[A]):Unit={
    this.children = this.children.filter( p => p.root.get != child)
    child.root = None
  }

  /** removes all children from the node*/
  def removeChildren: Unit = {
    this.children foreach {_.root = None}
    this.children = Nil
  }

  /** sets the Root of the current node*/
  def setRoot(that: Node[A]): Unit ={
    if (isAbove(that)) {
      throw new IllegalArgumentException("Current node already above the attempted root")
    }
    this.root match{
      case Some(c) => this.root.get.removeChild(this)
      case None =>
    }
    this.root = Some(that)
    List(this):::that.children
  }

  /** returns a nice looking printable string **/
  override def toString: String = {
    var output = ""
    this.preorderDepth { subnode :Node[A] =>
      output = output++ "\n"+"\t"*subnode.depth + subnode.data.toString
    }
    output+ "\n"
  }

  /** maps a tree of one datatype to a tree of another*/
  def map[B](f: A => B): Node[B] = {
    var fthis = new Node[B](f(this.data))
    def recur(n : Node[A],fn: Node[B]): Unit = {
      for (r <- n.children) {
        val addition = new Node[B](f(r.data))
        fn.addChild(addition)
        recur(r,addition)
      }
    }
    recur(this,fthis)
    fthis
  }

  /** traverses the tree depth first performs an action as it comes to the node*/
  def preorderDepth(visit: Node[A] => Unit): Unit = {
    def recur(n: Node[A]): Unit = {
      visit(n)
      for (r <- n.children) {
        recur(r)
      }
    }
    recur(this)
  }

  /** traverses the tree depth first and performs an action after it reaches the leaves */
  def postorderDepth(visit: Node[A] => Unit): Unit = {
    def recur(n: Node[A]): Unit = {
      for (r <- n.children) {
        recur(r)
      }
      visit(n)
    }
    recur(this)
  }

  /** traverses the tree depth first performs an action as it comes to the node*/
/*  def breadthTraverse(visit: Node[A] => Unit): Unit = {
    var q = new Queue[Node[A]]()

    this.children foreach {v => visit(v) }
    recur(this)
  }*/

  /** returns the height of the tree*/
  def height: Int = {
    this.children.foldLeft(-1) { (h, c) => h max c.height } + 1
  }

  /** returns the number of nodes in the tree */
  def size: Int = {
    this.children.foldLeft(0) { (s, c) => s + c.size } + 1
  }
}

//class AndOrNode[A] extends Node(dataVar)





