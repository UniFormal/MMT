package datastructures

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
    child.root = None
    this.children = this.children.filter( p => p.root.get != child)
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
    val total_depth = this.height
    var output = ""
    this.preorder { subnode :Node[A] =>
      var depth = total_depth-subnode.height
      output = output+"\t"*depth + subnode.data.toString + "\n"
    }
    output
  }

  /** traverses the tree breadth first and performs an action */
  def preorder(visit: Node[A] => Unit): Unit = {
    def recur(n: Node[A]): Unit = {
      visit(n)
      for (r <- n.children) {
        recur(r)
      }
    }
    recur(this)
  }

  /** traverses the tree depth first and performs an action */
  def postorder(visit: Node[A] => Unit): Unit = {
    def recur(n: Node[A]): Unit = {
      for (r <- n.children) {
        recur(r)
      }
      visit(n)
    }
    recur(this)
  }

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





