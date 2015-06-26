package datastructures

import scala.collection.immutable.Queue


/**
 * This class represents the key piece of meta information stored in an abstract
 * proof tree
 *
 * @param conjunctiveVar if true this signifies an and node and all subnodes
 *                        must be solved for the node to be satisfied. If false
 *                        it is an or node and only one subnode needs to be satisfied
 * @param isSatisfiableVar is true if it is solved, is None if it is unknown, is false if it is unsatasfiable
 */
class ProofData[A](metaVar: A, conjunctiveVar: Boolean, isSatisfiableVar: Option[Boolean] = None ) {
  var meta=metaVar
  var conjunctive = conjunctiveVar //TODO figure out why cannot call variable in nested situation
  var isSatisfiable = isSatisfiableVar
  def isUnsatisfiable = !(this.isSatisfiable.getOrElse(true))
  def isAnd = conjunctive
  def isOr = !conjunctive
  override def toString:String ={
    meta.toString
  }
}

/**
 * This class represents an and/or tree.
 *
 * @param dataVar Proof data to be stored at the nodes, meta information consisting of and/or status and solved status
 * @tparam A type of metadata stored in nodes
 */
class AndOrNode[A](var dataVar: ProofData[A] ) {
  var data = dataVar
  var meta = dataVar.meta
  var root: Option[AndOrNode[A]] = None
  var children: List[AndOrNode[A]] = Nil

  def isAnd = dataVar.isAnd
  def isOr = dataVar.isOr
  def isSatisfiable = dataVar.isSatisfiable
  def isUnsatisfiable = dataVar.isUnsatisfiable
  def isSolved = this.isSatisfiable.getOrElse(false)

  /** returns the siblings of the current node aka the nodes that share its parent*/
  def siblings: List[AndOrNode[A]] = { this.root match{
    case None => Nil
    case Some(p) => p.children diff List(this)
    }
  }

  /** returns the path of nodes to the root of the tree*/
  def path: List[AndOrNode[A]] = this :: root.map(_.path).getOrElse(Nil)

  /** returns the number of nodes above the current node*/
  //TODO improve speed
  def depth: Int = {this.path.length-1}

  /*/** component-wise equivalence*/
  def isEquivTo(n: AndOrNode[A]):Boolean ={
    (this.children,n.children) match {
      case (Nil,Nil) => this.mata == n.data
      case (l1,l2) =>
        if (l1.length != l2.length) {return false}
        for(x <- l1.indices){
          if (l1(x).isEquivTo(l2(x))== false) {return false}
        }
        true
      case _ => false
    }
  }*/

  /** checks if a node is equal to or below the current node*/
  def isBelow(that: AndOrNode[A]): Boolean = this == that || root.exists(_ isBelow that)
  /** checks if a node is equal to or above the current node*/
  def isAbove(that: AndOrNode[A]): Boolean = this == that || children.exists(_ isAbove that)
  /** checks if a node is the child of another*/
  def isChildOf(that: AndOrNode[A]): Boolean = root.get==that
  /** checks if a node is a parent of another*/
  def isParentOf(that: AndOrNode[A]): Boolean = this.children.contains(that)

  /** changes the data field in the node*/
  def setSatisfiability( bool: Boolean) {
    this.data.isSatisfiable = Some(bool)
  }

  //var Int=:=Int
  /** Adds a child from the node*/
  def addChild(child : AndOrNode[A]):Unit={
    child.root match {
      case Some(c) => child.root.get.disconnectChild(child)
      case None =>
    }
    child.root = Some(this)
    this.children = this.children ::: List(child)
  }

  /** disconnects specific child from the node*/
  def disconnectChild(child : AndOrNode[A]):Unit={
    this.children = this.children.filter( p => p.root.get != child)
    child.root = None
  }

  /** disconnects all children from the node*/
  def disconnectChildren(): Unit = {
    this.children foreach {_.root = None}
    this.children = Nil
  }

  /** deletes current node**/
  def delete() = {
    this.postorderDepth(n => n.finalize())
  }

  /** removes a specific child from the node and deletes object*/
  def deleteChild(child : AndOrNode[A]):Unit={
    this.children = this.children.filter( p => p.root.get != child)
    child.delete()
  }

  /** removes all children from the node and deletes them as objects*/
  def deleteChildren(): Unit = {
    this.children foreach {child=>child.delete()}
    this.children = Nil
  }

  /** sets the Root of the current node*/
  def setRoot(that: AndOrNode[A]): Unit ={
    if (isAbove(that)) {
      throw new IllegalArgumentException("Current node already above the attempted root")
    }
    this.root match{
      case Some(c) => this.root.get.disconnectChild(this)
      case None =>
    }
    this.root = Some(that)
    List(this):::that.children
  }

  /** returns a nice looking printable string **/
  //TODO improve speed
  override def toString: String = {
    var output = ""
    this.preorderDepth { subnode :AndOrNode[A] =>
      output = output++ "\n"+"\t"*subnode.depth + subnode.meta.toString
    }
    output+ "\n"
  }

  def mkAndOrNode[B](m:B,s:Boolean):AndOrNode[B]={
    new AndOrNode[B](new ProofData(m,s))
  }

  /** maps a tree of one data-type to a tree of another*/
  def map[B](f: A => B): AndOrNode[B] = {
    val fthis = mkAndOrNode(f(this.meta),this.isAnd)
    def recur(n : AndOrNode[A],fn: AndOrNode[B]): Unit = {
      for (r <- n.children) {
        val addition = mkAndOrNode(f(r.meta),r.isAnd)
        fn.addChild(addition)
        recur(r,addition)
      }
    }
    recur(this,fthis)
    fthis
  }

  /** traverses the tree depth first performs an action as it comes to the node*/
  def preorderDepth(visit: AndOrNode[A] => Unit): Unit = {
    def recur(n: AndOrNode[A] ): Unit = {
      visit(n)
      for (r <- n.children) {
        recur(r)
      }
    }
    recur(this)
  }

  /** traverses the tree depth first and performs an action after it reaches the leaves */
  def postorderDepth(visit: AndOrNode[A] => Unit): Unit = {
    def recur(n: AndOrNode[A] ): Unit = {
      for (r <- n.children) {
        recur(r)
      }
      visit(n)
    }
    recur(this)
  }

/*  /** traverses the tree depth first performs an action as it comes to the node,
    * set the depth to -1 to search all the way through */
  def breadthTraverse(visit: Node[A] => Unit, maxDepth:Int = -1): Unit = {
    var q = new Queue[Node[A]]()
    var labels = this.map(n=>(n,false))


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

  //override def addChild(n:AndOrNode[A]):Unit = this.addChild(n)

  def pruneBelow():Unit = {
    if (this.isAnd ) {
      if (children.exists(_.isUnsatisfiable)){this.deleteChildren()}
      if (children.forall(_.isSolved)) {this.setSatisfiability(true)}
    }else{
      children.filter(_.isUnsatisfiable).foreach(_.delete())
      if (children.exists(_.isSolved)) {children.filter(!_.isSolved).foreach(_.delete())}
    }
    children.foreach(_.pruneBelow())
  }

  def percolateAndTrim():Unit = {
    if (this.isSolved) {
      this.root match {
        case None => this.pruneBelow()
        case Some(p) if p.isOr => p.setSatisfiability(true); p.percolateAndTrim()
        case Some(p) => p.pruneBelow()
      }
    }
    if (this.isUnsatisfiable) {
      this.root match {
        case None => this.pruneBelow()
        case Some(p) if p.isAnd => p.setSatisfiability(false); p.percolateAndTrim()
        case Some(p) => p.pruneBelow()
      }
    }
  }



}









