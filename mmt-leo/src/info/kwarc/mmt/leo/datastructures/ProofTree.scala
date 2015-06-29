package info.kwarc.mmt.leo.datastructures

/**
 * This class represents the information stored in an abstract proof tree
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

  /** read and write locks*/
  var isReadable = true
  var isWritable = true

  def isUnsatisfiable = !this.isSatisfiable.getOrElse(true)
  def isAnd = conjunctive
  def isOr = !conjunctive
  override def toString:String ={
    "meta: "+meta.toString+" isAnd: "+isAnd.toString+" isSatisfiable: "+isSatisfiable.toString
  }
}


/**
 * This class represents an and/or tree.
 *
 * @param dataVar Proof data to be stored at the nodes, meta information consisting of and/or status and solved status
 * @tparam A type of metadata stored in nodes
 */
class ProofTree[A](var dataVar: ProofData[A] ) {
  var data = dataVar
  var meta = dataVar.meta
  var root: Option[ProofTree[A]] = None
  var children: List[ProofTree[A]] = Nil

  def isAnd = dataVar.isAnd
  def isOr = dataVar.isOr
  def isSatisfiable = dataVar.isSatisfiable
  def isUnsatisfiable = dataVar.isUnsatisfiable
  def isSolved = isSatisfiable.getOrElse(false)

  /** helper function to make tree nodes*/
  def mkNode[B](m:B,c:Boolean,s:Option[Boolean]=None):ProofTree[B]={
    new ProofTree[B](new ProofData(m,c,s))
  }

  /** returns the siblings of the current node aka the nodes that share its parent*/
  def siblings: List[ProofTree[A]] = { root match{
    case None => Nil
    case Some(p) => p.children diff List(this)
    }
  }

  /** returns the path of nodes to the root of the tree*/
  def path: List[ProofTree[A]] = this :: root.map(_.path).getOrElse(Nil)

  /** returns the number of nodes above the current node*/
  //TODO improve speed
  def depth: Int = {path.length-1}

  /** structural and component-wise equivalence
    * returns true if the two trees have the same data, conjunctive nodes, and solved status*/
  def isEquivTo(n: ProofTree[A]):Boolean ={
    (this.children,n.children) match {
      case (Nil,Nil) => this.meta == n.meta && this.isAnd==n.isAnd && this.isSatisfiable==n.isSatisfiable
      case (l1,l2) =>
        if (l1.length != l2.length) {return false}
        for(x <- l1.indices){
          if (!l1(x).isEquivTo(l2(x))) {return false}
        }
        true
      case _ => false
    }
  }

  /** checks if a node is equal to or below the current node*/
  def isBelow(that: ProofTree[A]): Boolean = this == that || root.exists(_ isBelow that)
  /** checks if a node is equal to or above the current node*/
  def isAbove(that: ProofTree[A]): Boolean = this == that || children.exists(_ isAbove that)
  /** checks if a node is the child of another*/
  def isChildOf(that: ProofTree[A]): Boolean = root.get==that
  /** checks if a node is a parent of another*/
  def isParentOf(that: ProofTree[A]): Boolean = children.contains(that)

  /** changes the data field in the node*/
  def setSatisfiability( bool: Boolean) {
    data.isSatisfiable = Some(bool)
  }

  /** Adds a child from the node*/
  def addChild(child : ProofTree[A]):Unit={
    if (child.isAbove(this)) {
      throw new IllegalArgumentException("Child node above the current")
    }
    child.root match {
      case Some(c) => child.root.get.disconnectChild(child)
      case None =>
    }
    child.setRoot(this)
    children = children ::: List(child)
  }

  /** disconnects specific child from the node*/
  def disconnectChild(child : ProofTree[A]):Unit={
    children = children.filter( c => c != child)
    child.root = None
  }

  /** disconnects all children from the node*/
  def disconnectChildren(): Unit = {
    children foreach {_.root = None}
    children = Nil
  }

  /** deletes current node**/
  def disconnect() = {
    root match {
      case Some(p)=>p.disconnectChild(this)
      case _ =>
    }
    root = None
  }

  /** sets the Root of the current node*/
  def setRoot(that: ProofTree[A]): Unit ={
    if (isAbove(that)) {
      throw new IllegalArgumentException("Current node already above the attempted root")
    }
    this.root match{
      case Some(c) => root.get.disconnectChild(this)
      case None =>
    }
    this.root = Some(that)
    List(this):::that.children
  }

  /** returns a nice looking printable string **/
  //TODO improve speed
  override def toString: String = {
    var output = ""
    preDepthTraverse { subnode :ProofTree[A] =>
      output = output++ "\n"+"\t"*subnode.depth + subnode.data.toString
    }
    output+ "\n"
  }

  /** maps a tree of one data-type to a tree of another*/
  def map[B](f: A => B): ProofTree[B] = {
    val fthis = mkNode(f(meta),isAnd)
    def recur(n : ProofTree[A],fn: ProofTree[B]): Unit = {
      for (r <- n.children) {
        val addition = mkNode(f(r.meta),r.isAnd)
        fn.addChild(addition)
        recur(r,addition)
      }
    }
    recur(this,fthis)
    fthis
  }

  /** traverses the tree depth first performs an action as it comes to the node*/
  def preDepthTraverse(visit: ProofTree[A] => Unit): Unit = {
    def recur(n: ProofTree[A] ): Unit = {
      visit(n)
      for (r <- n.children) {
        recur(r)
      }
    }
    recur(this)
  }

  /** traverses the tree depth first and performs an action after it reaches the leaves */
  def postDepthTraverse(visit: ProofTree[A] => Unit): Unit = {
    def recur(n: ProofTree[A] ): Unit = {
      for (r <- n.children) {
        recur(r)
      }
      visit(n)
    }
    recur(this)
  }
  
  def preDepthFlatten:List[ProofTree[A]] ={
    val out:List[ProofTree[A]] = Nil
    preDepthTraverse({n=>List(n):::out; Unit})
    out
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
    children.foldLeft(-1) { (h, c) => h max c.height } + 1
  }

  /** returns the number of nodes in the tree */
  def size: Int = {
    children.foldLeft(0) { (s, c) => s + c.size } + 1
  }

  /** checks to see if node is solved and can be closed and propagates
    * down the tree closing all unnecessary nodes*/
  def pruneBelow():Unit = {
    if (isAnd ) {
      if (children.exists(_.isUnsatisfiable)){disconnectChildren()}
      if (children.forall(_.isSolved)) {setSatisfiability(true)}
    }else{
      children.filter(_.isUnsatisfiable).foreach(_.disconnect())
      if (children.exists(_.isSolved)) {children.filter(!_.isSolved).foreach(_.disconnect())}
    }
    if (isSolved) children.filter(!_.isSolved).foreach(_.disconnect())
    children.foreach(_.pruneBelow())
  }

  /** propagates the effect of the solved node up the tree, trimming any unnecessary nodes*/
  def percolateAndTrim():Unit = {
    if (isSolved) {
      this.root match {
        case None => pruneBelow()
        case Some(p) if p.isOr =>
          p.setSatisfiability(true)
          siblings.foreach(_.disconnect())
          p.percolateAndTrim()
        case Some(p) if p.siblings.forall(_.isSolved) => p.setSatisfiability(true); p.percolateAndTrim()
        case Some(p) => p.pruneBelow()
      }
    }
    if (isUnsatisfiable) {
      this.root match {
        case None => pruneBelow()
        case Some(p) if p.isAnd => p.setSatisfiability(false); p.percolateAndTrim()
        case Some(p) => p.pruneBelow()
      }
    }
  }



}









