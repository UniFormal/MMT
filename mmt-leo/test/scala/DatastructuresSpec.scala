package test.scala


import info.kwarc.mmt.leo.datastructures._
import info.kwarc.mmt.leo.toyprover.PartitionAgent
import org.scalatest._
import sun.management.resources.agent

/**
 * Created by mark on 6/25/15.
 */

class DatastructuresSpec extends FlatSpec with Matchers {


  def mkNode[A](data:A, cong:Boolean, sat: Option[Boolean]=None):ProofTree[A]={
    val pd= new ProofData(data,cong,sat)
    new ProofTree(pd)
  }

  val node0 = mkNode(0,false)
  val node1 = mkNode(1,false)
  val node2 = mkNode(2,false)
  val node3 = mkNode(3,false)
  val node4 = mkNode(4,true)
  node0.addChild(node1)
  node1.addChild(node2)
  node1.addChild(node3)
  node2.addChild(node4)
  //node1.setRoot(node0)

  "A Node" should "have a root and children and know its siblings" in {
    node1.data should be (1)
    node1.siblings should be (Nil)
    node2.siblings should be (List(node3))
    node1.parent should be (Some(node0))
    node0.parent should be (None)
    node1.children should be (List(node2,node3))
    node2.children should be (List(node4))
    node0.depth should be (0)
    val node5 = mkNode(5,true)
    node4.addChild(node5)
    node5.disconnect()
    node4.children should be (Nil)
    node0.leaves should be (List(node3,node4))
    node4.root should be (node0)
  }

  val tnode0 = mkNode(0.5,false)
  val tnode1 = mkNode(1.5,false)
  val tnode2 = mkNode(2.5,false)
  val tnode3 = mkNode(3.5,false)
  val tnode4 = mkNode(4.5,true)
  tnode0.addChild(tnode1)
  tnode1.addChild(tnode2)
  tnode1.addChild(tnode3)
  tnode2.addChild(tnode4)

  it should "have a mapping function that preserves the structure of the tree" in {

    val fnode0 = node0.map(i => i.toDouble + .5)
    fnode0.data should be (0.5)
    fnode0.children.head.isEquivTo(tnode1) should be (true)
  }

  it should "be able to properly trim the proof tree" in {
    node4.setSatisfiability(true)
    node4.percolateAndTrim()
    val t2node0 = mkNode(0,false,Some(true))
    val t2node1 = mkNode(1,false,Some(true))
    val t2node2 = mkNode(2,false,Some(true))
    val t2node4 = mkNode(4,true,Some(true))
    t2node0.addChild(t2node1)
    t2node1.addChild(t2node2)
    t2node2.addChild(t2node4)

    node4.isEquivTo(t2node4) should be (true)
    node0.isEquivTo(t2node0) should be (true)
  }

  "A BlackBoard" should "solve the partition problem" in {
    val goal = mkNode(17,true)
    val blackboard = new Blackboard(goal)
    val ra = new PartitionAgent
    val pa = new SingletonProofAgent[Int](ra)
    val ma = new AuctionAgent[Int]

    blackboard.registerAgent(ra)
    blackboard.registerAgent(pa)
    blackboard.registerAgent(ma)

    blackboard.runCycle()
    blackboard.runCycle()
    println(goal)

    OutputLog.display()

  }

/*  it should "have breadth and depth first search capabilities" in {
    var listB = List(-1)
    var listD = List(-1)
    node1.preorderDepth({n => listB=listB:::List(n.data) })
    listB should be (List(-1,1,2,3,4))
    node1.postorderDepth({n => listD=listD:::List(n.data) })
    listD should be (List(-1,1,2,4,3))
  }*/

  /*it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }*/
}



