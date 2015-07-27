package test.scala


import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.DataTree
import info.kwarc.mmt.leo.AgentSystem.{Display, OutputLog}
import info.kwarc.mmt.leo.provers._
import org.scalatest._

/**
 * Created by mark on 6/25/15.
 *
 * this class represents the series of unit tests the prover
 * architecture and tree architecture must pass
 *
 */

class DatastructuresSpec extends FlatSpec with Matchers {
  implicit val controller = new Controller
  controller.handleLine("log console")
  controller.handleLine("log+ Blackboard")
  controller.handleLine("log+ AuctionAgent")
  controller.handleLine("log+ ExecutionAgent")
  controller.handleLine("log+ PartitionAgent")
  controller.handleLine("log+ PartitionTask")
  controller.handleLine("log+ AndOrSection")



  val node0 = new DataTree(0,conjVar = false,None)
  val node1 = new DataTree(1,conjVar = false,None)
  val node2 = new DataTree(2,conjVar = false,None)
  val node3 = new DataTree(3,conjVar= false,None)
  val node4 = new DataTree(4,conjVar = true,None)
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
    node1.children should be (List(node3,node2))
    node2.children should be (List(node4))
    node0.depth should be (0)
    val node5 = new DataTree(5,conjVar = true, None)
    node4.addChild(node5)
    node5.disconnect()
    node4.children should be (Nil)
    node0.leaves should be (List(node4,node3))
    node4.root should be (node0)
  }

  val tnode0 = new DataTree(0.5,conjVar = false,None)
  val tnode1 = new DataTree(1.5,conjVar = false,None)
  val tnode2 = new DataTree(2.5,conjVar = false,None)
  val tnode3 = new DataTree(3.5,conjVar = false,None)
  val tnode4 = new DataTree(4.5,conjVar = true,None)
  tnode0.addChild(tnode1)
  tnode1.addChild(tnode2)
  tnode1.addChild(tnode3)
  tnode2.addChild(tnode4)

  it should "have a mapping function that preserves the structure of the tree" in {
    /*val fnode0 = node0.map[Double](i => i.toDouble + .5)
    fnode0.data should be (0.5)
    fnode0.children.head.isEquivTo(tnode1) should be (right = true)


    val fnode0 = node0.functor({i:Int => i.toDouble +.5})
    fnode0.data should be (0.5)
    fnode0.children.head.isEquivTo(tnode1) should be (right = true)*/
  }

  it should "be able to properly trim the proof tree" in {
    node4.setSat(false)
    //var node0copy=node0.copy

    node4.setSat(true)
    node4.percolate()
    val t2node0 = new DataTree(0,conjVar = false,Some(true))
    val t2node1 = new DataTree(1,conjVar = false,Some(true))
    val t2node2 = new DataTree(2,conjVar = false,Some(true))
    val t2node4 = new DataTree(4,conjVar = true,Some(true))
    t2node0.addChild(t2node1)
    t2node1.addChild(t2node2)
    t2node2.addChild(t2node4)

    node4.isEquivTo(t2node4) should be (right = true)
    node0.isEquivTo(t2node0) should be (right = true)
  }

  "A BlackBoard" should "solve the partition problem" in {

    val prover1 = new PartitionProver( 23,List(2,3,5,7) )
    prover1.run() should be ("Solution: List(2, 7, 7, 7)")

    val prover2 = new PartitionProver( 23,List(15,17) )

    prover2.run() should be ("Contradiction Derived, no partition is possible. Outputting tree:" +
      "\ndata: 23 isAnd: false isSatisfiable: Some(false)" +
      "\n\tdata: 6 isAnd: false isSatisfiable: Some(false)" +
      "\n\tdata: 8 isAnd: false isSatisfiable: Some(false)")


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



