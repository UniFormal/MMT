package info.kwarc.mmt.leo.datastructures

import datastructures.{ProofData, AndOrNode}
import org.scalatest._
/**
 * Created by mark on 6/25/15.
 */

class DatastructuresSpec extends FlatSpec with Matchers {

  val pda1= new ProofData(1,true)
  val pda2= new ProofData(2,false)
  val node0 = new AndOrNode(pda1)
  val node1 = new AndOrNode(pda1)
  val node2 = new AndOrNode(pda2)
  val node3 = new AndOrNode(pda1)
  val node4 = new AndOrNode(pda1)
  val node5 = new AndOrNode(pda1)
  node0.addChild(node1)
  node1.addChild(node2)
  node1.addChild(node3)
  node2.addChild(node4)
  node4.addChild(node5)
  //node1.setRoot(node0)

  "A Node" should "have a root and children and know its siblings" in {
    node1.meta should be (1)
    node1.siblings should be (Nil)
    node2.siblings should be (List(node3))
    node1.root should be (Some(node0))
    node0.root should be (None)
    node1.children should be (List(node2,node3))
    node2.children should be (List(node4))
    node0.depth should be (0)

  }

/*  it should "have a mapping function that preserves the structure of the tree" in {
    val pda1= new ProofData(1.5,true)
    val pda2= new ProofData(2.5,false)
    val node0 = new AndOrNode(pda1)
    val node1 = new AndOrNode(pda1)
    val node2 = new AndOrNode(pda2)
    val node3 = new AndOrNode(pda1)
    val node4 = new AndOrNode(pda1)
    val node5 = new AndOrNode(pda1)
    node0.addChild(node1)
    node1.addChild(node2)
    node1.addChild(node3)
    node2.addChild(node4)
    node4.addChild(node5)

    var fnode0 = node0.map(i => i.toDouble + .5)
    fnode0.data should be (0.5)
    fnode0.children.head.isEquivTo(tnode1) should be (true)
  }*/

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



