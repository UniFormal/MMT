package info.kwarc.mmt.leo.datastructures

import datastructures.Node
import org.scalatest._
/**
 * Created by mark on 6/25/15.
 */

class DatastructuresSpec extends FlatSpec with Matchers {

  val node0 = new Node[Int](0)
  val node1 = new Node[Int](1)
  val node2 = new Node[Int](2)
  val node3 = new Node[Int](3)
  val node4 = new Node[Int](4)
  val node5 = new Node[Int](5)
  node0.addChild(node1)
  node1.addChild(node2)
  node1.addChild(node3)
  node2.addChild(node4)
  node4.addChild(node5)
  //node1.setRoot(node0)

  "A Node" should "have a root and children" in {
    node1.data should be (1)
    node1.root should be (Some(node0))
    node0.root should be (None)
    node1.children should be (List(node2,node3))
    node2.children should be (List(node4))
    node0.depth should be (0)

  }

  it should "have a mapping function that preserves the structure of the tree" in {
    val tnode0 = new Node(0.5)
    val tnode1 = new Node(1.5)
    val tnode2 = new Node(2.5)
    val tnode3 = new Node(3.5)
    val tnode4 = new Node(4.5)
    val tnode5 = new Node(5.5)
    tnode0.addChild(tnode1)
    tnode1.addChild(tnode2)
    tnode1.addChild(tnode3)
    tnode2.addChild(tnode4)
    tnode4.addChild(tnode5)

    var fnode0 = node0.map(i => i.toDouble + .5)
    fnode0.data should be (0.5)
    fnode0.children.head.isEquivTo(tnode1) should be (true)
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



