package info.kwarc.mmt.leo.datastructures

import datastructures.Node
import org.scalatest._
/**
 * Created by mark on 6/25/15.
 */

class DatastructuresSpec extends FlatSpec with Matchers {

  "A Node" should "have a root and children" in {
    val node = new Node[Int](1)
    val node2 = new Node[Int](2)
    val node3 = new Node[Int](3)
    val node4 = new Node[Int](4)
    node.addChild(node2)
    node.addChild(node3)
    node.setRoot(node4)
    node.data should be (1)
    node.root should be (Some(node4))
    node.children should be (List(node2,node3))
  }

  /*it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[Int]
    a [NoSuchElementException] should be thrownBy {
      emptyStack.pop()
    }
  }*/
}



