package info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.PartitionImpl

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.leo.AgentSystem.AndOrSystem.DataTreeSystem.DataTree
import info.kwarc.mmt.leo.AgentSystem.Presenter

/**
 * Created by Mark on 7/21/2015.
 */
class PartitionPresenter(implicit controller: Controller) extends Presenter {
  type ObjectType= DataTree[Int]

  def present(pt: DataTree[Int]): String = {
    /** @return a list of numbers solving the problem*/
    def getNumbers(node: DataTree[Int]): List[Int] ={
      if (node.children.isEmpty) {
        List(node.data)
      }else {
        val next = node.children.filter(_.isSolved).head
        (node.data - next.data)::getNumbers(next)
      }
    }

    pt.sat match {
      case Some(true) => "Solution: "+getNumbers(pt).sorted
      case Some(false) => "Contradiction Derived, no partition is possible. Outputting tree:" + pt
      case None => "Proof not found"
    }
  }
}
