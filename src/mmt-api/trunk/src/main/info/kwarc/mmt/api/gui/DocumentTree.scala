package info.kwarc.mmt.api.gui

import info.kwarc.mmt.api._
import frontend._
import archives._
import documents._
import modules._
import utils.MyList._

import javax.swing._
import tree._
import event._

abstract class MMTNode(val path: Path) {
   override def toString = path.last
   def children: List[MMTNode]
}

class ControllerNode(controller: Controller) {
   def label = "MMT"
   def children = controller.backend.getArchives.map(a =>
      new PathNode(DPath(a.narrationBase), controller)
    )
}

class StructuralElementNode(val se: StructuralElement, controller: Controller) extends MMTNode(se.path) {
   lazy val children = se.components.mapPartial {
      case r: XRef => Some(new PathNode(r.target, controller))
      case e: StructuralElement => Some(new StructuralElementNode(e, controller))
      case _ => None
   }
}

class PathNode(path: Path, controller: Controller) extends MMTNode(path) {
   private var seNode: Option[StructuralElementNode] = None
   def children = {seNode match {
      case None =>
         val se = controller.get(path)
         seNode = Some(new StructuralElementNode(se, controller))
         children
      case Some(seNode) => seNode.children
   }}
}

class MMTTreeModel(controller: Controller) extends TreeModel {

   def addTreeModelListener(l: TreeModelListener) {}
   def removeTreeModelListener(l: TreeModelListener) {}

   def getChild(parent: Object, index: Int) = parent match {
      case n: MMTNode => n.children(index)
   }

   def getChildCount(parent: Object) = parent match { 
      case n: MMTNode => n.children.length
   }

   def getIndexOfChild(parent: Object, child: Object) = parent match {
      case n: MMTNode => n.children.indexOf(child)
   }
   
   def getRoot = new ControllerNode(controller)
   
   def isLeaf(node: Object) = node match {
      case seNode : StructuralElementNode => seNode.se.isInstanceOf[Module]
      case _ => false
   }
   
   def valueForPathChanged(path: TreePath, newValue: Object) {}
}
