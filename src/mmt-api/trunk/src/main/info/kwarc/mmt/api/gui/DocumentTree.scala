package info.kwarc.mmt.api.gui

import info.kwarc.mmt.api._
import frontend._
import archives._
import documents._
import modules._
import objects._ 
import utils.MyList._

import javax.swing._
import tree._
import event._

abstract class MMTNode {
   def children: List[MMTNode]
}

class ControllerNode(controller: Controller) extends MMTNode {
   override def toString = "MMT"
   def children = controller.backend.getArchives.map(a =>
      new PathNode(DPath(a.narrationBase), controller)
    )
}

class StructuralElementNode(val se: StructuralElement, controller: Controller) extends MMTNode {
   override def toString = se.path.last
   lazy val children = se.components mapPartial {
      case r: XRef =>
         Some(new PathNode(r.target, controller))
      case o: Obj =>
         Some(new ObjNode(o))
      case e: StructuralElement =>
         Some(new StructuralElementNode(e, controller))
      case _ =>
         None
   }
}

class ObjNode(obj: Obj) extends MMTNode {
   override def toString = obj.toString
   def children = Nil
}

class PathNode(path: Path, controller: Controller) extends MMTNode {
   override def toString = path.last
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

   private var listeners: List[TreeModelListener] = Nil
   def addTreeModelListener(l: TreeModelListener) {listeners ::= l}
   def removeTreeModelListener(l: TreeModelListener) {
     listeners = listeners.filter(_ != l)
   }

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
      case seNode : StructuralElementNode => seNode.children.isEmpty
      case _ => false
   }
   
   def valueForPathChanged(path: TreePath, newValue: Object) {}
}
