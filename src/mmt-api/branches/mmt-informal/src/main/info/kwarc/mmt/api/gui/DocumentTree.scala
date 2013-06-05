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

import java.awt.BorderLayout
import java.awt.event.{MouseAdapter,MouseEvent}

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
   lazy val children = se.components.mapPartial[MMTNode] {
      case r: XRef =>
         Some(new PathNode(r.target, controller))
      case o: Obj =>
         None //Some(new ObjNode(o))
      case e: StructuralElement =>
         Some(new StructuralElementNode(e, controller))
      case _ =>
         None
   }
}

class ObjNode(val obj: Obj) extends MMTNode {
   override def toString = obj.toString
   def children = Nil
}

class PathNode(path: Path, controller: Controller) extends MMTNode {
   override def toString = path.last
   lazy val seNode = new StructuralElementNode(controller.get(path), controller)
   def children = seNode.children
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


class TreePane(controller: Controller) extends JPanel {
   /** true: text; false: XML; pres: presentation according to style */
   private var mode: String = "text"
   private var style = DPath(utils.mmt.baseURI / "styles" / "lf" / "mathml.omdoc") ? "twelf"

   setLayout(new BorderLayout())

   val items = List(Item("plain", "text"), Item("text", "text/notations"), Item("XML", "xml"), Item("rendered", "pres"))
   val styleTextArea = new JTextField(style.toPath, 30)
   private val buttons = Swing.RadioButtonPanel(items : _*){id =>
      mode = id
      if (mode == "pres")
         try {style = Path.parseM(styleTextArea.getText, style)}
         catch {case _ : Throwable => styleTextArea.setText("error: " + styleTextArea.getText)}
   }
   
   buttons.add(styleTextArea)
   add(buttons, BorderLayout.NORTH)

   private val content = new JTextArea //new FXPanel
   private val scrollContent = new JScrollPane(content)
   scrollContent.setPreferredSize(new java.awt.Dimension(700,700))
   /** a RenderingHandler that writes directly into the JTextArea */
   private val rb = new presentation.TextHandler {
       def write(s: String) {content.append(s)}
   }

   private val tree = new JTree(new MMTTreeModel(controller))
   tree.setRootVisible(false)
   val ml = new MouseAdapter() {
      override def mousePressed(e: MouseEvent) {
         val path = tree.getPathForLocation(e.getX, e.getY)
         if (path == null) return
         val node = path.getLastPathComponent.asInstanceOf[MMTNode]
         if (e.getClickCount == 1) {
            val se = node match {
               case seNode: StructuralElementNode => seNode.se
               case pn: PathNode => pn.seNode.se
               case _ => null
            }
            if (se != null) {
               content.setText("")
               val presenter = controller.extman.getPresenter(mode) getOrElse {
                  new presentation.StyleBasedPresenter(controller,style)
               }
               presenter(se, rb)
            }
         }
      }
   }
   tree.addMouseListener(ml)
   private val scrollTree = new JScrollPane(tree)
   scrollTree.setPreferredSize(new java.awt.Dimension(300,700))
   add(scrollTree, BorderLayout.WEST)
   add(scrollContent, BorderLayout.CENTER)
}
