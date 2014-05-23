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

   setLayout(new BorderLayout())

   val items = List(Item("plain", "text"), Item("text", "text/notations"), Item("XML", "xml"), Item("other", "other"))
   val presenterTextArea = new JTextField("", 30)
   private val toolbar = new JPanel
   private val buttons = Swing.RadioButtonPanel(items : _*){id =>
      mode = id
      if (mode == "other") {
         val other = presenterTextArea.getText
         if (controller.extman.getPresenter(other).isEmpty) {
            presenterTextArea.setText("error: " + other)
            mode = "text"
         } else
            mode = other
      }
   }
   private def back {
      if (current+1 < history.length) {
         current += 1
         setCurrentElement
      }
   }
   private def forward {
      if (current-1 >= 0) {
         current -= 1
         setCurrentElement
      }
   }
   toolbar.add(buttons)
   toolbar.add(Swing.Button("back")(back))
   toolbar.add(Swing.Button("forward")(forward))
   
   buttons.add(presenterTextArea)

   private val content = new JTextArea // FXPanel 
   private val scrollContent = new JScrollPane(content)

   val ontologyPane = new JPanel
   ontologyPane.setLayout(new BoxLayout(ontologyPane, BoxLayout.PAGE_AXIS))
   
   private val tree = new JTree(new MMTTreeModel(controller))
   tree.setRootVisible(false)
   
   private var history: List[StructuralElement] = Nil
   private var current = 0
   private def setNewElement(p: Path) {
      val se = controller.get(p)
      setNewElement(se)
   }
   private def setNewElement(se: StructuralElement) {
      history = history.take(current) ::: se :: history.drop(current)
      setCurrentElement
   }
   private def setCurrentElement {
      val se = history(current)
      val presenter = controller.extman.getPresenter(mode).get // defined due to check above
      val rb = new presentation.StringBuilder
      presenter(se)(rb)
      content.setText(rb.get) 
      val qsPanelWidth = ontologyPane.getSize().width
      ontologyPane.removeAll
      val p = se.path
      ontologyPane.add(Swing.centeredLabel(p.toString + " ..."))
      ontology.Binary.all.foreach {b =>
         val qs = controller.depstore.queryList(p, -b)
         if (! qs.isEmpty) {
            ontologyPane.add(Swing.centeredLabel("... " + b.backwardsDesc))
            val qsPanel = new JPanel(new WrapLayout(qsPanelWidth))
            qs.sortBy(_.toString).foreach {q =>
               val b = Swing.Button(q.toString)(setNewElement(q))
               qsPanel.add(b)
            }
            ontologyPane.add(qsPanel)
         }
      }
      revalidate
   }
   val ml = new MouseAdapter() {
      override def mousePressed(e: MouseEvent) {
         val jpath = tree.getPathForLocation(e.getX, e.getY)
         if (jpath == null) return
         val node = jpath.getLastPathComponent.asInstanceOf[MMTNode]
         val se = node match {
            case seNode: StructuralElementNode => seNode.se
            case pn: PathNode => pn.seNode.se
            case _ => null
         }
         (e.getButton, e.getClickCount) match {
            case (MouseEvent.BUTTON1, 1) =>
               if (se != null) setNewElement(se)
            case (MouseEvent.BUTTON1, 2) =>
               if (se != null) {
                  val act = Navigate(se.path)
                  controller.handle(act)
               }
         }
      }
   }
   tree.addMouseListener(ml)
   private val scrollTree = new JScrollPane(tree)
   //scrollTree.setPreferredSize(new java.awt.Dimension(300,300))
   //scrollContent.setPreferredSize(new java.awt.Dimension(700,300))

   add(toolbar, BorderLayout.NORTH)
   add(scrollTree, BorderLayout.WEST)
   add(scrollContent, BorderLayout.CENTER)
   add(ontologyPane, BorderLayout.SOUTH)
}
