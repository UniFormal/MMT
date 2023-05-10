package info.kwarc.mmt.api.gui

import info.kwarc.mmt.api._
import frontend._
import archives._
import documents._
import modules._
import objects._
import presentation._
import utils._

import javax.swing._
import tree._
import event._
import java.awt.BorderLayout
import java.awt.event.{MouseAdapter, MouseEvent}

import info.kwarc.mmt.api.frontend.actions.Navigate

// Tree model as used in the MMT GUI's content pane

abstract class MMTNode {
   def children: List[MMTNode]
}

/** root */
class ControllerNode(controller: Controller) extends MMTNode {
   override def toString = "MMT"
   def children = {
     val as = controller.backend.getArchives.sortBy(a => a.id)
     as.map {a =>
      new ArchiveNode(a, controller)
     }
   }
}

/** any MMT structural element */
class StructuralElementNode(val se: StructuralElement, controller: Controller) extends MMTNode {
   override def toString = se.path.name.toString
   lazy val children = se.getDeclarations.mapPartial[MMTNode] {
      case r: NRef =>
         Some(new StructuralElementNode(controller.get(r.target), controller))
      //case o: Obj =>
      //    Some(new ObjNode(o))
      case e: StructuralElement =>
         Some(new StructuralElementNode(e, controller))
      case _ =>
         None
   }
}

/** MMT objects and their subobjects */
class ObjNode(val obj: Obj) extends MMTNode {
   override def toString = obj.toString
   def children = Nil
}

/** archives */
class ArchiveNode(arch: Archive, controller: Controller) extends MMTNode {
   override def toString = arch.id
   lazy val seNode = new StructuralElementNode(controller.get(DPath(arch.narrationBase)), controller)
   def children = seNode.children
}

class MMTTreeModel(controller: Controller) extends TreeModel {

   private var listeners: List[TreeModelListener] = Nil

   def addTreeModelListener(l: TreeModelListener): Unit = {
      listeners ::= l
   }

   def removeTreeModelListener(l: TreeModelListener): Unit = {
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
      case seNode: StructuralElementNode => seNode.children.isEmpty
      case _ => false
   }

   def valueForPathChanged(path: TreePath, newValue: Object): Unit = {}
}

class MMTTree(controller: Controller) extends JTree(new MMTTreeModel(controller)) {
   setRootVisible(false)

   val ml = new MouseAdapter() {
      override def mousePressed(e: MouseEvent): Unit = {
         val jpath = getPathForLocation(e.getX, e.getY)
         if (jpath == null) return
         val node = jpath.getLastPathComponent.asInstanceOf[MMTNode]
         val se = node match {
            case seNode: StructuralElementNode => seNode.se
            case an: ArchiveNode => an.seNode.se
            case _ => null
         }
         clickOn(se,e)
      }
   }
   addMouseListener(ml)

   def clickOn(se: StructuralElement, e: MouseEvent): Unit = {}
}


class TreePane(controller: Controller) extends JPanel {
   /** true: text; false: XML; pres: presentation according to style */
   private var mode: String = "text"

   setLayout(new BorderLayout())

   val items = List(Item("plain", "text"), Item("text", "present-text-notations"), Item("XML", "xml"), Item("other", "other"))
   val presenterTextArea = new JTextField("", 30)
   private val toolbar = new JPanel
   private val buttons = Swing.RadioButtonPanel(items : _*){id =>
      mode = id
      if (mode == "other") {
         val other = presenterTextArea.getText
         if (controller.extman.get(classOf[Presenter], other).isEmpty) {
            presenterTextArea.setText("error: " + other)
            mode = "text"
         } else
            mode = other
      }
   }
   private def back: Unit = {
      if (current+1 < history.length) {
         current += 1
         setCurrentElement
      }
   }
   private def forward: Unit = {
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

   private val tree = new MMTTree(controller) {
      override def clickOn(se: StructuralElement, e: MouseEvent): Unit = {
         (e.getButton, e.getClickCount) match {
            case (MouseEvent.BUTTON1, 1) =>
               if (se != null) setNewElement(se)
            case (MouseEvent.BUTTON1, 2) =>
               if (se != null) {
                  val act = Navigate(se.path)
                  controller.handle(act)
               }
            case _ =>
         }
      }
   }

   private var history: List[StructuralElement] = Nil
   private var current = 0
   private def setNewElement(p: Path): Unit = {
      val se = controller.get(p)
      setNewElement(se)
   }
   private def setNewElement(se: StructuralElement): Unit = {
      history = history.take(current) ::: se :: history.drop(current)
      setCurrentElement
   }
   private def setCurrentElement: Unit = {
      val se = history(current)
      val presenter = controller.extman.get(classOf[Presenter], mode).get // defined due to check above
      val rb = new presentation.StringBuilder
      presenter(se)(rb)
      content.setText(rb.get)
      val qsPanelWidth = ontologyPane.getSize().width
      ontologyPane.removeAll
      val p = se.path
      ontologyPane.add(Swing.centeredLabel(p.toString + " ..."))
      controller.relman.allBinary.foreach {b =>
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
   private val scrollTree = new JScrollPane(tree)
   //scrollTree.setPreferredSize(new java.awt.Dimension(300,300))
   //scrollContent.setPreferredSize(new java.awt.Dimension(700,300))

   add(toolbar, BorderLayout.NORTH)
   add(scrollTree, BorderLayout.WEST)
   add(scrollContent, BorderLayout.CENTER)
   add(ontologyPane, BorderLayout.SOUTH)
}
