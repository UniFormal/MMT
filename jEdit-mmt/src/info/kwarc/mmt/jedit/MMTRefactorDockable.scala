package info.kwarc.mmt.jedit

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Font, Dimension, BorderLayout, GridLayout}
import javax.swing._
import info.kwarc.mmt.api
import info.kwarc.mmt.api.objects.OMID
import info.kwarc.mmt.api.presentation.{StringBuilder, MMTSyntaxPresenter}
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{DeclaredView, DeclaredTheory}
import info.kwarc.mmt.api.refactoring.{Intersecter, Moduleadder, Viewfinder}
import org.gjt.sp.jedit.{jEdit, View}

import scala.tools.nsc.doc.base.comment.Bold

class MMTRefactorDockable(view: View, position: String) extends JPanel with ActionListener {
  val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
  val controller = mmt.controller
  val topPanel = new JPanel
  val resultArea = new JPanel
  val findViewsHereButton = new JButton("Find Views in current Document")
  val findViewsGlobalButton = new JButton("Find Views globally")
  val addViewsButton = new JButton("Add declared Views")
  val textfield1 = new JTextField("Parameter Cutoff:")
  val cutoffField = new JTextField("0")
  val textfield2 = new JTextField("View Value cutoff:")
  val valueField = new JTextField("0")

  def init = {
    setLayout(new BorderLayout)
    topPanel.setLayout(new BoxLayout(topPanel,BoxLayout.X_AXIS))
    resultArea.setLayout(new BoxLayout(resultArea,BoxLayout.Y_AXIS))

    textfield1.setEditable(false)
    cutoffField.setEditable(true)
    textfield2.setEditable(false)
    valueField.setEditable(true)

    findViewsHereButton.addActionListener(this)
    findViewsGlobalButton.addActionListener(this)
    addViewsButton.addActionListener(this)

    topPanel.add(findViewsHereButton)
    topPanel.add(findViewsGlobalButton)
    topPanel.add(addViewsButton)
    topPanel.add(Box.createRigidArea(new Dimension(5,0)))
    topPanel.add(textfield1)
    topPanel.add(cutoffField)
    topPanel.add(Box.createRigidArea(new Dimension(5,0)))
    topPanel.add(textfield2)
    topPanel.add(valueField)
    add(topPanel,BorderLayout.PAGE_START)
    add(new JScrollPane(resultArea),BorderLayout.CENTER)
  }

  init

  def actionPerformed(ae: ActionEvent) = {
    try {
      if (ae.getSource == findViewsHereButton) {
        resultArea.removeAll()
        val text = new JTextArea("Looking for views...")
        text.setEditable(false)
        resultArea.add(text)
        resultArea.revalidate()
        val theories = controller.memory.content.getModules collect { case t: DeclaredTheory => t }
        val pairs = (for {a <- theories; b <- theories if a != b} yield Set(a, b)).toSet
        val views = (for {p <- pairs} yield {
          text.append("\n" + p.head.name + " -> " + p.tail.head.name + "...")
          this.repaint()
          val allviews = {
            val list = Viewfinder.findByAxioms(p.head,p.tail.head,controller,cutoffField.getText.toInt,true,true)
            (for {o <- list} yield (o,evaluateViewset(p.head,p.tail.head,o,controller))
            ).filter(p => p._2>=valueField.getText.toDouble && p._2>0)
          }
          text.append(allviews.toList.length+" Views found!")
          this.repaint()
          (p.head,p.tail.head,allviews)
        }).filter(p => p._3.nonEmpty)
        text.append("\nDone.")
        this.repaint()
        if (views.nonEmpty) {
          resultArea.remove(text)

          for (o <- views) {
            val titlefield = new JTextField(o._1.name + " -> " + o._2.name)
            titlefield.setEditable(false)
            titlefield.setSize(titlefield.getMinimumSize)
            titlefield.setFont(titlefield.getFont.deriveFont(Font.BOLD))

            resultArea.add(titlefield)

            for (p <- o._3) {
              resultArea.add(new ResultViewArea(o._1, o._2, p._1, p._2, controller, this))
            }
          }
        } else text.append("\nNo Views found!")

        resultArea.revalidate()
        this.repaint()

      }

      if (ae.getSource == findViewsGlobalButton) {
        resultArea.removeAll()
        val text = new JTextArea("Click2!")
        text.setEditable(false)
        resultArea.add(text)
        resultArea.revalidate()
      }

      if (ae.getSource == addViewsButton) {
        resultArea.removeAll()
        val docviews = controller.memory.content.getModules collect { case v: DeclaredView => v }
        for (o <- docviews) {
          val dom = controller.get(o.from.toMPath) match {
            case t:DeclaredTheory => t
            case _ => throw new Exception("DeclaredTheory expected!")
          }
          val cod = controller.get(o.to.toMPath) match {
            case t:DeclaredTheory => t
            case _ => throw new Exception("DeclaredTheory expected!")
          }

          val vset = viewtoviewset(o,controller)
          resultArea.add(new ExistingViewArea(o,dom,cod,vset,evaluateViewset(dom,cod,vset,controller),this,controller))
        }
        resultArea.revalidate()
        this.repaint()
      }

    } catch {
      case e:Exception => e.printStackTrace(); throw e
    }

  }

  def evaluateViewset(from:DeclaredTheory,to:DeclaredTheory,viewset:Set[(GlobalName,GlobalName)],ctrl:Controller): Double = {
    val domc = from.getConstants collect { case c: FinalConstant => c}

    val codc = to.getConstants collect { case c: FinalConstant => c}

    viewset.filter(p => domc.exists(q => q.path==p._1)).toList.length.toDouble / (if (domc.length<codc.length) domc.length else codc.length).toDouble

  }

  def viewtoviewset(v:DeclaredView,ctrl:Controller) : Set[(GlobalName,GlobalName)] = {

    val pairs = v.domain.map(name => {
     val domname = Path.parse(name.head.toString.tail.dropRight(1)).toTriple match {
       case (Some(d:DPath),Some(m:LocalName),None) =>
         GlobalName(OMID(MPath(d,m)) ,name.tail)
       case _ => throw new Exception("Module Path expected!")
     }
     val codname = v.get(name) match {
       case c:FinalConstant => c.df.get match {
         case t:OMID => ctrl.get(t.path) match {
           case o:FinalConstant => o.path
           case _ => false
         }
         case _ => false
       }
       case _ => throw new Exception("FinalConstant expected!")
     }
     (domname,codname)
    }).toSet
    pairs collect {case (a:GlobalName,b:GlobalName) => (a,b)}
  }

  def dumptoDocument(s:String) = {
    view.getTextArea.setText(view.getTextArea.getText+"\n\n"+s)
  }

}

class ResultViewArea(from:DeclaredTheory,to:DeclaredTheory,viewset:Set[(GlobalName,GlobalName)],value:Double,controller:Controller,
                      target:MMTRefactorDockable)
  extends JPanel with ActionListener {

  setLayout(new BoxLayout(this,BoxLayout.X_AXIS))
  private val vContent = new JComboBox(viewset.map(o => o._1.^!.last+"?"+o._1.name.toString()
    +" -> "+o._2.^!.last+"?"+o._2.name.toString()).toArray)
  private val valuefield = new JTextField("Value: "+value)
  valuefield.setEditable(false)
  private val vName = new JTextField("View Name")
  private val addButton = new JButton("Add View")
  addButton.addActionListener(this)
  private val intersectButton = new JButton("Refactor")
  intersectButton.addActionListener(this)

  vContent.setSize(vContent.getMinimumSize)
  valuefield.setSize(valuefield.getMinimumSize)
  add(vContent)
  add(Box.createRigidArea(new Dimension(5,0)))
  add(valuefield)
  add(Box.createRigidArea(new Dimension(5,0)))
  add(vName)
  add(Box.createRigidArea(new Dimension(5,0)))
  add(addButton)
  add(intersectButton)

  this.setSize(this.getMinimumSize)

  def actionPerformed(ae: ActionEvent) = {
    if (ae.getSource==addButton) {
      val v = new DeclaredView(from.parent, LocalName(vName.getText), OMID(from.path), OMID(to.path), false)
      Moduleadder(v,viewset,controller)

      implicit val rh = new api.presentation.StringBuilder
      val printer = new MMTSyntaxPresenter()
      controller.extman.addExtension(printer)
      printer(v)

      target.dumptoDocument(rh.get)
      SwingUtilities.getWindowAncestor(target).dispose()

    }

    if (ae.getSource==intersectButton) {
      target.resultArea.removeAll()
      val fullview = new DeclaredView(from.parent, LocalName(vName.getText), OMID(from.path), OMID(to.path), false)
      Moduleadder(fullview,viewset,controller)
      target.resultArea.add(new RefactorArea(from,to,fullview,viewset,controller,target))
      target.resultArea.revalidate()
    }

  }

}

class ExistingViewArea(v:DeclaredView,from:DeclaredTheory,to:DeclaredTheory,viewset:Set[(GlobalName,GlobalName)],value:Double,
                        target:MMTRefactorDockable,controller:Controller)
  extends JPanel with ActionListener {

  setLayout(new BoxLayout(this,BoxLayout.X_AXIS))

  private val titlefield = new JTextField(v.name.toString+": "+from.name.toString+" -> "+to.name.toString)
  titlefield.setSize(titlefield.getMinimumSize)
  titlefield.setFont(titlefield.getFont.deriveFont(Font.BOLD))
  titlefield.setEditable(false)
  private val vContent = new JComboBox(viewset.map(o => o._1.^!.last+"?"+o._1.name.toString()
    +" -> "+o._2.^!.last+"?"+o._2.name.toString()).toArray)
  private val valuefield = new JTextField("Value: "+value)
  valuefield.setEditable(false)
  private val intersectButton = new JButton("Refactor")
  intersectButton.addActionListener(this)

  add(titlefield)
  add(vContent)
  add(valuefield)
  add(Box.createRigidArea(new Dimension(5,0)))
  add(intersectButton)

  def actionPerformed(ae:ActionEvent) = {
    if (ae.getSource==intersectButton) {
      target.resultArea.removeAll()
      target.resultArea.add(new RefactorArea(from,to,v,viewset,controller,target))
      target.resultArea.revalidate()
    }
  }
}

class RefactorArea(from:DeclaredTheory,to:DeclaredTheory,cview:DeclaredView,viewset:Set[(GlobalName,GlobalName)],
                   controller:Controller,target:MMTRefactorDockable)
  extends JPanel with ActionListener {

  setLayout(new BoxLayout(this,BoxLayout.Y_AXIS))

  private val titlefield = new JTextArea("Refactoring Theories "+from.name+" and "+to.name+"\nUsing View "+cview.name)
  titlefield.setEditable(false)
  titlefield.setFont(titlefield.getFont.deriveFont(Font.BOLD))

  private val vContent = new JComboBox(viewset.map(o => o._1.^!.last+"?"+o._1.name.toString()
    +" -> "+o._2.^!.last+"?"+o._2.name.toString()).toArray)

  private val intName = new JTextField("IntersectionName")

  private val refButton = new JButton("Refactor")
  refButton.addActionListener(this)

  add(titlefield)
  add(vContent)
  add(intName)
  add(refButton)

  def actionPerformed(ae:ActionEvent) = {
    if (ae.getSource==refButton) {

      val list = Intersecter(cview,from,to,None,Some(LocalName(intName.getText)),Some(""))

      implicit val rh = new api.presentation.StringBuilder
      val printer = new MMTSyntaxPresenter()
      controller.extman.addExtension(printer)
      for (o <- list) printer(o)
      
      target.dumptoDocument(rh.get)
      SwingUtilities.getWindowAncestor(target).dispose()
    }
  }

}

