package info.kwarc.mmt.jedit

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Font, Dimension, BorderLayout}
import javax.swing._
import info.kwarc.mmt.api
import info.kwarc.mmt.api.objects.OMID
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredView, DeclaredTheory}
import info.kwarc.mmt.api.refactoring._
import org.gjt.sp.jedit.{jEdit, View}

// TODO : Order module content by dependencies

class MMTRefactorDockable(view: View, position: String) extends JPanel with ActionListener {
  val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
  val controller = mmt.controller
  val topPanel = new JPanel
  val resultArea = new JPanel
  val findViewsHereButton = new JButton("Find Views")
  val addTheoriesButton = new JButton("Add declared Theories")
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
    addTheoriesButton.addActionListener(this)
    addViewsButton.addActionListener(this)

    topPanel.add(findViewsHereButton)
    topPanel.add(addViewsButton)
    topPanel.add(Box.createRigidArea(new Dimension(5,0)))
    topPanel.add(textfield1)
    topPanel.add(cutoffField)
    topPanel.add(Box.createRigidArea(new Dimension(5,0)))
    topPanel.add(textfield2)
    topPanel.add(valueField)
    topPanel.add(addTheoriesButton)
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
            (for {o <- list} yield (o,evaluateViewset(p.head,p.tail.head,o))
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
              resultArea.add(new ResultViewArea(o._1, o._2, p._1, p._2, this))
            }
          }
        } else text.append("\nNo Views found!")

        resultArea.revalidate()
        this.repaint()

      }

      if (ae.getSource == addTheoriesButton) {
        resultArea.removeAll()
        val docths = controller.memory.content.getModules.toList collect {case t: DeclaredTheory => t}
        for (o <- docths) resultArea.add(new ResultTheoryArea(o,
          docths.take(docths.indexOf(o))++docths.drop(docths.indexOf(o)+1),this))
        resultArea.revalidate()
        this.repaint()
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

          val vset = viewtoviewset(o)
          resultArea.add(new ExistingViewArea(o,dom,cod,vset,evaluateViewset(dom,cod,vset),this))
        }
        resultArea.revalidate()
        this.repaint()
      }

    } catch {
      case e:Exception => e.printStackTrace(); throw e
    }

  }

  def evaluateViewset(from:DeclaredTheory,to:DeclaredTheory,viewset:Set[(GlobalName,GlobalName)]): Double = {
    val domc = from.getConstants collect { case c: FinalConstant => c}

    val codc = to.getConstants collect { case c: FinalConstant => c}

    viewset.filter(p => domc.exists(q => q.path==p._1)).toList.length.toDouble / (if (domc.length<codc.length) domc.length else codc.length).toDouble

  }

  def viewtoviewset(v:DeclaredView) : Set[(GlobalName,GlobalName)] = {

    val pairs = v.domain.map(name => {
     val domname = Path.parse(name.head.toString.tail.dropRight(1)).toTriple match {
       case (Some(d:DPath),Some(m:LocalName),None) =>
         GlobalName(OMID(MPath(d,m)) ,name.tail)
       case _ => throw new Exception("Module Path expected!")
     }
     val codname = v.get(name) match {
       case c:FinalConstant => c.df.get match {
         case t:OMID => controller.get(t.path) match {
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

  def dumptoDocument(s:List[DeclaredModule]) = {
    for (o <- s) controller.add(o)


    implicit val rh = new api.presentation.StringBuilder
    val presenter = new MMTSyntaxPresenter
    controller.extman.addExtension(presenter)

    for (o <- s) presenter(o)
    view.getTextArea.setText(view.getTextArea.getText+"\n\n"+rh.get)
    SwingUtilities.getWindowAncestor(this).dispose()
  }

}

class ResultViewArea(from:DeclaredTheory,to:DeclaredTheory,viewset:Set[(GlobalName,GlobalName)],value:Double,
                      target:MMTRefactorDockable)
  extends JPanel with ActionListener {

  setLayout(new BoxLayout(this,BoxLayout.X_AXIS))
  private val vContent = new JComboBox(viewset.map(o => o._1.^!.last+"?"+o._1.name.toString()
    +" -> "+o._2.^!.last+"?"+o._2.name.toString()).toArray)
  private val valuefield = new JTextField("Value: "+value)
  valuefield.setEditable(false)
  private val vName = new JTextField("ViewName")
  private val addButton = new JButton("Add View")
  addButton.addActionListener(this)
  private val intersectButton = new JButton("Intersect")
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
      Moduleadder(v,viewset,target.controller)
      target.dumptoDocument(List(v))

    }

    if (ae.getSource==intersectButton) {
      target.resultArea.removeAll()
      val fullview = new DeclaredView(from.parent, LocalName(vName.getText), OMID(from.path), OMID(to.path), false)
      Moduleadder(fullview,viewset,target.controller)
      target.resultArea.add(new IntersectArea(from,to,fullview,viewset,target))
      target.resultArea.revalidate()
    }

  }

}

class ExistingViewArea(v:DeclaredView,from:DeclaredTheory,to:DeclaredTheory,viewset:Set[(GlobalName,GlobalName)],value:Double,
                        target:MMTRefactorDockable)
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
  private val intersectButton = new JButton("Intersect")
  intersectButton.addActionListener(this)

  add(titlefield)
  add(vContent)
  add(valuefield)
  add(Box.createRigidArea(new Dimension(5,0)))
  add(intersectButton)

  def actionPerformed(ae:ActionEvent) = {
    if (ae.getSource==intersectButton) {
      target.resultArea.removeAll()
      target.resultArea.add(new IntersectArea(from,to,v,viewset,target))
      target.resultArea.revalidate()
    }
  }
}

class IntersectArea(from:DeclaredTheory,to:DeclaredTheory,cview:DeclaredView,viewset:Set[(GlobalName,GlobalName)],
                   target:MMTRefactorDockable)
  extends JPanel with ActionListener {

  setLayout(new BoxLayout(this,BoxLayout.Y_AXIS))

  private val titlefield = new JTextArea("Intersecting theories "+from.name+" and "+to.name+"\nUsing View "+cview.name)
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
      target.dumptoDocument(list)
    }
  }

}

class ResultTheoryArea(th:DeclaredTheory,others:List[DeclaredTheory],target:MMTRefactorDockable)
  extends JPanel with ActionListener {
  setLayout(new BoxLayout(this,BoxLayout.X_AXIS))

  private val titlefield = new JTextField(th.path.^!.last+"?"+th.name)
  titlefield.setEditable(false)
  private val viewButton = new JButton("Find views to:")
  viewButton.addActionListener(this)
  private val thnames = others.map(t => t.path.^!.last+"?"+t.name).toArray
  private val viewbox = new JComboBox("All"+:thnames)
  private val consts = th.getConstants collect {case c: FinalConstant => c}
  private val delButton = new JButton("Delete Declaration:")
  delButton.addActionListener(this)
  private val constBox = new JComboBox(consts.map(c => c.name.toString()).toArray)
  private val pushoutButton = new JButton("Pushout with:")
  pushoutButton.addActionListener(this)
  private val thBox = new JComboBox(thnames)

  add(titlefield)
  add(Box.createRigidArea(new Dimension(5,0)))
  add(viewButton)
  add(viewbox)
  add(Box.createRigidArea(new Dimension(5,0)))
  add(delButton)
  add(constBox)
  add(Box.createRigidArea(new Dimension(5,0)))
  add(pushoutButton)
  add(thBox)

  def actionPerformed(ae:ActionEvent) = {

    if (ae.getSource==viewButton) {

      target.resultArea.removeAll()
      val text = new JTextArea("Looking for views...")
      text.setEditable(false)
      target.resultArea.add(text)
      target.resultArea.revalidate()
      val pairs = if (viewbox.getSelectedIndex==0) others.map(t => (th,t)) else List((th,others(viewbox.getSelectedIndex-1)))
      val views = (for {p <- pairs} yield {
        text.append("\n" + p._1.name + " -> " + p._2.name + "...")
        target.repaint()
        val allviews = {
          val list = Viewfinder.findByAxioms(p._1,p._2,target.controller,target.cutoffField.getText.toInt,true,true)
          (for {o <- list} yield (o,target.evaluateViewset(p._1,p._2,o))
            ).filter(p => p._2>=target.valueField.getText.toDouble && p._2>0)
        }
        text.append(allviews.toList.length+" Views found!")
        target.repaint()
        (p._1,p._2,allviews)
      }).filter(p => p._3.nonEmpty)
      text.append("\nDone.")
      target.repaint()
      if (views.nonEmpty) {
        target.resultArea.remove(text)

        for (o <- views) {
          val titlefield = new JTextField(o._1.name + " -> " + o._2.name)
          titlefield.setEditable(false)
          titlefield.setSize(titlefield.getMinimumSize)
          titlefield.setFont(titlefield.getFont.deriveFont(Font.BOLD))

          target.resultArea.add(titlefield)

          for (p <- o._3) {
            target.resultArea.add(new ResultViewArea(o._1, o._2, p._1, p._2,target))
          }
        }
      } else text.append("\nNo Views found!")

      target.resultArea.revalidate()
      target.repaint()

    } else

    if (ae.getSource==delButton) {

      val newth = SubtractDeclaration(th,consts(constBox.getSelectedIndex),target.controller,Some(th.name))
      target.dumptoDocument(List(newth))

    } else

    if (ae.getSource==pushoutButton) {
      target.resultArea.removeAll()
      val th2index = thBox.getSelectedIndex
      target.resultArea.add(new PushoutArea(th,others(th2index),
        others.take(th2index)++others.drop(th2index+1),target))
      target.resultArea.revalidate()
    }

  }
}

class PushoutArea(thA:DeclaredTheory,thB:DeclaredTheory,others:List[DeclaredTheory],target:MMTRefactorDockable)
  extends JPanel with ActionListener {

  setLayout(new BoxLayout(this,BoxLayout.X_AXIS))

  private val titlefield = new JTextField("Pushout: "+thA.name+" <-> "+thB.name+" using:")
  titlefield.setEditable(false)
  titlefield.setFont(titlefield.getFont.deriveFont(Font.BOLD))

  private val thnames = others.map(t => t.path.^!.last+"?"+t.name).toArray
  private val thCbox = new JComboBox("Find best"+:thnames)

  private val namefield = new JTextField("PushoutName")

  private val pushoutbutton = new JButton("Create Pushout")
  pushoutbutton.addActionListener(this)

  private val viewfinderbox = new JCheckBox("Use Viewfinder:",true)

  add(titlefield)
  add(thCbox)
  add(Box.createRigidArea(new Dimension(5,0)))
  add(namefield)
  add(viewfinderbox)
  add(pushoutbutton)

  def actionPerformed(ae:ActionEvent) = {
    if (ae.getSource==pushoutbutton) {
      if (viewfinderbox.isSelected) {
        if (thCbox.getSelectedIndex == 0) {
          for (t <- others; v <- Viewfinder(thA, t, target.controller) ::: Viewfinder(thB, t, target.controller)) target.controller.add(v)
        } else {
          for (v <- Viewfinder(thA,others(thCbox.getSelectedIndex - 1),target.controller)
            :::Viewfinder(thB,others(thCbox.getSelectedIndex - 1),target.controller)) target.controller.add(v)
        }
      }

      val ret = if (thCbox.getSelectedIndex == 0) Unifier(thA, thB, target.controller, false, Some(LocalName(namefield.getText)),Some(""))
      else Unifier(thA, thB, others(thCbox.getSelectedIndex - 1), target.controller, false, Some(LocalName(namefield.getText)),
        Some(others(thCbox.getSelectedIndex - 1).name))

      target.dumptoDocument(ret)
    }
  }
}