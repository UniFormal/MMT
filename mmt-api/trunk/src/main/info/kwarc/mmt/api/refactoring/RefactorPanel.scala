package info.kwarc.mmt.api.refactoring

import java.awt.{Font, BorderLayout, Dimension}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.objects.{Term, OMID}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.{ReportHandler, Controller}
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredView, DeclaredTheory}
import info.kwarc.mmt.api.ontology.{MMTExtractor, IsView, IsTheory}
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.symbols.{Constant, FinalConstant}
import scala.util.{Success, Try}

/**
 * Created by raupi on 29.05.15.
 */

class RefactorPanel(ctrl:Controller,publish: String => Unit) extends JPanel with ActionListener {
  val controller = ctrl

  var theories = (controller.memory.content.getModules collect { case t: DeclaredTheory => t }).toList
  var views = (controller.memory.content.getModules collect { case v: DeclaredView => v }).toList

  var theorynames = List().asInstanceOf[List[String]]
  var viewnames = List().asInstanceOf[List[String]]
  val viewfinder = new Viewfinder(controller)
  val intersecter = new Intersecter(controller)
  val mainpanel = new JTabbedPane
  val topPanel = new JPanel
  private val hideButton = new JButton("Hide selected")
  val archs = controller.backend.getArchives
  private val archcombo = new JComboBox(("all"::(archs map (a => a.toString))).toArray)
  private val addarchbutton = new JButton("Load Archive")
  private val unhideButton = new JButton("Unhide all")

  setLayout(new BorderLayout)
  topPanel.setLayout(new BoxLayout(topPanel,BoxLayout.X_AXIS))

  hideButton.addActionListener(this)
  unhideButton.addActionListener(this)
  addarchbutton.addActionListener(this)

  topPanel.add(hideButton)
  topPanel.add(unhideButton)
  topPanel.add(Box.createRigidArea(new Dimension(5,0)))
  topPanel.add(archcombo)
  topPanel.add(addarchbutton)

  val viewpanel = new ViewfinderPanel(this)
  val theorypanel = new TheoriesPanel(this)

  mainpanel.addTab("Theories",theorypanel)
  mainpanel.addTab("Views",viewpanel)

  add(topPanel,BorderLayout.PAGE_START)
  add(mainpanel,BorderLayout.CENTER)

  reinit

  def dropfromList[T](a:T,l:List[T]): List[T] = {
    require (l.contains(a))
    val index = l.indexOf(a)
    l.take(index):::l.drop(index+1)
  }

  def dumptoDocument(s:List[DeclaredModule]) = {
    for (o <- s) {
      controller.add(o)
      controller.library.add(o)
    }
    implicit def add = controller.depstore.+=_
    for (o <- s) MMTExtractor(o)

    implicit val rh = new presentation.StringBuilder
    val presenter = new MMTSyntaxPresenter
    controller.extman.addExtension(presenter)

    for (o <- s) {
      presenter(o)
      o match {
        case t:DeclaredTheory => theories=t::theories
        case v:DeclaredView => views=v::views
      }
    }
    publish(rh.get)
    controller.extman.removeExtension(presenter)
    reinit
  }

  def reinit = {
    theorynames = theories.map(o => o.path.^!.last+"?"+o.path.name)
    viewnames = views.map(o => o.path.^!.last+"?"+o.path.name)
    viewpanel.reinit
    theorypanel.reinit
    this.repaint()
  }


  def actionPerformed(ae: ActionEvent) = {
    if (ae.getSource==unhideButton) {
      viewpanel.hiddenviews=List()
      theorypanel.hiddentheories=List()
      reinit
    }

    if(ae.getSource==hideButton) {
      if (mainpanel.isEnabledAt(0)) {
        for (o <- viewpanel.knownviews:::viewpanel.newviews if o.cb.isSelected) o.hidethis
        viewpanel.reinit
      } else {
        for (o <- theorypanel.theories if o.cb.isSelected) o.hidethis
        theorypanel.reinit
      }
    }

    if(ae.getSource==addarchbutton) {
      mainpanel.removeAll()
      val tf = new JTextArea("Adding relation information of Archives...")
      tf.setEditable(false)
      mainpanel.addTab("Adding Archive...",new JScrollPane(tf))
      addarchbutton.setEnabled(false)
      hideButton.setEnabled(false)
      unhideButton.setEnabled(false)
      mainpanel.revalidate()

      new archloader(tf,
        if (archcombo.getSelectedIndex==0) archs
        else List(archs(archcombo.getSelectedIndex-1))
      ).execute()
    }
  }

  class archloader(texter: JTextArea,archives:List[Archive]) extends javax.swing.SwingWorker[Unit,Void] {
    def doInBackground = {
      for (o <- archives) {
        texter.append("\n - Reading "+o+"...")
        texter.setCaretPosition(texter.getDocument.getLength)
        o.readRelational(Nil,controller,"rel")
      }
      texter.append("\nDone.\n - Loading Theories...")
      texter.setCaretPosition(texter.getDocument.getLength)

      val theories3 = controller.depstore.getInds(IsTheory).toList
      val theories2 = theories3.map(tp =>
        Try(controller.get(tp))
      ) collect {case Success(t) => t}
      val theories1 = theories2 collect { case t : DeclaredTheory => t}


      texter.append("\n - Loading Views...")
      texter.setCaretPosition(texter.getDocument.getLength)

      val views3 = controller.depstore.getInds(IsView).toList
      val views2 = views3.map(tp =>
        Try(controller.get(tp))
      ) collect {case Success(t) => t}
      val views1 = views2 collect { case t : DeclaredView
        if (Try((controller.get(t.from.toMPath),controller.get(t.to.toMPath))) match {case Success(x) => true case _ => false}) => t}

      texter.append("\n - Adding modules...")
      texter.setCaretPosition(texter.getDocument.getLength)
      theories = (theories:::theories1).distinct
      views = (views:::views1).distinct
      texter.append("\nDone.")
      texter.setCaretPosition(texter.getDocument.getLength)
    }

    override def done = {
      mainpanel.removeAll()
      addarchbutton.setEnabled(true)
      hideButton.setEnabled(true)
      unhideButton.setEnabled(true)

      mainpanel.addTab("Theories",theorypanel)
      mainpanel.addTab("Views",viewpanel)
      reinit
    }
  }

}

class ViewfinderPanel(target:RefactorPanel) extends JPanel with ActionListener {

  val TopPanel = new JPanel
  val resultArea = new JPanel
  private val textfield1 = new JTextField("Find Views: From")
  val theoryfield1 = new JComboBox(List("All").toArray)
  private val textfield2 = new JTextField("to")
  val theoryfield2 = new JComboBox(List("All").toArray)
  private val textfield3 = new JTextField("Parameter Cutoff:")
  private val cutoffField = new JTextField("0")
  private val textfield4 = new JTextField("Value cutoff:")
  private val valueField = new JTextField("0")
  private val viewbutton = new JButton("Find Views!")
  var knownviews: List[OldViewPanel] = List()
  var newviews: List[NewViewPanel] = List()
  var hiddenviews: List[ViewPanel] = List()
  private val oldviewsfield = new JTextField("Declared views:")
  private val newviewsfield = new JTextField("New views:")
  private val rslayout = new GroupLayout(resultArea)
  private val topPanel1 = new JPanel
  private val topPanel2 = new JPanel
  val scroll = new JScrollPane(resultArea)
  private val maxbox = new JCheckBox("Maximize Views")
  private val compbox = new JCheckBox("Find maximally consistent unions")

  maxbox.setSelected(true)
  compbox.setSelected(true)
  try { rslayout.setAutoCreateContainerGaps(true)
  rslayout.setAutoCreateGaps(true)
  setLayout(new BorderLayout)
  TopPanel.setLayout(new BoxLayout(TopPanel, BoxLayout.Y_AXIS))
  resultArea.setLayout(rslayout)
  textfield1.setEditable(false)
  textfield2.setEditable(false)
  textfield3.setEditable(false)
  textfield4.setEditable(false)
  viewbutton.addActionListener(this)
  oldviewsfield.setEditable(false)
  oldviewsfield.setFont(oldviewsfield.getFont.deriveFont(Font.BOLD))
  newviewsfield.setEditable(false)
  newviewsfield.setFont(newviewsfield.getFont.deriveFont(Font.BOLD))

  topPanel1.setLayout(new BoxLayout(topPanel1, BoxLayout.X_AXIS))
  topPanel2.setLayout(new BoxLayout(topPanel2, BoxLayout.X_AXIS))

  topPanel1.add(textfield1)
  topPanel1.add(theoryfield1)
  topPanel1.add(textfield2)
  topPanel1.add(theoryfield2)
  topPanel1.add(Box.createRigidArea(new Dimension(5,0)))
  topPanel1.add(viewbutton)
  topPanel2.add(textfield3)
  topPanel2.add(cutoffField)
  topPanel2.add(textfield4)
  topPanel2.add(valueField)
  topPanel2.add(maxbox)
  topPanel2.add(compbox)

  TopPanel.add(topPanel1)
  TopPanel.add(topPanel2)
  add(TopPanel, BorderLayout.PAGE_START)
  add(scroll, BorderLayout.CENTER)
  reinit
  } catch {case e:Exception => e.printStackTrace(); throw e}


  def reinit = try {
    removeAll()
    add(TopPanel, BorderLayout.PAGE_START)
    add(scroll, BorderLayout.CENTER)
    resultArea.removeAll()
    resultArea.setLayout(rslayout)
    theoryfield1.removeAllItems()
    theoryfield2.removeAllItems()
    for (o<- "All"::target.theorynames) {
      theoryfield1.addItem(o)
      theoryfield2.addItem(o)
    }

    val hor = rslayout.createParallelGroup()
    val oldvgroup = rslayout.createSequentialGroup()
    val newvgroup = rslayout.createSequentialGroup()
    val vertseq = rslayout.createSequentialGroup()

    val chgroup1 = rslayout.createParallelGroup(GroupLayout.Alignment.CENTER)
    val namegroup1 = rslayout.createParallelGroup()
    val typegroup1 = rslayout.createParallelGroup()
    val valuegroup1 = rslayout.createParallelGroup()
    val combogroup1 = rslayout.createParallelGroup()
    val intButtongroup1 = rslayout.createParallelGroup(GroupLayout.Alignment.CENTER)

    val chgroup2 = rslayout.createParallelGroup(GroupLayout.Alignment.CENTER)
    val namegroup2 = rslayout.createParallelGroup()
    val typegroup2 = rslayout.createParallelGroup()
    val valuegroup2 = rslayout.createParallelGroup()
    val combogroup2 = rslayout.createParallelGroup()
    val addButtongroup2 = rslayout.createParallelGroup(GroupLayout.Alignment.CENTER)
    val intButtongroup2 = rslayout.createParallelGroup(GroupLayout.Alignment.CENTER)

    knownviews = for {view <- target.views} yield OldViewPanel(view,target)
    val oldviews = knownviews.filter(p => !hiddenviews.contains(p))
    if (oldviews.isEmpty) {
      oldviewsfield.setText("No declared views")
      vertseq.addComponent(oldviewsfield)
    } else {
      oldviewsfield.setText("Declared views:")
      vertseq.addComponent(oldviewsfield)
      for (o <- oldviews) {
        val seq = rslayout.createParallelGroup(GroupLayout.Alignment.CENTER)
        o.cb.setText(oldviews.indexOf(o).toString)
        seq.addComponent(o.cb)
        chgroup1.addComponent(o.cb)
        seq.addComponent(o.nametext)
        namegroup1.addComponent(o.nametext)
        seq.addComponent(o.typetext)
        typegroup1.addComponent(o.typetext)
        seq.addComponent(o.valuefield)
        valuegroup1.addComponent(o.valuefield)
        seq.addComponent(o.combo)
        combogroup1.addComponent(o.combo)
        seq.addComponent(o.intButton)
        intButtongroup1.addComponent(o.intButton)
        vertseq.addGroup(seq)
      }
    }

    hor.addComponent(oldviewsfield)
    oldvgroup.addGroup(chgroup1)
    oldvgroup.addGroup(namegroup1)
    oldvgroup.addGroup(typegroup1)
    oldvgroup.addGroup(valuegroup1)
    oldvgroup.addGroup(combogroup1)
    oldvgroup.addGroup(intButtongroup1)
    hor.addGroup(oldvgroup)

    val unfviews = newviews.filter(p => !hiddenviews.contains(p))
    if (unfviews.isEmpty) {
      newviewsfield.setText("No new views")
      vertseq.addComponent(newviewsfield)
    } else {
      newviewsfield.setText("New views:")
      vertseq.addComponent(newviewsfield)
      for (o <- unfviews) {
        o.cb.setText(unfviews.indexOf(o).toString)
        val seq = rslayout.createParallelGroup(GroupLayout.Alignment.CENTER)

        seq.addComponent(o.cb)
        chgroup2.addComponent(o.cb)
        seq.addComponent(o.nametext)
        namegroup2.addComponent(o.nametext)
        seq.addComponent(o.typetext)
        typegroup2.addComponent(o.typetext)
        seq.addComponent(o.valuefield)
        valuegroup2.addComponent(o.valuefield)
        seq.addComponent(o.combo)
        combogroup2.addComponent(o.combo)
        seq.addComponent(o.addButton)
        addButtongroup2.addComponent(o.addButton)
        seq.addComponent(o.intButton)
        intButtongroup2.addComponent(o.intButton)
        vertseq.addGroup(seq)
      }
    }

    hor.addComponent(newviewsfield)
    newvgroup.addGroup(chgroup2)
    newvgroup.addGroup(namegroup2)
    newvgroup.addGroup(typegroup2)
    newvgroup.addGroup(valuegroup2)
    newvgroup.addGroup(combogroup2)
    newvgroup.addGroup(addButtongroup2)
    newvgroup.addGroup(intButtongroup2)
    hor.addGroup(newvgroup)

    rslayout.setHorizontalGroup(hor)
    rslayout.setVerticalGroup(vertseq)
    resultArea.revalidate()
    scroll.getVerticalScrollBar().setValue(0)
    target.repaint()
  } catch {case e:Exception => e.printStackTrace(); throw e}

  def actionPerformed(ae: ActionEvent) = {
    if(ae.getSource==viewbutton) {

      this.remove(scroll)
      val newresult = new JPanel
      newresult.setLayout(new BorderLayout())
      val progress = new JProgressBar(0,1)
      val textf = new JTextArea("LOOKING FOR VIEWS...")
      textf.setEditable(false)
      progress.setStringPainted(true)
      newresult.add(progress,BorderLayout.PAGE_START)
      newresult.add(new JScrollPane(textf),BorderLayout.CENTER)
      add(newresult,BorderLayout.CENTER)
      revalidate()
      new finderthread(textf,progress,newresult).execute()
    }

  }

  class finderthread(textf: JTextArea,progress:JProgressBar,result:JPanel) extends javax.swing.SwingWorker[Boolean,Void] {
    object textfhandler extends ReportHandler("textfield") {
      def apply(ind: Int, caller: String, group: String, msg: String) = {
        val m = indentString(ind) + group + ": " + msg+"\n"
        textf.append(m)
        textf.setCaretPosition(textf.getText.length)
      }
    }
    def doInBackground:Boolean = {
      target.controller.handleLine("log+ Viewfinder")
      target.viewfinder.report.addHandler(textfhandler)
      if (theoryfield1.getSelectedIndex==0 && theoryfield2.getSelectedIndex==0) {
        val leng= target.theories.length
        val allint = leng*(leng-1)/2
        progress.setMaximum(allint)
        progress.setSize(progress.getMaximumSize)
        progress.revalidate()
        var current = 0
        var found = 0

        for (i <- 0 to target.theories.length-2; j <- i+1 to target.theories.length-1) {
          current+=1
          progress.setString("Looking for Views... "+current+"/"+allint+" - "+found+" morphisms found.")
          progress.setValue(current)
          val th1 = target.theories(i)
          val th2 = target.theories(j)
          val res = target.viewfinder.findByAxioms(th1,th2,cutoffField.getText.toInt,compbox.isSelected,maxbox.isSelected)
          addviews(res,th1,th2)
          found+=res.size
        }
        true
      } else if (theoryfield1.getSelectedIndex==0) {
        val allint = target.theories.length
        progress.setMaximum(allint)
        progress.setSize(progress.getMaximumSize)
        progress.revalidate()
        var current = 0
        var found=0

        val th2 = target.theories(theoryfield2.getSelectedIndex-1)
        for (th1 <- target.theories if th1!=th2) {
          current+=1
          progress.setString("Looking for Views... "+current+"/"+allint+" - "+found+" morphisms found.")
          progress.setValue(current)
          val res = target.viewfinder.findByAxioms(th1,th2,cutoffField.getText.toInt,compbox.isSelected,maxbox.isSelected)
          addviews(res,th1,th2)
          found+=res.size
        }
        true
      } else if (theoryfield2.getSelectedIndex==0) {
        val allint = target.theories.length
        progress.setMaximum(allint)
        progress.setSize(progress.getMaximumSize)
        progress.revalidate()
        var current = 0
        var found = 0

        val th1 = target.theories(theoryfield1.getSelectedIndex-1)
        for (th2 <- target.theories if th1!=th2) {
          current+=1
          progress.setString("Looking for Views... "+current+"/"+allint+" - "+found+" morphisms found.")
          progress.setValue(current)
          val res = target.viewfinder.findByAxioms(th1,th2,cutoffField.getText.toInt,compbox.isSelected,maxbox.isSelected)
          addviews(res,th1,th2)
          found+=res.size
        }
        true
      } else {
        val th1 = target.theories(theoryfield1.getSelectedIndex-1)
        val th2 = target.theories(theoryfield2.getSelectedIndex-1)
        val res = target.viewfinder.findByAxioms(th1,th2,cutoffField.getText.toInt,compbox.isSelected,maxbox.isSelected)
        addviews(res,th1,th2)
        true
      }
    }
    override def done {
      target.viewfinder.report.removeHandler("textfield")
      remove(result)
      if(get) reinit
    }
    def addviews(vs: Set[Set[(GlobalName,GlobalName)]],from:DeclaredTheory,to:DeclaredTheory) = {
      val nviews= vs.map(o => {
        val newset = o.map(p => (target.controller.get(p._1),target.controller.get(p._2))).toList collect {
          case c:(FinalConstant,FinalConstant) => c
        }
        val value = Viewfinder.evaluateViewset(from,to,o)(target.controller)
        NewViewPanel(newset,value,from,to,target)
      }).toList
      newviews = (newviews:::nviews).distinct
    }
  }

}

abstract class ViewPanel(target:RefactorPanel) extends ActionListener {
  val cb: JCheckBox
  val combo: JComboBox[String]
  val intButton: JButton
  val value: Double
  val viewset: List[(FinalConstant,FinalConstant)]
  val others: List[(FinalConstant,Term)]
  val from:DeclaredTheory
  val to:DeclaredTheory

  def hidethis = {
    cb.setSelected(false)
    target.viewpanel.hiddenviews = this::target.viewpanel.hiddenviews
  }
  def intersectthis(view: Option[DeclaredView]) = {
    target.viewpanel.resultArea.removeAll()
    target.viewpanel.resultArea.setLayout(new BorderLayout)
    if (others.nonEmpty && view.isDefined) {
      val pi = new PreIntersect(from, to, target, target.viewpanel.resultArea)
      for (o <- viewset) pi.resultArea.add(o._1, o._2)
      for (o <- others) pi.resultArea.add(o._1, o._2, view.get)
      pi.resultArea.init
      pi.viewbox1.setEnabled(false)
      pi.viewbox1.removeAllItems()
      pi.viewbox1.addItem(view.get.name.toString)
      pi.viewbox2.setEnabled(false)
      pi.checkbutton.setEnabled(false)
      pi.backcb.setEnabled(false)
      pi.viewfindercb.setEnabled(false)
      pi.checked = true
      pi.goButton.setEnabled(true)
      pi.switchbutton.setEnabled(false)
      target.viewpanel.resultArea.add(pi, BorderLayout.CENTER)
    } else target.viewpanel.resultArea.add(new IntersectArea(from,to,viewset,target))
    target.revalidate
  }
}

case class OldViewPanel(v: DeclaredView,target:RefactorPanel) extends ViewPanel(target) {
  val view = v
  val cb = new JCheckBox()
  val intButton = new JButton("Intersect")
  val from = target.controller.get(view.from.toMPath) match {
    case t:DeclaredTheory => t
    case _ => throw new Exception("DeclaredTheory expected!")
  }
  val to = target.controller.get(view.to.toMPath) match {
    case t:DeclaredTheory => t
    case _ => throw new Exception("DeclaredTheory expected!")
  }
  val value = Viewfinder.evaluateView(view,Some(from),Some(to))(target.controller)
  val valuefield = new JTextField("Value: "+(value*100).round+"%")
  val (viewset,others,_) = Intersecter.getPairs(v,None,from,to)
  val combo = new JComboBox((viewset.map(o => o._1.path.^!.last+"?"+o._1.name.toString()
    +" -> "+o._2.path.^!.last+"?"+o._2.name.toString()):::others.map(o =>
    o._1.path.^!.last+"?"+o._1.name.toString() + " -> " +
  {
    implicit val rh = new presentation.StringBuilder
    val presenter = new MMTSyntaxPresenter
    target.controller.extman.addExtension(presenter)
    presenter.objectLevel(o._2,Some(v.path ? (ComplexStep(from.path) / o._1.name) $ DefComponent))
    target.controller.extman.removeExtension(presenter)
    rh.get
  }
  )).toArray)

  val nametext = new JTextField(view.name+":")
  val typetext = new JTextField(from.name+" -> "+to.name)

  nametext.setEditable(false)
  typetext.setEditable(false)
  valuefield.setEditable(false)
  intButton.addActionListener(this)

  def actionPerformed(ae: ActionEvent) = {
    if(ae.getSource==intButton) intersectthis(Some(v))
  }

}

case class NewViewPanel(viewset:List[(FinalConstant,FinalConstant)],value:Double,from:DeclaredTheory,to:DeclaredTheory,
                        target:RefactorPanel) extends ViewPanel(target) {
  val cb = new JCheckBox()
  val valuefield = new JTextField("Value: "+(value*100).round+"%")
  val combo = new JComboBox(viewset.map(o => o._1.path.^!.last+"?"+o._1.name.toString()
    +" -> "+o._2.path.^!.last+"?"+o._2.name.toString()).toArray)
  val addButton = new JButton("Add View")
  val intButton = new JButton("Intersect")
  val nametext = new JTextField("ViewName")
  val typetext = new JTextField(from.name+" -> "+to.name)
  val others = List()

  nametext.setEditable(true)
  typetext.setEditable(false)
  valuefield.setEditable(false)
  addButton.addActionListener(this)
  intButton.addActionListener(this)

  def actionPerformed(ae: ActionEvent) = {
    if(ae.getSource==intButton) intersectthis(None)
    if(ae.getSource==addButton) {
      val view = new DeclaredView(from.parent,LocalName(nametext.getText),OMID(from.path),OMID(to.path),false) // TODO add metamorph?
      Moduleadder(view,viewset.toSet)
      target.dumptoDocument(List(view))
    }
  }

}

class IntersectArea(th1:DeclaredTheory,th2:DeclaredTheory,pairs:List[(FinalConstant,FinalConstant)],target:RefactorPanel) extends JPanel with ActionListener {
  private val rslayout = new GroupLayout(this)
  private val backButton = new JButton("< Back")
  private val text1 = new JTextField("Intersect "+th1.name+" with "+th2.name+": ")
  private val namefield = new JTextField("IntersectionName")
  private val radioGroup = new ButtonGroup
  private val text2 = new JTextField("Use declaration names from:")
  private val radio1 = new JRadioButton(th1.name.toString)
  private val radio2 = new JRadioButton(th2.name.toString)
  private val radio3 = new JRadioButton("Custom Names")
  private val goButton = new JButton("Intersect")
  private val horizontal = rslayout.createParallelGroup()
  private val vertical = rslayout.createSequentialGroup()

  case class declgroup(pair: (FinalConstant,FinalConstant),index:Int) extends ActionListener {
    val thisvertical = rslayout.createParallelGroup()
    val text1 = new JTextField(th1.name+"?"+pair._1.name+" <-> "+th2.name+"?"+pair._2.name+": ")
    val name = new JTextField("Decl"+index)
    val checkbox = new JCheckBox("Add notation: ")
    val notation = new JTextField("None")

    notation.setEditable(false)
    checkbox.setSelected(false)
    text1.setEditable(false)
    checkbox.addActionListener(this)
    thisvertical.addComponent(text1)
    thisvertical.addComponent(name)
    thisvertical.addComponent(checkbox)
    thisvertical.addComponent(notation)

    def actionPerformed(ae:ActionEvent) = {
      if (checkbox.isSelected) notation.setEditable(true) else notation.setEditable(false)
    }
  }

  val decls = pairs.indices.map(i => declgroup(pairs(i),i) )
  radio1.addActionListener(this)
  radio2.addActionListener(this)
  radio3.addActionListener(this)
  backButton.addActionListener(this)
  goButton.addActionListener(this)
  text1.setEditable(false)
  text2.setEditable(false)
  radioGroup.add(radio1)
  radioGroup.add(radio2)
  radioGroup.add(radio3)
  radio3.setSelected(true)
  private val topPar = rslayout.createParallelGroup
  private val topSeq = rslayout.createSequentialGroup
  topPar.addComponent(backButton)
  topSeq.addComponent(backButton)
  topPar.addComponent(text1)
  topSeq.addComponent(text1)
  topPar.addComponent(namefield)
  topSeq.addComponent(namefield)
  topPar.addComponent(goButton)
  topSeq.addComponent(goButton)
  vertical.addGroup(topPar)
  horizontal.addGroup(topSeq)
  private val decnamesPar = rslayout.createParallelGroup
  private val decnamesSeq = rslayout.createSequentialGroup
  decnamesPar.addComponent(text2)
  decnamesPar.addComponent(radio1)
  decnamesPar.addComponent(radio2)
  decnamesPar.addComponent(radio3)
  decnamesSeq.addComponent(text2)
  decnamesSeq.addComponent(radio1)
  decnamesSeq.addComponent(radio2)
  decnamesSeq.addComponent(radio3)
  vertical.addGroup(decnamesPar)
  horizontal.addGroup(decnamesSeq)
  for (o <- decls) vertical.addGroup(o.thisvertical)

  private val declseq = rslayout.createSequentialGroup
  private val text1group = rslayout.createParallelGroup
  for (o <- decls) text1group.addComponent(o.text1)
  private val namegroup = rslayout.createParallelGroup
  for (o <- decls) namegroup.addComponent(o.name)
  private val cbgroup = rslayout.createParallelGroup
  for (o <- decls) cbgroup.addComponent(o.checkbox)
  private val notgroup = rslayout.createParallelGroup
  for (o <- decls) notgroup.addComponent(o.notation)
  declseq.addGroup(text1group)
  declseq.addGroup(namegroup)
  declseq.addGroup(cbgroup)
  declseq.addGroup(notgroup)
  horizontal.addGroup(declseq)
  rslayout.setHorizontalGroup(horizontal)
  rslayout.setVerticalGroup(vertical)

  setLayout(rslayout)

  def actionPerformed(ae:ActionEvent) = {
    if (ae.getSource==radio1 || ae.getSource==radio2 || ae.getSource==radio3) {
      if (radio1.isSelected) for (o <- decls) {
          o.name.setEditable(false)
          o.name.setText(o.pair._1.name.toString)
        }
      if (radio2.isSelected) for (o <- decls) {
        o.name.setEditable(false)
        o.name.setText(o.pair._2.name.toString)
      }
      if (radio3.isSelected) for (o <- decls) {
        o.name.setEditable(true)
      }
    }
    if (ae.getSource ==backButton) target.reinit
    if(ae.getSource == goButton) {
      val result = target.intersecter(th1,th2,decls.map(d => (d.pair._1,d.pair._2,LocalName(d.name.getText),
        if(d.checkbox.isSelected) Some(d.notation.getText) else None)).toList,LocalName(namefield.getText))
      target.dumptoDocument(result)
    }
  }
}

class TheoriesPanel(target:RefactorPanel) extends JPanel with ActionListener {
  var hiddentheories = List().asInstanceOf[List[TheoryPanel]]
  var theories = List().asInstanceOf[List[TheoryPanel]]

  val topPanel = new JPanel
  val resultArea = new JPanel
  private val rslayout = new GroupLayout(resultArea)
  private val text1 = new JTextField("Select two to ")
  private val intButton = new JButton("Intersect")
  private val pushButton = new JButton("Pushout selected")

  intButton.addActionListener(this)
  pushButton.addActionListener(this)
  text1.setEditable(false)
  intButton.setEnabled(false)
  pushButton.setEnabled(false)
  topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.X_AXIS))
  setLayout(new BorderLayout)

  topPanel.add(text1)
  topPanel.add(intButton)
  topPanel.add(pushButton)
  add(topPanel,BorderLayout.PAGE_START)
  add(new JScrollPane(resultArea),BorderLayout.CENTER)

  def reinit = {
    resultArea.removeAll()
    theories = target.theories.map(TheoryPanel(_,target))
    val curths = theories.filter(!hiddentheories.contains(_))

    val hor = rslayout.createSequentialGroup()
    val vert = rslayout.createSequentialGroup()

    val cbpar = rslayout.createParallelGroup()
    val combopar = rslayout.createParallelGroup()
    val buttonpar = rslayout.createParallelGroup()

    if (curths.isEmpty) {
      resultArea.setLayout(new BoxLayout(resultArea, BoxLayout.Y_AXIS))
      val text= new JTextField("No Theories")
      text.setEnabled(false)
      resultArea.add(text)
    }
    else {
      for (o <- curths) {
        val seq = rslayout.createParallelGroup()
        seq.addComponent(o.cb)
        cbpar.addComponent(o.cb)
        seq.addComponent(o.combo)
        combopar.addComponent(o.combo)
        seq.addComponent(o.delButton)
        buttonpar.addComponent(o.delButton)

        vert.addGroup(seq)
      }

      hor.addGroup(cbpar)
      hor.addGroup(combopar)
      hor.addGroup(buttonpar)

      rslayout.setHorizontalGroup(hor)
      rslayout.setVerticalGroup(vert)
      resultArea.setLayout(rslayout)
    }
    checkselectednumber
    resultArea.revalidate()
  }

  def checkselectednumber = {
    val selectedtheories = theories.filter(_.cb.isSelected)
    if (selectedtheories.size==2) {
      intButton.setEnabled(true)
      pushButton.setEnabled(true)
    }
    else {
      intButton.setEnabled(false)
      pushButton.setEnabled(false)
    }
  }

  def actionPerformed(ae:ActionEvent) = {
    if (ae.getSource match {case s:JCheckBox => true case _ => false}) checkselectednumber
    if (ae.getSource==intButton) {
      intButton.setEnabled(false)
      pushButton.setEnabled(false)
      resultArea.removeAll()
      resultArea.setLayout(new BorderLayout)
      val ths = theories.filter(_.cb.isSelected).map(t => t.theory)
      resultArea.add(new PreIntersect(ths.head,ths(1),target,resultArea))
      this.revalidate()
    }
    if (ae.getSource==pushButton) {
      val selectedtheories = theories.filter(_.cb.isSelected)
      resultArea.removeAll()
      resultArea.setLayout(new BoxLayout(resultArea,BoxLayout.Y_AXIS))
      resultArea.add(PushoutArea(selectedtheories.head.theory,selectedtheories.tail.head.theory,target))
      resultArea.revalidate()
    }
  }
}

class PreIntersect(th1:DeclaredTheory,th2:DeclaredTheory,target:RefactorPanel,srcpanel:JPanel) extends JPanel with ActionListener {
  private val mainlayout = new GroupLayout(this)
  private val backButton = new JButton("< Back")
  private val text1 = new JTextField("Intersecting "+th1.name+" with "+th2.name)
  private val text2 = new JTextField("Intersect along:")
  val views1 = target.views.filter(p => p.from.toMPath == th1.path && p.to.toMPath == th2.path)
  val views2 = target.views.filter(p => p.from.toMPath == th2.path && p.to.toMPath == th1.path)
  val switchbutton = new JButton("switch")
  val viewbox1 = new JComboBox(views1.map(v => v.name.toString).toArray)
  val backcb = new JCheckBox("and ")
  val viewbox2 = new JComboBox(views2.map(v => v.name.toString).toArray)
  val checkbutton = new JButton("Check Views")
  val viewfindercb = new JCheckBox("Use Viewfinder instead")
  var checked = false
  val goButton = new JButton("OK")
  val preintersectthis = this

  val resultArea = new JPanel {
    private val resultlayout = new GroupLayout(this)
    private val text0 = new JTextField("Name new declarations: ")
    text0.setEditable(false)
    text0.setVisible(false)
    private val rsvertical = resultlayout.createSequentialGroup()
    private val rshorizontal0 = resultlayout.createParallelGroup
    private val rshorizontal = resultlayout.createSequentialGroup()
    private val text1group = resultlayout.createParallelGroup()
    private val text2group = resultlayout.createParallelGroup()
    private val text3group = resultlayout.createParallelGroup()
    private val namegroup = resultlayout.createParallelGroup()
    private var matchedpairs = Nil.asInstanceOf[List[(FinalConstant,FinalConstant)]]
    private var others1 = Nil.asInstanceOf[List[(FinalConstant,Term,JTextField)]]
    private var others2 = Nil.asInstanceOf[List[(Term,FinalConstant,JTextField)]]
    rshorizontal0.addComponent(text0)
    rsvertical.addComponent(text0)
    rshorizontal0.addGroup(rshorizontal)

    def add (c1:FinalConstant,c2:FinalConstant): Unit = {
      matchedpairs::=(c1,c2)
    }
    def add(c:FinalConstant,t:Term,v:DeclaredView) = {
      val seqgroup = resultlayout.createParallelGroup()
      val text1 = new JTextField(c.name.toString)
      val text2 = new JTextField(" -> ")
      implicit val rh = new presentation.StringBuilder
      val presenter = new MMTSyntaxPresenter
      target.controller.extman.addExtension(presenter)
      presenter.objectLevel(t,Some(v.path ? (ComplexStep(c.parent) / c.name) $ DefComponent))
      val text3 = new JTextField(rh.get)
      target.controller.extman.removeExtension(presenter)

      val name = new JTextField("DeclName"+others1.length)
      text1.setEditable(false)
      text2.setEditable(false)
      text3.setEditable(false)
      seqgroup.addComponent(text1)
      text1group.addComponent(text1)
      seqgroup.addComponent(text2)
      text2group.addComponent(text2)
      seqgroup.addComponent(text3)
      text3group.addComponent(text3)
      seqgroup.addComponent(name)
      namegroup.addComponent(name)
      rsvertical.addGroup(seqgroup)

      others1::=(c,t,name)
    }
    def add(t:Term,c:FinalConstant,v:DeclaredView) = {
      val seqgroup = resultlayout.createParallelGroup()
      val text3 = new JTextField(c.name.toString)
      val text2 = new JTextField(" <- ")
      implicit val rh = new presentation.StringBuilder
      val presenter = new MMTSyntaxPresenter
      target.controller.extman.addExtension(presenter)
      presenter.objectLevel(t,Some(v.path ? (ComplexStep(c.parent) / c.name) $ DefComponent))
      val text1 = new JTextField(rh.get)
      target.controller.extman.removeExtension(presenter)

      val name = new JTextField("DeclName"+others1.length)
      text1.setEditable(false)
      text2.setEditable(false)
      text3.setEditable(false)
      seqgroup.addComponent(text1)
      text1group.addComponent(text1)
      seqgroup.addComponent(text2)
      text2group.addComponent(text2)
      seqgroup.addComponent(text3)
      text3group.addComponent(text3)
      seqgroup.addComponent(name)
      namegroup.addComponent(name)
      rsvertical.addGroup(seqgroup)

      others2::=(t,c,name)
    }
    def init: Unit = {
      rshorizontal.addGroup(text1group)
      rshorizontal.addGroup(text2group)
      rshorizontal.addGroup(text3group)
      rshorizontal.addGroup(namegroup)
      resultlayout.setHorizontalGroup(rshorizontal0)
      resultlayout.setVerticalGroup(rsvertical)
      setLayout(resultlayout)
      if (others1.isEmpty && others2.isEmpty) text0.setText("View has no non-renamings")
        else text0.setText("Name new declarations: ")
      text0.setVisible(true)
      preintersectthis.revalidate()
    }
    def getpairs : List[(FinalConstant,FinalConstant)] = matchedpairs:::others1.map(p => {
      val c2 = Constant(OMID(th2.path),LocalName(p._3.getText),None,
        Moduleadder.substitute(p._1.tp,matchedpairs.map(x => (x._2.path,x._1.path))),Some(p._2),p._1.rl,p._1.notC)
      (p._1,c2)
    }):::others2.map(p => {
      val c1 = Constant(OMID(th1.path),LocalName(p._3.getText),None,
        Moduleadder.substitute(p._2.tp,matchedpairs.map(x => (x._1.path,x._2.path))),Some(p._1),p._2.rl,p._2.notC)
      (c1,p._2)
    })
  }

  switchbutton.addActionListener(this)
  backButton.addActionListener(this)
  text1.setEditable(false)
  text2.setEditable(false)
  backcb.setSelected(false)
  backcb.addActionListener(this)
  viewbox2.setEnabled(false)
  viewfindercb.setSelected(false)
  viewfindercb.addActionListener(this)
  checkbutton.addActionListener(this)
  checkbutton.setEnabled(false)
  goButton.setEnabled(true)
  goButton.addActionListener(this)

  if (views2.isEmpty) {
    viewbox2.addItem("No Views available!")
    backcb.setEnabled(false)
  }
  if (views1.isEmpty) {
    viewbox1.addItem("No views available!")
    viewbox2.addItem("No Views available!")
    viewbox1.setEnabled(false)
    backcb.setEnabled(false)
    viewfindercb.setSelected(true)
    viewfindercb.setEnabled(false)
    goButton.setEnabled(true)
  }

  private val horizontal = mainlayout.createParallelGroup()
  private val vertical = mainlayout.createSequentialGroup()
  private val topseq = mainlayout.createSequentialGroup()
  private val toppar = mainlayout.createParallelGroup()
  topseq.addComponent(backButton)
  topseq.addComponent(text1)
  toppar.addComponent(backButton)
  toppar.addComponent(text1)
  topseq.addComponent(switchbutton)
  toppar.addComponent(switchbutton)
  private val line2seq = mainlayout.createSequentialGroup()
  private val line2par = mainlayout.createParallelGroup()
  line2seq.addComponent(text2)
  line2seq.addComponent(viewbox1)
  line2seq.addComponent(backcb)
  line2seq.addComponent(viewbox2)
  line2seq.addComponent(checkbutton)
  line2par.addComponent(text2)
  line2par.addComponent(viewbox1)
  line2par.addComponent(backcb)
  line2par.addComponent(viewbox2)
  line2par.addComponent(checkbutton)

  horizontal.addGroup(topseq)
  horizontal.addGroup(line2seq)
  horizontal.addComponent(viewfindercb)
  horizontal.addComponent(resultArea)
  horizontal.addComponent(goButton)
  vertical.addGroup(toppar)
  vertical.addGroup(line2par)
  vertical.addComponent(viewfindercb)
  vertical.addComponent(resultArea)
  vertical.addComponent(goButton)
  mainlayout.setHorizontalGroup(horizontal)
  mainlayout.setVerticalGroup(vertical)
  setLayout(mainlayout)

  def actionPerformed(ae:ActionEvent) = {
    if(ae.getSource==switchbutton) {
      srcpanel.removeAll()
      srcpanel.add(new PreIntersect(th2,th1,target,srcpanel))
      srcpanel.revalidate()
    }
    if (viewfindercb.isSelected) {
      viewbox1.setEnabled(false)
      viewbox2.setEnabled(false)
      checkbutton.setEnabled(false)
      backcb.setSelected(false)
      backcb.setEnabled(false)
      goButton.setEnabled(true)
    } else if (backcb.isSelected && !checked) {
      viewbox1.setEnabled(true)
      viewbox2.setEnabled(true)
      checkbutton.setEnabled(true)
      goButton.setEnabled(false)
    } else if (!checked) {
      viewbox1.setEnabled(true)
      viewbox2.setEnabled(false)
      checkbutton.setEnabled(true)
      goButton.setEnabled(false)
      backcb.setEnabled(if (views2.nonEmpty) true else false)
    }
    if (ae.getSource==backButton) target.reinit else
      if(ae.getSource==goButton) (new goThread).execute()
      else if(ae.getSource==checkbutton) {
      viewbox1.setEnabled(false)
      viewbox2.setEnabled(false)
      checkbutton.setEnabled(false)
      backcb.setEnabled(false)
      viewfindercb.setEnabled(false)
      (new builderthread).execute()
      checked = true
    }
  }

  class builderthread extends javax.swing.SwingWorker[Boolean,Void] {
    def doInBackground:Boolean = {
      val v1 = views1(viewbox1.getSelectedIndex)
      val v2 = views2(viewbox2.getSelectedIndex)
      val (p, a, b) = Intersecter.getPairs(v1, Some(v2), th1, th2)
      if ((p,a,b) == (Nil,Nil,Nil)) return false
      p.map(x => resultArea.add(x._1, x._2))
      a.map(x => resultArea.add(x._1,x._2,v1))
      b.map(x => resultArea.add(x._1,x._2,v2))
      true
    }
    override def done {
      if(get) {
        goButton.setEnabled(true)
        resultArea.init
      } else {
        preintersectthis.removeAll()
        preintersectthis.setLayout(new BorderLayout)
        preintersectthis.add(backButton,BorderLayout.PAGE_START)
        val textfield = new JTextField("Chosen morphisms are mutually inconsistent!")
        textfield.setEditable(false)
        preintersectthis.add(textfield,BorderLayout.CENTER)
        preintersectthis.revalidate()
      }
    }
  }
  class goThread extends javax.swing.SwingWorker[Option[List[(FinalConstant,FinalConstant)]],Void] {
    val textfield = new JTextArea("Working...")
    def doInBackground:Option[List[(FinalConstant,FinalConstant)]] = {
      preintersectthis.removeAll()
      preintersectthis.setLayout(new BorderLayout)
      textfield.setEditable(false)
      preintersectthis.add(textfield,BorderLayout.CENTER)
      preintersectthis.revalidate()
      if (viewfindercb.isSelected) {
        target.viewfinder.findBest(th1,th2) match {
          case None => None
          case Some(x) => Some(Viewfinder.viewtoviewset(x._1)(target.controller))
        }
      } else if (checked) {
        Some(resultArea.getpairs)
      } else Some(Viewfinder.viewtoviewset(views1(viewbox1.getSelectedIndex))(target.controller))
    }
    override def done: Unit = {
      get match {
        case None => textfield.append("\nNo applicable morphism found!")
          preintersectthis.add(backButton,BorderLayout.PAGE_END)
        case Some(x) => srcpanel.removeAll()
          srcpanel.add(new IntersectArea(th1,th2,x,target))
          srcpanel.revalidate()
      }
    }
  }
}


case class TheoryPanel(th:DeclaredTheory,target:RefactorPanel) extends ActionListener {
  val theory = th
  val cb = new JCheckBox(th.path.^!.last+"?"+th.path.name)
  val consts = th.getConstants collect {case c:FinalConstant => c}
  val combo = new JComboBox(consts.map(c => c.name.toString).toArray)
  val delButton = new JButton("Delete Declarations")
  delButton.addActionListener(this)
  cb.addActionListener(target.theorypanel)

  def hidethis = {
    target.theorypanel.hiddentheories = this::target.theorypanel.hiddentheories
    cb.setSelected(false)
  }

  def actionPerformed(ae:ActionEvent) = {
    target.theorypanel.resultArea.removeAll()
    target.theorypanel.resultArea.setLayout(new BoxLayout(target.theorypanel.resultArea,BoxLayout.Y_AXIS))
    target.theorypanel.resultArea.add(DeletionArea(th,target))
    target.theorypanel.resultArea.revalidate()
  }
}

case class DeletionArea(th:DeclaredTheory,target:RefactorPanel) extends JPanel with ActionListener {
  val glayout = new GroupLayout(this)
  val text1 = new JTextField("Remove (dependency closure of) selected components from theory "+th.name+": ")
  val name = new JTextField("TheoryName")
  val tp = new JPanel
  val doButton = new JButton("Go")
  val backButton = new JButton("<Back")
  val consts = th.getConstants collect {case c:FinalConstant => ConstantCb(c)}

  tp.setLayout(new BoxLayout(tp,BoxLayout.X_AXIS))
  text1.setEditable(false)
  tp.add(backButton)
  tp.add(text1)
  tp.add(name)
  tp.add(doButton)
  doButton.addActionListener(this)
  backButton.addActionListener(this)

  setLayout(new BoxLayout(this,BoxLayout.Y_AXIS))
  add(tp)
  for (c <- consts) add(c)

  case class ConstantCb(c:FinalConstant) extends JCheckBox(c.name.toString) {
    val const = c
  }

  def actionPerformed(ae:ActionEvent) = if (ae.getSource==doButton) target.dumptoDocument(List(SubtractDeclaration(
        th,
        consts.filter(_.isSelected).map(c => c.const.path),
        target.controller,
        Some(LocalName(name.getText))))) else
    if(ae.getSource==backButton) target.reinit
}

case class PushoutArea(thA:DeclaredTheory,thB:DeclaredTheory,target:RefactorPanel)
  extends JPanel with ActionListener {

  val others = target.dropfromList(thA,target.dropfromList(thB,target.theories))
  setLayout(new BoxLayout(this,BoxLayout.X_AXIS))

  private val titlefield = new JTextField("Pushout: "+thA.name+" <-> "+thB.name+" using:")
  titlefield.setEditable(false)
  titlefield.setFont(titlefield.getFont.deriveFont(Font.BOLD))

  private val backButton = new JButton(("< Back"))
  backButton.addActionListener(this)

  private val thnames = others.map(t => t.path.^!.last+"?"+t.name).toArray
  private val thCbox = new JComboBox("Find best"+:thnames)

  private val namefield = new JTextField("PushoutName")

  private val pushoutbutton = new JButton("Create Pushout")
  pushoutbutton.addActionListener(this)

  private val viewfinderbox = new JCheckBox("Use Viewfinder:",true)

  add(backButton)
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
          for (t <- others; v <- target.viewfinder(thA, t) ::: target.viewfinder(thB, t)) target.controller.add(v)
        } else {
          for (v <- target.viewfinder(thA,others(thCbox.getSelectedIndex - 1))
            :::target.viewfinder(thB,others(thCbox.getSelectedIndex - 1))) target.controller.add(v)
        }
      }

      val ret = if (thCbox.getSelectedIndex == 0) Unifier(thA, thB, target.controller, false, Some(LocalName(namefield.getText)),Some(""))
      else Unifier(thA, thB, others(thCbox.getSelectedIndex - 1), target.controller, false, Some(LocalName(namefield.getText)),
        Some(others(thCbox.getSelectedIndex - 1).name))

      target.dumptoDocument(ret)
    }
    if (ae.getSource==backButton) target.reinit
  }
}