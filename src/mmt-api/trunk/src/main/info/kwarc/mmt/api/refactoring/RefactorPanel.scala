package info.kwarc.mmt.api.refactoring

import java.awt.{Font, BorderLayout, Dimension}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.objects.OMID
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{DeclaredModule, DeclaredView, DeclaredTheory}
import info.kwarc.mmt.api.ontology.{IsView, IsTheory}
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.symbols.FinalConstant

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

  mainpanel.addTab("Views",viewpanel)
  mainpanel.addTab("Theories",theorypanel)

  add(topPanel,BorderLayout.PAGE_START)
  add(mainpanel,BorderLayout.CENTER)

  reinit

  def dropfromList[T](a:T,l:List[T]): List[T] = {
    require (l.contains(a))
    val index = l.indexOf(a)
    l.take(index):::l.drop(index+1)
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
        texter.append("\n - Loading "+o+"...")
        texter.setCaretPosition(texter.getDocument.getLength)
        o.readRelational(Nil,controller,"rel")
      }
      texter.append("\nDone.")
      texter.setCaretPosition(texter.getDocument.getLength)
    }

    override def done = {
      mainpanel.removeAll()
      addarchbutton.setEnabled(true)
      hideButton.setEnabled(true)
      unhideButton.setEnabled(true)

      mainpanel.addTab("Views",viewpanel)
      mainpanel.addTab("Theories",theorypanel)

      theories = ((controller.depstore.getInds(IsTheory).map(tp => controller.get(tp))
        collect { case t : DeclaredTheory => t}).toList:::theories).distinct
      views = ((controller.depstore.getInds(IsView).map(tp => controller.get(tp))
        collect { case t : DeclaredView => t}).toList:::views).distinct
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

      resultArea.removeAll()
      resultArea.setLayout(new BoxLayout(resultArea, BoxLayout.Y_AXIS))
      resultArea.revalidate()
      val textf = new JTextArea("LOOKING FOR VIEWS...")
      textf.setEditable(false)
      resultArea.add(textf)
      resultArea.revalidate()
      new finderthread(textf).execute()
    }

  }

  class finderthread(textf: JTextArea) extends javax.swing.SwingWorker[Boolean,Void] {
    def doInBackground:Boolean = {

      // This is basically a copy of Viewfinder.findByAxioms !
      def findThis(th1:DeclaredTheory,th2:DeclaredTheory,cutoff:Int,makeComp:Boolean,maximize:Boolean)
      : Set[Set[(GlobalName,GlobalName)]] = {

        textf.append("\n - hashing + pairing...")
        textf.setCaretPosition(textf.getDocument.getLength)
        val allhashes = Consthash(th1,th2,target.controller)
        val allpairs = for {a <- allhashes._1; b <- allhashes._2 if a.hash==b.hash} yield (a,b)
        val pairedbyaxioms = allpairs.filter(p => p._1.isAxiom && p._1.pars.length>=cutoff).toIndexedSeq


        textf.append("\n - finding Views...")
        textf.setCaretPosition(textf.getDocument.getLength)
        val allPaths = pairedbyaxioms.indices.map(i =>
          target.viewfinder.findByAxiomsIterator(allpairs,pairedbyaxioms(i),List()).getOrElse(List()).distinct)

        val max = if(maximize) {
          textf.append("\n - maximizing...")
          textf.setCaretPosition(textf.getDocument.getLength)
          allPaths.map(o => target.viewfinder.makeMaximal(o,allpairs,Some(pairedbyaxioms))).distinct
        }
        else allPaths

        val comp = (for {o <- if (makeComp) {
          textf.append("\n - finding maximally consistent unions...")
          textf.setCaretPosition(textf.getDocument.getLength)
          target.viewfinder.makeCompatible(max,List())
        } else max} yield o.toSet).toList.sortBy(_.size)


        textf.append("\n - filtering out redundant subviews...")
        textf.setCaretPosition(textf.getDocument.getLength)
        comp.foldRight(comp)((b,s) =>
          if (!s.contains(b)) s else {
            val i = s.indexOf(b)
            s.take(i).filter(p => !p.subsetOf(b)):::s.drop(i)
          }
        ).toSet

      }

      val maximize = maxbox.isSelected
      val makecomp = compbox.isSelected

      val pairs = (for {
        a <- if (theoryfield1.getSelectedIndex==0) target.theories
        else List(target.theories(theoryfield1.getSelectedIndex-1));
        b <- if (theoryfield2.getSelectedIndex==0) target.dropfromList(a,target.theories)
        else List(target.theories(theoryfield2.getSelectedIndex-1))
        if a!=b
      } yield Set(a, b)).toSet

      val views = pairs.map(p => {
        textf.append("\nLOOKING FOR VIEWS " + p.head.name + " -> " + p.tail.head.name + "...  ")
        textf.setCaretPosition(textf.getDocument.getLength)

        val allviews = {
          val list = findThis(p.head,p.tail.head,
            try {cutoffField.getText.toInt} catch {case e:Exception => 0},maximize,makecomp)
          (for {o <- list} yield (o,target.evaluateViewset(p.head,p.tail.head,o))
            ).filter(p => (p._2*100)>=(try {valueField.getText.toDouble}
          catch {case e:Exception => 0}) && p._2>0)
        }
        textf.append("\n - "+allviews.toList.length+" Views found!")
        textf.setCaretPosition(textf.getDocument.getLength)

        (p.head,p.tail.head,allviews)
      }).filter(p => p._3.nonEmpty)
      textf.append("\nDone.")
      textf.setCaretPosition(textf.getDocument.getLength)

      if (views.nonEmpty) {
        newviews = List()
        for (o <- views; v <- o._3) newviews = NewViewPanel(v._1,v._2,o._1,o._2,target)::newviews
        true
      } else {
        textf.append("\nNo Views found!")
        textf.setCaretPosition(textf.getDocument.getLength)
        false
      }

    }
    override def done { if(get) reinit }

  }

}

abstract class ViewPanel(target:RefactorPanel) extends ActionListener {
  val cb: JCheckBox
  val combo: JComboBox[String]
  val intButton: JButton
  val value: Double
  val viewset: Set[(GlobalName,GlobalName)]
  val from:DeclaredTheory
  val to:DeclaredTheory
  //val objects:List[JComponent]

  def hidethis = {
    cb.setSelected(false)
    target.viewpanel.hiddenviews = this::target.viewpanel.hiddenviews
  }
  def intersectthis(view:DeclaredView) = {
    target.viewpanel.resultArea.removeAll()
    target.viewpanel.resultArea.setLayout(new BoxLayout(target.viewpanel.resultArea, BoxLayout.Y_AXIS))
    target.viewpanel.resultArea.add(new IntersectViewArea(view,viewset,from,to,target))
    target.viewpanel.resultArea.revalidate()
  }
}

case class OldViewPanel(v: DeclaredView,target:RefactorPanel) extends ViewPanel(target) {
  val view = v
  val cb = new JCheckBox()
  val viewset = target.viewtoviewset(view)
  val value = target.viewfinder.evaluateView(view)
  val valuefield = new JTextField("Value: "+(value*100).round+"%")
  val combo = new JComboBox(viewset.map(o => o._1.^!.last+"?"+o._1.name.toString()
    +" -> "+o._2.^!.last+"?"+o._2.name.toString()).toArray)
  val intButton = new JButton("Intersect")
  val from = target.controller.get(view.from.toMPath) match {
    case t:DeclaredTheory => t
    case _ => throw new Exception("DeclaredTheory expected!")
  }
  val to = target.controller.get(view.to.toMPath) match {
    case t:DeclaredTheory => t
    case _ => throw new Exception("DeclaredTheory expected!")
  }
  val nametext = new JTextField(view.name+":")
  val typetext = new JTextField(from.name+" -> "+to.name)

  nametext.setEditable(false)
  typetext.setEditable(false)
  valuefield.setEditable(false)
  intButton.addActionListener(this)

  def actionPerformed(ae: ActionEvent) = {
    if(ae.getSource==intButton) intersectthis(view)
  }

}

case class NewViewPanel(viewset:Set[(GlobalName,GlobalName)],value:Double,from:DeclaredTheory,to:DeclaredTheory,
                        target:RefactorPanel) extends ViewPanel(target) {
  val cb = new JCheckBox()
  val valuefield = new JTextField("Value: "+(value*100).round+"%")
  val combo = new JComboBox(viewset.map(o => o._1.^!.last+"?"+o._1.name.toString()
    +" -> "+o._2.^!.last+"?"+o._2.name.toString()).toArray)
  val addButton = new JButton("Add View")
  val intButton = new JButton("Intersect")
  val nametext = new JTextField("ViewName")
  val typetext = new JTextField(from.name+" -> "+to.name)

  nametext.setEditable(true)
  typetext.setEditable(false)
  valuefield.setEditable(false)
  addButton.addActionListener(this)
  intButton.addActionListener(this)

  def actionPerformed(ae: ActionEvent) = {
    if(ae.getSource==intButton) {
      val view = new DeclaredView(from.parent,LocalName(nametext.getText),OMID(from.path),OMID(to.path),false) // TODO add metamorph?
      Moduleadder(view,viewset,target.controller)
      intersectthis(view)
    }
    if(ae.getSource==addButton) {
      val view = new DeclaredView(from.parent,LocalName(nametext.getText),OMID(from.path),OMID(to.path),false) // TODO add metamorph?
      Moduleadder(view,viewset,target.controller)
      target.dumptoDocument(List(view))
    }
  }

}

class IntersectViewArea(view:DeclaredView,viewset:Set[(GlobalName,GlobalName)],from:DeclaredTheory,
                    to:DeclaredTheory,target:RefactorPanel) extends JPanel with ActionListener {
  private val titlefield = new JTextField("Intersecting "+from.name+" and "+to.name+" along View:")
  private val combo = new JComboBox(viewset.map(o => o._1.^!.last+"?"+o._1.name.toString()
    +" -> "+o._2.^!.last+"?"+o._2.name.toString()).toArray)
  private val backButton = new JButton("< Back")

  private val cb = new JCheckBox("Use original Domain-/Codomain names")
  private val textfield1 = new JTextField("Use suffix instead:")
  private val suffixfield = new JTextField("suffix")
  private val textfield2 = new JTextField("Name of theory intersection:")
  private val intname = new JTextField("IntersectionName")
  private val goButton = new JButton("Intersect")
  private val glayout = new GroupLayout(this)
  private val TopPanel1 = glayout.createParallelGroup(GroupLayout.Alignment.CENTER)
  private val TopPanel2 = glayout.createSequentialGroup()

  backButton.addActionListener(this)
  titlefield.setEditable(false)
  titlefield.setFont(titlefield.getFont.deriveFont(Font.BOLD))
  titlefield.setHorizontalAlignment(SwingConstants.RIGHT)
  TopPanel1.addComponent(backButton)
  TopPanel1.addComponent(titlefield)
  TopPanel1.addComponent(combo)
  TopPanel2.addComponent(backButton)
  TopPanel2.addComponent(titlefield)
  TopPanel2.addComponent(combo)

  cb.setSelected(true)
  textfield1.setEditable(false)
  suffixfield.setEditable(false)
  cb.addActionListener(this)
  textfield2.setEditable(false)
  goButton.addActionListener(this)

  private val vert = glayout.createSequentialGroup()
  private val hor = glayout.createParallelGroup(GroupLayout.Alignment.LEADING)
  vert.addGroup(TopPanel1)
  hor.addGroup(TopPanel2)
  vert.addComponent(cb)
  hor.addComponent(cb)
  private val group1a=glayout.createParallelGroup(GroupLayout.Alignment.LEADING)
  private val group1b=glayout.createSequentialGroup()
  group1a.addComponent(textfield1)
  group1a.addComponent(suffixfield)
  group1b.addComponent(textfield1)
  group1b.addComponent(suffixfield)
  vert.addGroup(group1a)
  hor.addGroup(group1b)
  private val group2a=glayout.createParallelGroup(GroupLayout.Alignment.LEADING)
  private val group2b=glayout.createSequentialGroup()
  group2a.addComponent(textfield2)
  group2a.addComponent(intname)
  group2b.addComponent(textfield2)
  group2b.addComponent(intname)
  vert.addGroup(group2a)
  hor.addGroup(group2b)
  vert.addComponent(goButton)
  hor.addComponent(goButton)

  glayout.linkSize(BoxLayout.Y_AXIS,backButton,titlefield,cb,textfield1,suffixfield,textfield2,goButton,combo,intname)

  glayout.setHorizontalGroup(hor)
  glayout.setVerticalGroup(vert)

  setLayout(glayout)
  target.viewpanel.resultArea.revalidate()
  target.repaint()

  def actionPerformed(ae:ActionEvent) = {
    if(ae.getSource==backButton) target.viewpanel.reinit
    if(ae.getSource==cb) if(cb.isSelected) suffixfield.setEditable(false) else suffixfield.setEditable(true)

    if(ae.getSource==goButton) {
      val list = target.intersecter(view,from,to,None,Some(LocalName(intname.getText)),
        if (cb.isSelected) Some("") else Some(suffixfield.getText))
      if (cb.isSelected) {
        target.theories = target.dropfromList(from,target.theories)
        target.theories = target.dropfromList(to,target.theories)
      }
      target.dumptoDocument(list)
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
  private val intButton = new JButton("Intersect along:")
  private var allowedviews = List().asInstanceOf[List[DeclaredView]]
  private val viewCombo = new JComboBox(("Use Viewfinder"::allowedviews.map(_.name.toString)).toArray)
  private val pushButton = new JButton("Pushout selected")

  intButton.addActionListener(this)
  pushButton.addActionListener(this)
  text1.setEditable(false)
  intButton.setEnabled(false)
  pushButton.setEnabled(false)
  viewCombo.setEnabled(false)
  topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.X_AXIS))
  setLayout(new BorderLayout)

  topPanel.add(text1)
  topPanel.add(intButton)
  topPanel.add(viewCombo)
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
    resultArea.revalidate()
  }

  def actionPerformed(ae:ActionEvent) = {
    if (ae.getSource match {case s:JCheckBox => true case _ => false}) {
      val selectedtheories = theories.filter(_.cb.isSelected)
      if (selectedtheories.size==2) {
        allowedviews = target.views.filter(v => (v.from.toMPath==selectedtheories.head.theory.path
          && v.to.toMPath==selectedtheories.tail.head.theory.path)
          || (v.to.toMPath==selectedtheories.head.theory.path
          && v.from.toMPath==selectedtheories.tail.head.theory.path)
        )
        viewCombo.removeAllItems()
        viewCombo.addItem("Use Viewfinder")
        for (o <- allowedviews) viewCombo.addItem(o.name.toString)
        viewCombo.setEnabled(true)
        intButton.setEnabled(true)
        pushButton.setEnabled(true)
      }
      else {
        intButton.setEnabled(false)
        pushButton.setEnabled(false)
        viewCombo.setEnabled(false)
      }
    }
    if (ae.getSource==intButton) {
      if(viewCombo.getSelectedIndex== 0) {
        val selectedtheories = theories.filter(_.cb.isSelected)
        for (o <- theories) o.cb.setSelected(false)
        resultArea.removeAll()
        resultArea.setLayout(new BoxLayout(resultArea, BoxLayout.Y_AXIS))
        resultArea.add(new IntersectTheoriesArea(selectedtheories.head.theory,selectedtheories.tail.head.theory,target))
        resultArea.revalidate()
      }
      else {
        val v = target.viewpanel.knownviews.collectFirst{
          case p if p.view==allowedviews(viewCombo.getSelectedIndex-1) => p
        }.getOrElse(throw new Exception("View missing!"))
        target.mainpanel.setSelectedIndex(0)
        v.intersectthis(v.view)
      }
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

class IntersectTheoriesArea(from:DeclaredTheory,
                        to:DeclaredTheory,target:RefactorPanel) extends JPanel with ActionListener {
  private val titlefield = new JTextField("Intersecting "+from.name+" and "+to.name+" using Viewfinder")
  private val backButton = new JButton("< Back")

  private val cb = new JCheckBox("Use original Domain-/Codomain names")
  private val textfield1 = new JTextField("Use suffix instead:")
  private val suffixfield = new JTextField("suffix")
  private val textfield2 = new JTextField("Name of theory intersection:")
  private val intname = new JTextField("IntersectionName")
  private val goButton = new JButton("Intersect")
  private val glayout = new GroupLayout(this)
  private val TopPanel1 = glayout.createParallelGroup(GroupLayout.Alignment.CENTER)
  private val TopPanel2 = glayout.createSequentialGroup()

  backButton.addActionListener(this)
  titlefield.setEditable(false)
  titlefield.setFont(titlefield.getFont.deriveFont(Font.BOLD))
  titlefield.setHorizontalAlignment(SwingConstants.RIGHT)
  TopPanel1.addComponent(backButton)
  TopPanel1.addComponent(titlefield)
  TopPanel2.addComponent(backButton)
  TopPanel2.addComponent(titlefield)

  cb.setSelected(true)
  textfield1.setEditable(false)
  suffixfield.setEditable(false)
  cb.addActionListener(this)
  textfield2.setEditable(false)
  goButton.addActionListener(this)

  private val vert = glayout.createSequentialGroup()
  private val hor = glayout.createParallelGroup(GroupLayout.Alignment.LEADING)
  vert.addGroup(TopPanel1)
  hor.addGroup(TopPanel2)
  vert.addComponent(cb)
  hor.addComponent(cb)
  private val group1a=glayout.createParallelGroup(GroupLayout.Alignment.LEADING)
  private val group1b=glayout.createSequentialGroup()
  group1a.addComponent(textfield1)
  group1a.addComponent(suffixfield)
  group1b.addComponent(textfield1)
  group1b.addComponent(suffixfield)
  vert.addGroup(group1a)
  hor.addGroup(group1b)
  private val group2a=glayout.createParallelGroup(GroupLayout.Alignment.LEADING)
  private val group2b=glayout.createSequentialGroup()
  group2a.addComponent(textfield2)
  group2a.addComponent(intname)
  group2b.addComponent(textfield2)
  group2b.addComponent(intname)
  vert.addGroup(group2a)
  hor.addGroup(group2b)
  vert.addComponent(goButton)
  hor.addComponent(goButton)

  glayout.linkSize(BoxLayout.Y_AXIS,backButton,titlefield,cb,textfield1,suffixfield,textfield2,goButton,intname)

  glayout.setHorizontalGroup(hor)
  glayout.setVerticalGroup(vert)

  setLayout(glayout)
  target.viewpanel.resultArea.revalidate()
  target.repaint()

  def actionPerformed(ae:ActionEvent) = {
    if(ae.getSource==backButton) target.theorypanel.reinit
    if(ae.getSource==cb) if(cb.isSelected) suffixfield.setEditable(false) else suffixfield.setEditable(true)

    if(ae.getSource==goButton) {
      val list = target.intersecter(from,to,None,Some(LocalName(intname.getText)),
        if (cb.isSelected) Some("") else Some(suffixfield.getText))
      if (cb.isSelected) {
        target.theories = target.dropfromList(from,target.theories)
        target.theories = target.dropfromList(to,target.theories)
      }
      target.dumptoDocument(list)
    }

  }
}

case class DeletionArea(th:DeclaredTheory,target:RefactorPanel) extends JPanel with ActionListener {
  val glayout = new GroupLayout(this)
  val text1 = new JTextField("Remove (dependency closure of) selected components from theory "+th.name+": ")
  val name = new JTextField("TheoryName")
  val tp = new JPanel
  val doButton = new JButton("Go")
  val consts = th.getConstants collect {case c:FinalConstant => ConstantCb(c)}

  tp.setLayout(new BoxLayout(tp,BoxLayout.X_AXIS))
  text1.setEditable(false)
  tp.add(text1)
  tp.add(name)
  tp.add(doButton)
  doButton.addActionListener(this)

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
        Some(LocalName(name.getText)))))
}

case class PushoutArea(thA:DeclaredTheory,thB:DeclaredTheory,target:RefactorPanel)
  extends JPanel with ActionListener {

  val others = target.dropfromList(thA,target.dropfromList(thB,target.theories))
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
  }
}