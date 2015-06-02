package info.kwarc.mmt.api.gui

// TODO This doesn't actually work yet!
import java.awt.BorderLayout
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{DeclaredView, DeclaredTheory}
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils.{ShellCommand, File}

/**
 * Created by raupi on 29.05.15.
 */
class GraphPanel(ctrl:Controller) extends JPanel with ActionListener {
  val controller = ctrl
  val theories:List[TheoryBox] = (controller.memory.content.getModules collect { case t: DeclaredTheory =>new TheoryBox(t) }).toList
  val views:List[ViewBox] = (controller.memory.content.getModules collect { case v: DeclaredView => new ViewBox(v) }).toList
  val goButton = new JButton("Update")
  val graphviz = "/usr/bin/dot" // TODO change that!
  val dotFile = File(System.getProperty("java.io.tmpdir"))/"MMTGraph.dot"

  val sidebar=new JPanel
  //val graphpanel=new FXPanel
  val graphpanel=new JPanel

  goButton.addActionListener(this)
  for (t <- theories) sidebar.add(t)
  for (v <- views) sidebar.add(v)
  graphpanel.setLayout(new BoxLayout(graphpanel, BoxLayout.Y_AXIS))
  sidebar.setLayout(new BoxLayout(sidebar, BoxLayout.Y_AXIS))
  setLayout(new BorderLayout)
  add(goButton, BorderLayout.PAGE_START)
  add(graphpanel,BorderLayout.CENTER)
  add(new JScrollPane(sidebar),BorderLayout.WEST)

  reinit

  def reinit = {
    val texter = new JTextArea("Loading Graph...")
    graphpanel.add(texter)
    graphpanel.revalidate()
    (new reinitor(texter)).execute()
  }

  class reinitor(texter: JTextArea) extends javax.swing.SwingWorker[Option[String],Void] {
    def doInBackground = {
      try {
        implicit def add(rf:RelationalElement):Unit = controller.depstore.+=(rf)
        for(a <- theories) MMTExtractor(a.theory)
        for(a <- views) MMTExtractor(a.view)
        val graph = new TheoryGraph(controller.depstore)
        val export = new GraphExporter(
          theories.filter(_.isSelected).map(_.theory.path),
          views.filter(_.isSelected).map(_.view.path),
          graph
        )

        // TODO Something screws exporter (probably invalid declarations in theories!)

        export.exportDot(dotFile)
        ShellCommand.run(graphviz, "-Tsvg", "-o" + dotFile.setExtension("svg"), dotFile.toString)
        //graphpanel.loadBody(res.get)

      } catch {case e:Exception => Some(e.getLocalizedMessage)}

    }

    override def done = {
      texter.append(get.getOrElse("No return?"))
    }
  }

  def actionPerformed(ae: ActionEvent) = {

  }

}

class TheoryBox(th:DeclaredTheory) extends JCheckBox(th.path.^!.last+"?"+th.path.name) {
  val theory : DeclaredTheory = th
  setSelected(true)
}

class ViewBox(v:DeclaredView) extends JCheckBox(v.name+": "+v.from.toMPath.name +" -> "+v.to.toMPath.name) {
  val view : DeclaredView = v
  setSelected(true)
}