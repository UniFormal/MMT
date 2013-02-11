package info.kwarc.mmt.api.gui
import info.kwarc.mmt.api._
import backend._
import archives._

import javax.swing._
import java.awt.event._

import info.kwarc.mmt.api._
import frontend._

class GUIFrame(wm: WindowManager) extends JFrame("MMT") {
   private val panel = new GUIPanel(wm.controller)
   add(panel)
   addWindowListener(new WindowAdapter {
      override def windowClosed(e: WindowEvent) {wm.closeBrowser} 
   })
   setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
   pack()
   setVisible(true)
}

class GUIPanel(controller: Controller) extends JPanel {
   private val textArea = new JTextArea()
   private val tabbed = new JTabbedPane()
   
   private val tree = new TreePane(controller)
   tabbed.addTab("Tree", null, tree, "the content tree")

   private val logpane = LogPane(controller)
   tabbed.addTab("Log (still buggy)", null, logpane, "configure logging options")

   private val bepane = new BackendPane(controller.backend)
   tabbed.addTab("Backend", null, bepane, "current backends")
   
   add(tabbed)
}