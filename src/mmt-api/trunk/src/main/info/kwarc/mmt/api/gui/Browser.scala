package info.kwarc.mmt.api.gui
import info.kwarc.mmt.api._
import backend._
import archives._

import javax.swing._
import java.awt.event._

import info.kwarc.mmt.api._
import frontend._

class Browser(wm: WindowManager) extends JFrame("MMT Browser") {
   private val controller = wm.controller
   private val textArea = new JTextArea()
   private val tabbed = new JTabbedPane()
   
   private val tree = new TreePane(controller)
   tabbed.addTab("Tree", null, tree, "the content tree")

   private val logpane = LogPane(controller)
   tabbed.addTab("Log (still buggy)", null, logpane, "configure logging options")

   private val bepane = new BackendPane(controller.backend)
   tabbed.addTab("Backend", null, bepane, "current backends")
   
   add(tabbed)
   addWindowListener(new WindowAdapter {
      override def windowClosed(e: WindowEvent) {wm.closeBrowser} 
   })
   setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
   setVisible(true)
   pack()
   
}