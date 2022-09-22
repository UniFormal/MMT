package info.kwarc.mmt.api.gui
import info.kwarc.mmt.api._
import backend._
import archives._
import frontend._

import javax.swing._
import java.awt.event._
import java.awt.Font
import java.awt.BorderLayout

class GUIFrame(wm: WindowManager) extends JFrame("MMT") {
   private val panel = new GUIPanel(wm.controller)
   add(panel)
   addWindowListener(new WindowAdapter {
      override def windowClosed(e: WindowEvent): Unit = {wm.closeBrowser}
   })
   setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
   pack()
   setVisible(true)
}

class GUIPanel(controller: Controller) extends JPanel {
   setLayout(new BorderLayout())
   setFont(new Font(Font.MONOSPACED, Font.PLAIN, 10))
   private val tabbed = new JTabbedPane()

   private val tree = new TreePane(controller)
   tabbed.addTab("Tree", null, tree, "the content tree")

   private val searchpane = new SearchPane(controller)
   tabbed.addTab("Search", null, searchpane, "MathWebSearch")

   //private val logpane = LogPane(controller)
   //tabbed.addTab("Log (still buggy)", null, logpane, "configure logging options")

   private val bepane = new BackendPane(controller.backend)
   tabbed.addTab("Backend", null, bepane, "current backends")

   private val empane = new ExtManPane(controller.extman)
   tabbed.addTab("Extensions", null, empane, "registered extensions")

   private val configpane = new ConfigPane(controller.getConfig)
   tabbed.addTab("Configuration", null, configpane, "loaded configuration entries")

   add(tabbed, BorderLayout.CENTER)
}
