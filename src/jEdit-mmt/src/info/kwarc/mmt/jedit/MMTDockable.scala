package info.kwarc.mmt.jedit

import java.awt.GridLayout

import info.kwarc.mmt.api._
import gui._
import org.gjt.sp.jedit._
import javax.swing._

class MMTDockable(view: View, position: String) extends JPanel {
   val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
   val controller = mmt.controller
   def init: Unit = {
      setLayout(new GridLayout(1,1))
      val gui = new JScrollPane(new GUIPanel(controller))
      add(gui)
   }
   init
}
