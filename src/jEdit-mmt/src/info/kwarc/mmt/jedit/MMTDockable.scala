package info.kwarc.mmt.jedit
import info.kwarc.mmt.api._
import gui._
import org.gjt.sp.jedit._
import javax.swing._

class MMTDockable(view: View, position: String) extends JPanel {
   val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
   val controller = mmt.controller
   def init {
      val gui = new GUIPanel(controller)
      add(gui)
   }
}
