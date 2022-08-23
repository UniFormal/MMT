package info.kwarc.mmt.jedit

import java.awt.GridLayout
import javax.swing.{JScrollPane, JTree, JPanel}
import info.kwarc.mmt.api.gui._
import org.gjt.sp.jedit.{jEdit, View}

class MMTTreeDockable(view: View, position: String) extends JPanel {
  val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
  val controller = mmt.controller
  val tree = new MMTTree(controller) {

  }
  def init: Unit = {
    setLayout(new GridLayout(1,1))
    tree.setRootVisible(false)
    val treepane = new JScrollPane(tree)
    add(treepane)
  }
  init
}
