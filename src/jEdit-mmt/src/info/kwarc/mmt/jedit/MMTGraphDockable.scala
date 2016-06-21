package info.kwarc.mmt.jedit

import javax.swing._
import org.gjt.sp.jedit.{jEdit, View}

/**
 * Created by raupi on 28.05.15.
 */
class MMTGraphDockable(jview: View, position: String) extends JPanel {
  val mmt: MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
  val graphviz = "/usr/bin/dot" //TODO change that!
  setLayout(new BoxLayout(this,BoxLayout.X_AXIS))
  //add(new GraphPanel(mmt.controller,graphviz))
}
