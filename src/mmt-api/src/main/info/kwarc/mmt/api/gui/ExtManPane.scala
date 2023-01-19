package info.kwarc.mmt.api.gui
import info.kwarc.mmt.api._
import frontend._

import javax.swing._
//import event._

private[gui] abstract class ExtManPaneEI(val ta: JTextArea) extends JScrollPane(ta)
class ExtManPane(extman: ExtensionManager) extends ExtManPaneEI(new JTextArea) {
   def set = {
      ta.setText(extman.stringDescription)
   }
   set
}
