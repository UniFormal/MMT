package info.kwarc.mmt.api.gui
import info.kwarc.mmt.api._
import frontend._

import javax.swing._
//import event._

private[gui] abstract class ConfigPaneEI(val ta: JTextArea) extends JScrollPane(ta)
class ConfigPane(config: MMTConfig) extends ConfigPaneEI(new JTextArea) {
   def set = {
      ta.setText(config.toString)
   }
   set
}
