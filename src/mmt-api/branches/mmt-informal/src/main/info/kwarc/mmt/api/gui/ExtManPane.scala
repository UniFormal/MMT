package info.kwarc.mmt.api.gui
import info.kwarc.mmt.api._
import frontend._

import javax.swing._
//import event._

class ExtManPane(extman: ExtensionManager) extends {val ta = new JTextArea} with JScrollPane(ta) {
   def set = {
      ta.setText(extman.stringDescription) 
   }
   set
}