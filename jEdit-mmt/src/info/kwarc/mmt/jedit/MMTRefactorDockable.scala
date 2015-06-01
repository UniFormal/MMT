package info.kwarc.mmt.jedit

import info.kwarc.mmt.api.refactoring.RefactorPanel
import org.gjt.sp.jedit.{jEdit, View}


class MMTRefactorDockable(jview: View, position: String) extends RefactorPanel(
  jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin].controller,
  s => jview.getTextArea.setText(jview.getTextArea.getText+"\n\n"+s)
) {
  val mmt: MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
}
