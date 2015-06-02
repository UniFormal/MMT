package info.kwarc.mmt.jedit

import java.awt.BorderLayout
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._

import info.kwarc.mmt.api.gui.{GraphPanel}
import info.kwarc.mmt.api.modules.{DeclaredView, DeclaredTheory}
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.utils.{ShellCommand, File}
import org.gjt.sp.jedit.{jEdit, View}

/**
 * Created by raupi on 28.05.15.
 */
class MMTGraphDockable(jview: View, position: String) extends GraphPanel(
  jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin].controller
) {
  val mmt: MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
}