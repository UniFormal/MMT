package info.kwarc.mmt.jedit

import java.awt.event.{ActionEvent, ActionListener, MouseEvent}

import javax.swing.{JMenu, JMenuItem}
import org.gjt.sp.jedit.gui.DynamicContextMenuService
import org.gjt.sp.jedit.jEdit
import org.gjt.sp.jedit.textarea.JEditTextArea

class ContextMenu extends DynamicContextMenuService {
  lazy val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
  lazy val controller = mmt.controller

  def createMenu(jEditTextArea: JEditTextArea, mouseEvent: MouseEvent): Array[JMenuItem] = {
    val view = jEditTextArea.getView

    val menu = new JMenu("MMT")
    menu.add(ContextMenu.item("Show Full Normalization",mmt.editActions.showNormalization(view, false, true)))
    menu.add(ContextMenu.item("Normalize",mmt.editActions.showNormalization(view, true, true)))
    menu.add(ContextMenu.item("Show One-Step Normalization",mmt.editActions.showNormalization(view, false, false)))
    menu.add(ContextMenu.item("Normalize One Step",mmt.editActions.showNormalization(view, true, false)))
    menu.add(ContextMenu.item("Introduce Hole",mmt.editActions.introduceHole(view)))
    menu.add(mmt.editActions.viewfindermenu(view))
    Array(menu)
  }

}

object ContextMenu {
  def item(label : String, action : => Unit) = {
    val it = new JMenuItem(label)
    it.addActionListener(new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = action
    })
    it
  }
}