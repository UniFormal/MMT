package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import gui.Swing
import info.kwarc.mmt.api.refactoring.{NotDone, ViewFinder}
import javax.swing.JMenu
import org.gjt.sp.jedit._
import textarea._

import scala.concurrent.Future

object EditActions {
  /** replaces a part of the text, first and last are inclusive */
  def overwriteText(textArea: TextArea, first: Int, last: Int, withText: String) {
    textArea.setSelection(new Selection.Range(first,last+1))
    textArea.setSelectedText(withText) 
  }
}

/** collects functionality that changes the text in the buffer
 *  these functions should be bound to actions in actions.xml, which can then be bound to keystrokes etc.
 */ 
class EditActions(mmtplugin: MMTPlugin) {
  def introduceHole(view: View) {
    val editPane = view.getEditPane
    val textArea = editPane.getTextArea
    val buffer = editPane.getBuffer
    val offset = textArea.getCaretPosition
    if (buffer.getText(offset, 1) != "_")
      return
    val as = MMTSideKick.getAssetAtOffset(view, offset).getOrElse(return)
    as match {
      case oa: MMTObjAsset =>
        val tp = oa.inferType.getOrElse(return)
        val tm = checking.Hole(tp)
        val tmS = mmtplugin.controller.presenter.asString(tm)
        EditActions.overwriteText(textArea, offset, offset, tmS)
      case _ =>
    }
  }

  /**
    * shows the normalization of the current asset (selection or cursor)
    *
    * @param replace if true, replace selected asset; otherwise, show popup
    */
  def showNormalization(view: View, replace: Boolean) {
    val (as, selected) = MMTSideKick.getCurrentAsset(view).getOrElse(return)
    as match {
      case oa: MMTObjAsset =>
        val obj = oa.obj
        val objS = mmtplugin.controller.simplifier(obj, oa.context, true)
        val str = mmtplugin.asString(objS)
        val textArea = view.getTextArea
        if (selected && replace) {
          textArea.setSelectedText(str)
        } else {
          if (selected) {
            textArea.setSelection(oa.toSelection)
          }
          new TextareaPopup(textArea, oa.region.start.offset, str) // to be closed by user clicking the button
        }
      case _ =>
    }
  }

  private def findViewTo(view: View, to: String): Unit = {
    val em = mmtplugin.controller.extman
    val (as, selected) = MMTSideKick.getCurrentAsset(view).getOrElse(return)
    as match {
      case oa: MMTObjAsset =>
        val str: String = oa.getScope match {
          case Some(mp: MPath) =>
            val vfO = em.get(classOf[ViewFinder]).headOption
            vfO match {
              case Some(vf) =>
                try {
                  val results = vf.find(mp, to).reverse.map(v => v.toString) //.mkString("\n\n")
                  mp.toString + " --> " + to + "\n" + {
                    if (results.isEmpty) "No results found :("
                    else results.length + " Results found. Best:\n\n" + results.head
                  }
                } catch {
                  case NotDone => NotDone.toString // Should not happen
                }
              case _ =>
                ??? // should not happen
            }
          case _ => "Not in theory?"
        }
        new TextareaPopup(view.getTextArea, oa.region.start.offset, str)
      case _ =>
    }
  }

  def viewfindermenu(view: View) = {
    mmtplugin.controller.extman.get(classOf[ViewFinder]).headOption match {
      case Some(vf) =>
        val menu = new JMenu("Find Views to...")
        vf.targets.sortWith(_ < _) foreach { s =>
          menu.add(ContextMenu.item(s, findViewTo(view, s)))
        }
        if (!vf.isInitialized) {
          menu.add("(More still loading...)")
        }
        menu
      case None =>
        ContextMenu.item("Start Viewfinder", {
          val n = new ViewFinder
          mmtplugin.controller.handleLine("log+ " + n.logPrefix)
          mmtplugin.controller.extman.addExtension(n)
        })
    }
  }
}


import javax.swing._
import java.awt.{Color,FlowLayout}
/** pops up a small text window on top of a TextArea */
class TextareaPopup(textArea: TextArea, offset: Int, text: String) extends JFrame() {
   private val contentArea = new JTextArea()
   private val closeButton = Swing.Button("X"){dispose}
   def set(content: String) {
      contentArea.setText(content)
      pack()
   }
   def setLocationRelativeToOffset(deltax: Int, deltay: Int) {
     val p = textArea.offsetToXY(offset)
     SwingUtilities.convertPointToScreen(p, textArea.getPainter)
     setLocation(p.getX.toInt + deltax, p.getY.toInt + deltay)
   }
   contentArea.setEditable(false)
   contentArea.setBorder(new border.LineBorder(Color.BLACK, 1, false))
   closeButton.setSize(10,10)
   setLayout(new FlowLayout())
   add(contentArea)
   add(closeButton)
   setAlwaysOnTop(true)
   setUndecorated(true)
   set(text)
   setLocationRelativeToOffset(0,30)
   setVisible(true)
}
