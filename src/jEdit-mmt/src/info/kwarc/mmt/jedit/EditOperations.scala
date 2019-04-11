package info.kwarc.mmt.jedit

import java.awt.Dimension

import info.kwarc.mmt.api._
import gui.{MMTObjAsset, Swing}
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects.{OMID, OMS}
import info.kwarc.mmt.api.refactoring.{NotDone, ViewFinder}
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.uom.SimplificationUnit
import javax.swing.JMenu
import org.gjt.sp.jedit.{View => JEditView, _}
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
  def introduceHole(view: JEditView) {
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
  def showNormalization(view: JEditView, replace: Boolean) {
    val (as, selected) = MMTSideKick.getCurrentAsset(view).getOrElse(return)
    as match {
      case oa: JObjAsset =>
        val obj = oa.obj
        val objS = mmtplugin.controller.simplifier(obj, SimplificationUnit(oa.context, true, true))
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

  private def findViewTo(view: JEditView, to: String): Unit = {
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
                  val results = vf.find(mp, to).map(presentView) //.mkString("\n\n")
                  "From: " + mp.toString + "\nTo: " + to + "\n" + {
                    if (results.isEmpty) "No results found :("
                    else results.length + " Results found:\n\n" + results.mkString("\n\n")
                  }
                } catch {
                  case NotDone => NotDone.toString // Should not happen
                }
              case _ =>
                ??? // should not happen
            }
          case _ => "Not in theory?"
        }
        new ScrollTextareaPopup(view.getTextArea, oa.region.start.offset, str)
      case _ =>
    }
  }

  private def presentView(v : View) : String = v.name.toString + " : " +
    v.from.toMPath.module.name.toString + " -> " + v.to.toMPath.module.name.toString + "\n" + (v.getIncludes.map { i =>
      "  include " + i._2.toStr(true)
    } ::: v.getDeclarations.collect {
      case c : FinalConstant if c.df.isDefined =>
        "  " + c.name + " = " + (c.df match {
          case Some(OMS(p)) => p.module.name + "?" + p.name
          case Some(t) => t.toStr(true)
        })
    }).mkString("\n")

  def viewfindermenu(view: JEditView) = {
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

class ScrollTextareaPopup(textArea: TextArea, offset: Int, text: String) extends JFrame() {
  private val contentArea = new JTextArea()
  // private val panel = new JPanel()
  private val scroll = new JScrollPane(contentArea,22,32)
  private val closeButton = Swing.Button("X"){dispose}
  private def set(content: String) {
    contentArea.setText(content)
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
  add(scroll)
  // panel.add(contentArea)
  // add(contentArea)
  add(closeButton)
  setAlwaysOnTop(true)
  setUndecorated(true)
  set(text)
  scroll.setPreferredSize(new Dimension(1000,300))
  pack()
  setLocationRelativeToOffset(0,30)
  setVisible(true)
}
