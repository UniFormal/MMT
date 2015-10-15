package info.kwarc.mmt.jedit

import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.event._

import org.gjt.sp.jedit.GUIUtilities
import org.gjt.sp.jedit.jEdit
import org.gjt.sp.jedit.View
import org.gjt.sp.jedit.msg._

/*
MMT toolbar with useful symbols. This is a temporary
solution to the problem regarding the terminating character.
 */

class MMTToolBar(mmtp: MMTPlugin) extends JToolBar {

  private val insUS = new JButton("Object Terminator (US)")
  private val insRS = new JButton("Declaration Terminator (RS)")
  private val insGS = new JButton("Module Terminator (GS)")

  val toolBar = new JToolBar("Symbol toolbar")
  toolBar.setFloatable(false)
  add(insUS)
  add(insRS)
  add(insGS)

  //Adding components
  insUS.setToolTipText("Inserts delimiter 1")

  insUS.addActionListener(new ActionListener()
    {
      def actionPerformed(e: ActionEvent)
      {
        val view = jEdit.getActiveView()
        Inserter.insertUSorTab(view.getTextArea())
      }
    }
  )

  insRS.addActionListener(new ActionListener()
    {
      def actionPerformed(e: ActionEvent)
      {
        val view = jEdit.getActiveView()
        Inserter.insertRSReturn(view.getTextArea())
      }
    }

  )

  insGS.addActionListener(new ActionListener()
    {
      def actionPerformed(e: ActionEvent)
      {
        val view = jEdit.getActiveView()
        Inserter.insertGSReturn(view.getTextArea())
      }
    }
  )
}

