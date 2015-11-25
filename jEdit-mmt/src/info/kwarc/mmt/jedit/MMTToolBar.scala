package info.kwarc.mmt.jedit

import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.ImageIcon
import javax.swing.event._

import org.gjt.sp.jedit._
import org.gjt.sp.jedit.msg._

/*
MMT toolbar with useful symbols and a clear button.
 */

class MMTToolBar(mmtp: MMTPlugin) extends JToolBar {

  private val insUS = new JButton("Object Terminator (US)")
  private val insRS = new JButton("Declaration Terminator (RS)")
  private val insGS = new JButton("Module Terminator (GS)")

  val clrIMG = (new ImageIcon(
    this.getClass().getResource(
      "/images/clear_button.png"))).getImage()
  val clrIMGs = clrIMG.getScaledInstance(
    16, 16, java.awt.Image.SCALE_SMOOTH );
  private val clrBTN = new JButton("Clear",
    new ImageIcon(clrIMGs))

  private val controller = mmtp.controller

  val toolBar = new JToolBar("Symbol toolbar")
  toolBar.setFloatable(false)
  add(insUS)
  add(insRS)
  add(insGS)
  add(clrBTN)

  //Adding components
  insUS.setToolTipText("Inserts object delimiter")
  insRS.setToolTipText("Inserts declaration delimiter")
  insGS.setToolTipText("Inserts module delimiter")
  clrBTN.setToolTipText("Clears MMT memory")

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

  clrBTN.addActionListener(new ActionListener()
    {
      def actionPerformed(e: ActionEvent)
      {
        controller.clear
      }
    }
  )
}

