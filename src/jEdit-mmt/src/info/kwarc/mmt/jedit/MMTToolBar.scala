package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import gui._

import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.ImageIcon

import org.gjt.sp.jedit._
import org.gjt.sp.jedit.msg._

/*
MMT toolbar with useful symbols and a clear button.
 */

class MMTToolBar(mmtp: MMTPlugin) extends JToolBar {
  private val controller = mmtp.controller

  private def view = jEdit.getActiveView()

  private def init {
    val insUS = Swing.Button("O", tooltip = "Inserts object delimiter (US)") {
       Inserter.insertUSorTab(view.getTextArea())
    }
    val insRS = Swing.Button("D", tooltip = "Inserts declaration delimiter (RS)") {
       Inserter.insertRSReturn(view.getTextArea())
    }
    val insGS = Swing.Button("M", tooltip = "Inserts module delimiter (GS)") {
       Inserter.insertGSReturn(view.getTextArea())
    }

    val buildButton = Swing.Button("Build", tooltip = "Builds current file") {
      mmtp.buildActions.buildCurrent(view)
    }
    val buildOpenButton = Swing.Button("Build all", tooltip = "Builds all open files") {
      mmtp.buildActions.buildOpen(view)
    }
    val stopButton = Swing.Button("Stop", tooltip = "Stop parsing of current file") {
      val buffer = view.getBuffer
      mmtp.progressTracker.get(buffer).foreach {
        _.kill
      }
    }
    val clrButton = Swing.Button("Clear", tooltip = "Clears MMT memory") {
      controller.clear
    }

    val clrIMG = (new ImageIcon(this.getClass().getResource("/images/clear_button.png"))).getImage()
    val clrIMGs = clrIMG.getScaledInstance(16, 16, java.awt.Image.SCALE_SMOOTH )
    clrButton.setIcon(new ImageIcon(clrIMGs))

    val clrFileButton = Swing.Button("Clear File", tooltip = "Clears current file from MMT memory") {
       val pd = sidekick.SideKickParsedData.getParsedData(view)
       pd.root.getUserObject match {
         case a: MMTElemAsset =>
            a.elem match {
               case d: documents.Document =>
                 controller.delete(d.path)
               case _ =>
            }
         case _ =>
       }
    }

    add(insUS)
    add(insRS)
    add(insGS)
    add(stopButton)
    add(buildButton)
    add(buildOpenButton)
    add(clrButton)
    add(clrFileButton)
  }

  init
}

