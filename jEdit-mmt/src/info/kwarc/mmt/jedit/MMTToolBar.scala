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
    val insUS = Swing.Button("O") {
       Inserter.insertUSorTab(view.getTextArea())     
    }
    insUS.setToolTipText("Inserts object delimiter (US)")
  
    val insRS = Swing.Button("D") {
       Inserter.insertRSReturn(view.getTextArea())
    }
    insRS.setToolTipText("Inserts declaration delimiter (RS)")
    
    val insGS = Swing.Button("M") {
       Inserter.insertGSReturn(view.getTextArea())
    }
    insGS.setToolTipText("Inserts module delimiter (GS)")
  
    val clrButton = Swing.Button("Clear") {
      controller.clear
    }
    clrButton.setToolTipText("Clears MMT memory")
    val clrIMG = (new ImageIcon(this.getClass().getResource("/images/clear_button.png"))).getImage()
    val clrIMGs = clrIMG.getScaledInstance(16, 16, java.awt.Image.SCALE_SMOOTH )
    clrButton.setIcon(new ImageIcon(clrIMGs))
    
    val clrFileButton = Swing.Button("Clear File") {
       val pd = sidekick.SideKickParsedData.getParsedData(view)
       pd.root.getUserObject match {
         case a: MMTElemAsset =>
            a.elem match {
               case d: documents.Document =>
                 d.collectModules(controller) foreach {p => controller.delete(p)}
                 controller.delete(d.path)
               case _ =>
            }
         case _ =>
       }
    }
    clrFileButton.setToolTipText("Clears current file from MMT memory")

    val toolBar = new JToolBar("Symbol toolbar")
    //toolBar.setFloatable(false)
    add(insUS)
    add(insRS)
    add(insGS)
    add(clrButton)
    add(clrFileButton)
  }
  
  init
}

