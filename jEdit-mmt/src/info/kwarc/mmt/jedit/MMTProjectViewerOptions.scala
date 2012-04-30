package info.kwarc.mmt.jedit
import info.kwarc.mmt.api._

import org.gjt.sp.jedit._
import projectviewer._

class MMTProjectOptionPane(proj: vpt.VPTProject) extends AbstractOptionPane("info.kwarc.mmt.jedit.MMTPlugin.project-options")  {
   private var idComponent = new java.awt.TextField
   override def _init {
      val id = proj.getProperty("info.kwarc.mmt.jedit.MMTPlugin.project-options.id")
      idComponent.setText(id) 
      addComponent("project ID", idComponent)
   }
   override def _save {
      val id = idComponent.getText
      proj.setProperty("info.kwarc.mmt.jedit.MMTPlugin.project-options.id", id)
   }
}

class MMTProjectViewerOptions extends config.OptionsService { 
   def getOptionGroup(proj: vpt.VPTProject) = null
   def getOptionPane(proj: vpt.VPTProject) = new MMTProjectOptionPane(proj)
}