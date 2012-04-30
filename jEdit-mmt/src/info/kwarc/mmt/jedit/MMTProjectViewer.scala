package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import utils._
import utils.FileConversion._

import org.gjt.sp.jedit._
import projectviewer._
import errorlist._

class MMTProjectViewerAction(build: String) extends action.Action {
   protected val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
   protected val controller = mmt.controller
   private var currentProj : vpt.VPTProject = null
   private var currentDim : String = null
   private var currentPath : List[String] = Nil
   def getText : String = build
   override def prepareForNode(node: vpt.VPTNode) {
      if (! (node.isFile || node.isDirectory)) {
         cmItem.setVisible(false)
         return
      }
      val url = node.getNodePath
      currentProj = vpt.VPTNode.findProjectFor(node)
      val root = currentProj.getNodePath
      if (! url.startsWith(root)) {
         // should not happen in MMT projects
         cmItem.setVisible(false)
         return
      }
      val relPath: List[String] = File(url.substring(root.length + 1)).segments // node path relative to project root
      currentDim = relPath.head
      currentPath = relPath.tail
      if (currentDim == "source" && build == "compile") {
         cmItem.setVisible(true)
      } else {
         cmItem.setVisible(false)
      }
   }
   def actionPerformed(e: java.awt.event.ActionEvent) {
      // get the archive, also makes sure it exists
      val id = currentProj.getProperty("info.kwarc.mmt.jedit.MMTPlugin.project-options.id")
      mmt.compile(id, currentPath)
   }
}