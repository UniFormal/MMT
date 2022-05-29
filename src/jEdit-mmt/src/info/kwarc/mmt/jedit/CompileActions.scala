package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.source
import info.kwarc.mmt.api.utils._
import org.gjt.sp.jedit._

/** This class collects all build-related functionality to be triggered by GUI button, jedit actions, etc.. */
class BuildActions(mmtplugin: MMTPlugin) {
   private val errorSource = mmtplugin.errorSource
   private val controller = mmtplugin.controller
   private def log(msg: String) {controller.report("jedit-compile", msg)}

   /** builds a file or directory */
   def build(f: String) {
      val file = File(f)
      val errorCont = new ErrorListForwarder(errorSource, controller, file)
      errorSource.removeMMTFileErrors(file)
      if (file.isFile) {
         log("build file " + file)
         try {
            controller.build(file)(errorCont)
         } catch {
           case e: Error => errorCont(e)
         }
      }
   }

   //TODO saving may trigger sidekick-parsing in which case parsing and building happen at the same time and likely confuse each other
   private def saveAndBuild(view: View, buffer: Buffer) {
      if (buffer.isDirty) {
         buffer.save(view, null)
         io.VFSManager.waitForRequests // wait until buffer is saved
      }
      build(buffer.getPath)
   }

   /** saves and builds the current file of the current view */
   def buildCurrent(view: View) {
      val buffer = view.getBuffer
      saveAndBuild(view, buffer)
   }
   /** saves and builds the open files in the current view */
   def buildOpen(view: View) {
      val buffers = view.getBuffers
      buffers.foreach {b => saveAndBuild(view, b)}
   }
   /** saves and build the file/folder currently selected in the the file browser */
   def buildSelected(view: View, brw: browser.VFSBrowser) {
      val files = brw.getSelectedFiles
      files foreach {vfsfile =>
         val file = vfsfile.getPath
         // save if the file is open
         if (vfsfile.getType != io.VFSFile.DIRECTORY) {
            val buffer = jEdit.getBuffer(file)
            if (buffer != null) {
               buffer.save(view, null)
               io.VFSManager.waitForRequests // wait until buffer is saved
            }
         }
         build(file)
      }
   }
}
