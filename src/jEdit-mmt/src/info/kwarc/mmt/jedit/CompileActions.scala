package info.kwarc.mmt.jedit

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives.source
import info.kwarc.mmt.api.utils._
import org.gjt.sp.jedit._

/** This class is factored out from MMTPlugin to structure the code better.
 * It collects all compilation-related functionality. */
class CompileActions(mmtplugin: MMTPlugin) {
   private val errorSource = mmtplugin.errorSource
   private val controller = mmtplugin.controller
   private def log(msg: String) {controller.report("jedit-compile", msg)}
   
   /** compiles a buffer or directory */
   def compile(f: String) {
      val file = File(f)
      implicit val errorCont = new ErrorListForwarder(errorSource, controller, file)
      errorCont.reset
      if (file.isFile) {
         log("compiling buffer " + file)
         try {
            controller.build(file)
         } catch {case e: Error => errorCont(e)}
      }
   }
   def compileCurrent(view: View) {
      val buffer = view.getBuffer
      buffer.save(view, null)
      io.VFSManager.waitForRequests // wait until buffer is saved
      compile(buffer.getPath)
   }
   def compileSelected(view: View, brw: browser.VFSBrowser) {
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
         compile(file)
      }
   }
}