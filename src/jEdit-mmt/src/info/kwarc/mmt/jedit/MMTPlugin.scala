package info.kwarc.mmt.jedit
import org.gjt.sp.jedit._

import info.kwarc.mmt.api._
import frontend._
import libraries._
import utils.FileConversion._

/**
 * The main class of the MMTPlugin
 * after initialization, it creates a Controller and executes home/startup.mmt
 * logging information is sent to home/mmtplugin.log
 * the home directory is obtained from jEdit, e.g., settings/plugins/info.kwarc.mmt.jedit.MMTPlugin
 */
class MMTPlugin extends EditPlugin {
   val controller : Controller = new Controller
   private def log(msg: String) {controller.report("jEdit", msg)}
   /** called by jEdit when plugin is loaded */
   override def start() {
      val home = getPluginHome()
      home.mkdirs()
      controller.setFileReport(home / "mmtplugin.log")
      controller.setHome(home)
      controller.setCheckStructural
      val startup = new java.io.File(home, "startup.mmt")
      if (startup.isFile())
         controller.handle(ExecFile(startup))
      else
         controller.report("error", "could not find startup.mmt file")
   }
   /** called by jEdit when plugin is unloaded */
   override def stop() {
      controller.cleanup
   }
   
   def read(view : View) {
      val src = scala.io.Source.fromString(view.getTextArea.getText)
      controller.clear
      controller.textReader.read(src)
   }

}