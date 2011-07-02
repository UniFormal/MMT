package info.kwarc.mmt.jedit
import org.gjt.sp.jedit._
import info.kwarc.mmt.api._
import frontend._
import libraries._

/**
 * The main class of the MMTPlugin
 * after initialization, it creates a Controller and executes home/startup.mmt
 * logging information is sent to home/mmtplugin.log
 * the home directory is obtained from jEdit, e.g., settings/plugins/info.kwarc.mmt.jedit.MMTPlugin
 */
class MMTPlugin extends EditPlugin {
   // initialized only in start method
   private var controller : Controller = null
   // initialized only in start method, log messages are written to home/mmtplugin.log
   private var report : Report = null
   private def log(msg: String) {report("jEdit", msg)}
   /** called by jEdit when plugin is loaded */
   override def start() {
      val home = getPluginHome()
      home.mkdirs()
      val checker = new FoundChecker(DefaultFoundation)
      report = new FileReport(new java.io.File(home, "mmtplugin.log"))
      controller = new Controller(checker, report)
      controller.setHome(home)
      val startup = new java.io.File(home, "startup.mmt")
      if (startup.isFile())
         controller.handle(ExecFile(startup))
      else
         report("error", "could not find startup.mmt file")
   }
   /** called by jEdit when plugin is unloaded */
   override def stop() {
      controller.clear
   }
   
   def read(view : View) {
      view.splitHorizontally()
   }

}