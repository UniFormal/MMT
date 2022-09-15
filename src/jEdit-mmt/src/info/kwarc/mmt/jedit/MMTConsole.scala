package info.kwarc.mmt.jedit
import org.gjt.sp.jedit._
import console._
import info.kwarc.mmt.api.frontend.ReportHandler
import info.kwarc.mmt.api.frontend.actions.Action

class OutputAsReport(output: Output) extends ReportHandler("jEdit console") {
   def apply(ind: Int, caller: => String, group : String, msgParts : List[String]): Unit = {
      msgParts.foreach {msg => output.print(null, indentString(ind) + group + ": " + msg)} // null for default color
   }
}

/** common code for Consoles, handles executing commands with different threads */
abstract class ThreadedConsole(s: String) extends console.Shell(s) {
   protected val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
   protected val controller = mmt.controller
   
   protected def logError(s: String) = controller.report("error", s)
   

   @volatile
   private var success : Option[Boolean] = Some(true)

   //If your shell executes commands in a separate thread, this method should stop the currently running thread, if any.
   override def stop (console: Console): Unit = {}

   //This method should block until the currently running command has completed, and return true if the command executed successfully, false otherwise. If no command is currently running, it should return the status of the most recently run command.
   override def waitFor(console: Console) : Boolean = synchronized {
     while (success.isEmpty) {
       this.wait()
     }
     success.get
   }
   
   /** the body of the execute function should be wrapped in this */
   protected def executionWrapper(code: => Boolean): Unit = {synchronized {
     success = None
     try {
       val r = code
       success = Some(r)
     } finally {
       this.notify()
     }
   }}
}

/** a MMT console for jEdit, it reads MMT actions and prints the log output */ 
class MMTConsole extends ThreadedConsole("mmt") {
   //This method is invoked by the console when the user selects the shell in question. It should print a short informational message, outlining the main capabilities of the shell.
   override def printInfoMessage (output: Output): Unit = {
      output.print(null, "This is the MMT Shell")
   }

   override def getCompletions(console: Console, command: String): Shell.CompletionInfo = new Shell.CompletionInfo {
      offset = 0
      completions = Action.completeAct(controller, command).toArray
   }

   def execute(console: Console, input: String, output: Output, error: Output, command: String) = executionWrapper {
      val han = new OutputAsReport(output)
      controller.report.addHandler(han)
      try {
         controller.handleLine(command)
         true
      } catch {
         case e: Exception =>
           controller.report("error", e.getMessage)
           false
      } finally {
         controller.report.removeHandler(han.id)
         output.commandDone()
      }
   }
}
