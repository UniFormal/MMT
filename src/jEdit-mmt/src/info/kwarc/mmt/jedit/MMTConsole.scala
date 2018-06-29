package info.kwarc.mmt.jedit
import org.gjt.sp.jedit._
import console._
import info.kwarc.mmt.api.frontend.ReportHandler
import info.kwarc.mmt.api.frontend.actions.Action

class OutputAsReport(output: Output) extends ReportHandler("jEdit console") {
   def apply(ind: Int, caller: => String, group : String, msgParts : List[String]) {
      msgParts.foreach {msg => output.print(null, indentString(ind) + group + ": " + msg)} // null for default color
   }
}

class MMTConsole extends console.Shell("mmt") {
   val mmt : MMTPlugin = jEdit.getPlugin("info.kwarc.mmt.jedit.MMTPlugin", true).asInstanceOf[MMTPlugin]
   val controller = mmt.controller
   //This method is invoked by the console when the user selects the shell in question. It should print a short informational message, outlining the main capabilities of the shell.
   override def printInfoMessage (output: Output) {
      output.print(null, "This is the MMT Shell")
   }
   //If your shell executes commands in a separate thread, this method should stop the currently running thread, if any.
   override def stop (console: Console) {}
   private var success : Option[Boolean] = Some(true)

   //This method should block until the currently running command has completed, and return true if the command executed successfully, false otherwise. If no command is currently running, it should return the status of the most recently run command.
   override def waitFor(console: Console) : Boolean = true

   override def getCompletions(console: Console, command: String): Shell.CompletionInfo = new Shell.CompletionInfo {
      offset = 0
      completions = Action.completeAct(controller, command).toArray
   }

   def execute(console: Console, input: String, output: Output, error: Output, command: String) {
      val han = new OutputAsReport(output)
      controller.report.addHandler(han)
      success = None
      try {
         controller.handleLine(command)
      } catch {
         case e: Exception => controller.report("error", e.getMessage)
      } finally {
         controller.report.removeHandler(han.id)
         success = Some(true)
         output.commandDone()
      }
   }
}
