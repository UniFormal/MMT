package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import utils._

/** used to add a new command line application to MMT
  *
  * ShellExtensions are looked for after all configurations are loaded.
  * These must contain the corresponding [[ExtensionConf]] entry so that MMT can find a ShellExtension.
  */
abstract class ShellExtension(command: String) extends FormatBasedExtension {
   def isApplicable(s: String) = s == command

  /** executed via the shell command "mmt :command ARGS"
    *
    * @return an error level to use as the exit code iff MMT should clean up and exit
    */
   def run(shell: Shell, args: List[String]): Option[Level.Level]
   /** return values for run to exit with error */
   protected val withError = Some(Level.Fatal)
   /** return value for run to exit successfully */
   protected val withSuccess = Some(Level.Info)
   /** return values for run to exit the main method but not call sys.exit
     * (which keeps any threads started by this extension alive)
     */
   protected val andKeepAlive = None
   /** help text for this command */
   def helpText: String
}

/** pass on arguments to an MMT server instance and terminate */
class ShellSendCommand extends ShellExtension("send") {
   def helpText = "mmt :send URL ARGS"
   def run(shell: Shell, args: List[String]): Option[Level.Level] = {
      if (args.isEmpty) {
         println(helpText)
         return withError
      }
      val target = args.head
      val server = if (target.forall(_.isDigit)) {
         // port number
         URI("http", "localhost:" + args.head)
      } else {
         URI(target)
      }
      val url = (server / ":action") ? args.tail.mkString(" ")
      try {
        println("sending: " + url.toString)
        val ret = utils.xml.get(url)
        println(ret.nonEmptyChildren.flatMap(d => d.nonEmptyChildren.map(_.toString)).mkString("\n"))
        withSuccess
      } catch {
        case e: Exception =>
          println("error while connecting to remote MMT: " + e.getMessage)
          withError
      }
   }
}

/** example shell extension that starts a static HTTP server serving the current directory */
class FileServerHere extends ShellExtension(":fileserver") {
  def helpText = "runs an HTTP server for the current directory"
  def run(shell: Shell, args: List[String]) = {
    val serverArgs = if (args.isEmpty) List("") else args // "" yields current directory
    controller.extman.addExtension(new web.FileServer, serverArgs)
    controller.handleLine("server on 8080")
    andKeepAlive // keep the server running
  }
}

