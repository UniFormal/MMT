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
    * @return true iff MMT should clean up and exit
    */
   def run(shell: Shell, args: List[String]): Boolean
   /** help text for this command */
   def helpText: String
}

/** pass on arguments to an MMT server instance and terminate */
class ShellSendCommand extends ShellExtension("send") {
   def helpText = "mmt :send URL ARGS"
   def run(shell: Shell, args: List[String]): Boolean = {
      if (args.isEmpty) {
         println(helpText)
         return true
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
        val ret = utils.xml.get(url.toURL)
        println(ret.nonEmptyChildren.flatMap(d => d.nonEmptyChildren.map(_.toString)).mkString("\n"))
      } catch {
        case e: Exception =>
          println("error while connecting to remote MMT: " + e.getMessage)
      }
      true
   }
}