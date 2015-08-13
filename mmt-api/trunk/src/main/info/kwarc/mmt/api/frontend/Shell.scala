package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._

/** Creates a Controller and provides a shell interface to it.
  * The command syntax is given by the Action class and the parser in its companion object.

  */
class Shell extends {
  lazy val controller = new Controller

  private val helptext = """
usage:
  mmt -help
    display this help
  mmt -send PORT COMMANDS
    execute COMMANDS by mmt instance listening at PORT
  mmt [-shell | -noshell] [COMMANDS]
    execute COMMANDS and possibly take further commands on the MMT shell.
    (If neither flag is provided, a shell is displayed iff COMMANDS is empty.)
    Even if there is no shell, mmt only exits when all secondary threads (e.g., GUI, HTTP server) have terminated.
                         """

  private val shelltitle = """
This is the MMT shell.
See https://svn.kwarc.info/repos/MMT/doc/api/index.html#info.kwarc.mmt.api.frontend.Action for the available commands.

                           """

  def main(a: Array[String]): Unit = {
    // command to process initially
    var args = a.toList
    // respond with shell after processing initial command
    var shell = true
    // release all resources and terminate after processing initial command and shell
    var terminate = true
    def processArgs {
       args match {
         case "-help" :: _ =>
           println(helptext)
           sys.exit
         // send command to existing instance listening at a port, and quit
         case "-send" :: port :: rest =>
           val uri = (URI("http", "localhost:" + port) / ":admin") ? rest.mkString("", " ", "")
           try {
             println("sending: " + uri.toString)
             val ret = utils.xml.get(uri.toJava.toURL)
             println(ret.toString)
           } catch {
             case e: Exception =>
               println("error while connecting to remote MMT: " + e.getMessage)
           }
           sys.exit
         // execute command line arguments and drop to shell
         case "-shell" :: rest =>
           args = rest
           processArgs
         // execute command line arguments and terminate
         // if the server is started, we only terminate when the server thread does
         case "-noshell" :: rest =>
           args = rest
           shell = false
           terminate = false
           processArgs
         // '-file N' is short for '-shell file N'
         case "-file" :: name :: Nil =>
           args = List("file", name)
         case "-mbt" :: name :: Nil =>
           args = List("mbt", name)
           shell = false
         // default behavior: execute command line arguments and exit; shell if no arguments
         case _ =>
           shell = args.isEmpty
       }
    }
    processArgs
    try {
      // execute startup arguments
      val startup = MMTSystem.rootFolder / "startup.msl"
      //println("trying to run " + startup)
      if (startup.exists) {
        controller.handle(ExecFile(startup, None))
      }
      // execute command line arguments
      val commands = args.mkString(" ").split(" ; ")
      commands foreach controller.handleLine
      // wait for interactive commands
      if (shell)
        println(shelltitle)
      val Input = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
      while (shell) {
        val command = Input.readLine()
        if (command != null)
           controller.handleLine(command)
      }
      if (terminate) controller.cleanup
    } catch {
      case e: Error =>
        controller.report(e)
        controller.cleanup
        throw e
      case e: Exception =>
        controller.cleanup
        throw e
    }
  }
}

/** A shell, the default way to run MMT as an application */
object Run extends Shell()
