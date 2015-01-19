package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import backend._
import utils._
import FileConversion._

import javax.swing.UIManager

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
   
   def main(a : Array[String]) : Unit = {
      var args = a.toList
      var shell = true
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
            } catch {case e:Exception =>
               println("error while connecting to remote MMT: " + e.getMessage)
            }
            sys.exit
         // execute command line arguments and drop to shell
         case "-shell" :: rest =>
            args = rest
            shell = true
         // execute command line arguments and terminate
         // if the server is started, we only terminate when the server thread does 
         case "-noshell" :: rest =>
            args = rest
            shell = false
         // '-file N' is short for '-shell file N'
         case "-file" :: name :: Nil =>
            args = List("file", name)  
            shell = true
         // default behavior: execute command line arguments and exit; shell if no arguments
         case _ =>
            shell = args.isEmpty
      }
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName()) // standard GUI configuration
      
      try {
         // execute startup arguments
         val folder = File(getClass.getProtectionDomain.getCodeSource.getLocation.getPath).up
         val startup = folder / "startup.msl"
         if (startup.exists)
            controller.handle(ExecFile(startup, None))
         // execute command line arguments
         val commands = args.toList.mkString(" ").split(" ; ")
         commands foreach controller.handleLine
         // wait for interactive commands
         if (shell)
            println(shelltitle)
         val Input = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
         while (shell) {
            val command = Input.readLine()
            controller.handleLine(command)
         }
      } catch {
         case e: Error =>
           controller.report(e)
           controller.cleanup
           throw e
         case e : Throwable =>
           controller.cleanup
           throw e
      }
   }
}

/** A shell, the default way to run MMT as an application */
object Run extends Shell()