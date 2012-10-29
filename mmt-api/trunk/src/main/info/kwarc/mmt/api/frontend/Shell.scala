package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.utils._

/** Creates a Controller and provides a shell interface to it.
 * The command syntax is given by the Action class and the parser in its companion object.

 */
class Shell() extends {
   lazy val controller = new Controller
   private var shell = true

   def main(a : Array[String]) : Unit = {
      var args = a.toList
      args match {
         // send command to existing instance listening at a port, and quit
         case "-send" :: port :: rest =>
            val uri = (URI("http", "localhost:" + port) / ":admin") ? rest.mkString("", " ", "") 
            val ret = utils.xml.get(uri.toJava.toURL)
            println(ret.toString)
            sys.exit
         // execute command line arguments but do not read from standard input
         case "-noshell" :: rest =>
            shell = false
            args = rest
         // default behavior: execute command line arguments, read further commands from standard input
         case _ =>
      }
      val command = args.mkString("", " ", "")
      try {
         controller.handleLine(command)
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
         case e =>
           controller.cleanup
           throw e
      }
   }
}

/** A shell, the default way to run MMT as an application */
object Run extends Shell()