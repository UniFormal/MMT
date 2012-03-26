package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.utils._

/** Creates a Controller and provides a shell interface to it.
 * The command syntax is given by the Action class and the parser in its companion object.
 * @param foundation the foundation that is used for type checking
 */
class Shell() extends {
   val controller = new Controller
   def main(args : Array[String]) : Unit = {
      controller.setConsoleReport
      controller.setFileReport(File("jomdoc.log"))
      val command = args.mkString("", " ", "")
      try {
         controller.handleLine(command)
         val Input = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
         while (true) {
            val command = Input.readLine()
            controller.handleLine(command)
         }
      } catch {
         case e => controller.cleanup; throw e
      }
   }
}

/** A shell, the default way to run MMT as an application */
object Run extends Shell()

/** same as Run, obsolete */
object RunNull extends Shell()