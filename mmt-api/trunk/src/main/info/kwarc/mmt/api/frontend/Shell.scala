package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.utils._

/** Creates a Controller and provides a shell interface to it.
 * The command syntax is given by the Action class and the parser in its companion object.
 * @param foundation the foundation that is used for type checking
 */
class Shell() extends {
   val filereport = new frontend.FileReport(new java.io.File("jomdoc.log"))
   val consreport = new ConsoleReport
   val report = new MultipleReports(filereport, consreport)
   val controller = new Controller(report)
   def main(args : Array[String]) : Unit = {
      val command = args.mkString("", " ", "")
      filereport.groups += "*"
      consreport.groups += "*"
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