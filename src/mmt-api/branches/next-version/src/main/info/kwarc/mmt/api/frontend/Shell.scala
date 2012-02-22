package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.utils._

/** Creates a Controller and provides a shell interface to it.
 * The command syntax is given by the Action class and the parser in its companion object.
 * @param f the checker
 */
class Shell(f : Report => libraries.Checker) extends {
   val filereport = new frontend.FileReport(new java.io.File("jomdoc.log"))
   val consreport = new ConsoleReport
   val report = new MultipleReports(filereport, consreport)       // reports to both console and file
   val checker = f(report)
   val controller = new Controller(checker,report)
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

/** A shell with the DefaultFoundation. The default entry point into the jar file. */
object Run extends Shell(r => new libraries.StructuralChecker(r))

/** A shell with the NullChecker */
object RunNull extends Shell(r => libraries.NullChecker)