package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.utils._

/** Creates a Controller and provides a shell interface to it.
 * The command syntax is given by the Action class and the parser in its companion object.
 * @param foundation the foundation that is used for type checking
 */
class Shell(foundation : libraries.Foundation) extends {
      val filereport = new frontend.FileReport(new java.io.File("jomdoc.log"))
      val consreport = new ConsoleReport
      val r = new MultipleReports(filereport, consreport)
      val c = new libraries.FoundChecker(foundation)
} with Controller(c, r) {
   def main(args : Array[String]) : Unit = {
      val command = args.mkString("", " ", "")
      filereport.groups += "*"
      consreport.groups += "*"
      handleLine(command)
      val Input = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
      while (true) {
         val command = Input.readLine()
         handleLine(command)
      }
   }
}

/** A shell with the DefaultFoundation. The default entry point into the jar file. */
object Run extends Shell(libraries.DefaultFoundation)