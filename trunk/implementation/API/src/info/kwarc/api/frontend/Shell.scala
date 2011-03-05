package jomdoc.frontend
import jomdoc._
import jomdoc.backend._
import jomdoc.utils._

class Shell(foundation : libraries.Foundation) extends {
      val filereport = new frontend.FileReport(new java.io.File("jomdoc.log"))
      val consreport = new ConsoleReport
      val report = new MultipleReports(filereport, consreport)
      val checker = new libraries.FoundChecker(foundation)
} with Controller(checker, report) {
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

object Run extends Shell(libraries.DefaultFoundation)