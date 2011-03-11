package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.utils._

class Shell(foundation : libraries.Foundation) extends {
      val filereport = new frontend.FileReport(new java.io.File("info.kwarc.mmt.api.log"))
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