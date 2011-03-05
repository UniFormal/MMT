package info.kwarc.mmt.api.frontend
import jomdoc._

/** Instances of Report handle all output to the user */
abstract class Report {
   /** output is categorized, the elements of group determine which categories are considered
    *  the categories "user" (for user input) and "error" are output by default */
   val groups = scala.collection.mutable.Set[String]("user", "error")
   /** outputs a message */
   def apply(group : => String, msg : => String) : Unit =
	   if (groups.contains(group) || groups.contains("*")) log(group, msg)
   /** outputs an error (catgory "error") */
   def apply(e : Error) : Unit = apply("error", "\n" + e.msg)
   /** implementation specific logging hook */
   def log(group : => String, msg : => String)
}

/** outputs nothing, throws exceptions */
object NullReport extends Report {
   def log(group : => String, msg : => String) {}
   override def apply(e: Error) {throw e}
}

/** outputs to standard output */
class ConsoleReport extends Report {
   def log(group : => String, msg : => String) {
      println(group + ": " + msg) 
   }
}

/** outputs to a file */
class FileReport(val filename : java.io.File) extends Report with utils.FileWriter {
   def log(group : => String, msg : => String) {
         file.println(java.lang.System.currentTimeMillis().toString + "\t" + group + ": " + msg)
         file.flush
   }
}

/** joins a list of reports (an output category is reported if this report and the component report it) */
class MultipleReports(reports : Report*) extends Report {
	def log(group : => String, msg : => String) {
		reports.toList.map(_.apply(group, msg))
	}
}