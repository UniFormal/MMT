package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._

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
   /** implementation specific logging hook, returns the formatted message that was logged */
   def log(group : => String, msg : => String) {
	   val m = handle(group, msg)
	   if (rec) mem ::= m
	   m
	}
   def handle(group : => String, msg : => String) : String
   protected var ind : String = ""
   /** increase indentation */
   def indent {ind = ind + "  "}
   /** decrease indentation */
   def unindent {ind = ind.substring(2)}
   private var mem : List[String] = Nil
   private var rec = false
   def record {rec = true}
   def recall = mem
   def clear {mem = Nil; rec = false}
}

/** outputs nothing, throws exceptions */
object NullReport extends Report {
   def handle(group : => String, msg : => String) = ""
   override def apply(e: Error) {throw e}
}

/** outputs to standard output */
class ConsoleReport extends Report {
   def handle(group : => String, msg : => String) = {
      val m = ind + group + ": " + msg
      println(m)
      m
   }
}

/** outputs to a file */
class FileReport(val filename : java.io.File) extends Report with utils.FileWriter {
   private val df = new java.text.SimpleDateFormat("HH:mm:ss.S")
   def time = df.format(new java.util.Date())
   def handle(group : => String, msg : => String) = {
         val m = time + "\t" + ind + group + ": " + msg
         file.println(m)
         file.flush
         m
   }
}

/** joins a list of reports (an output category is reported if this report and the component report it) */
class MultipleReports(reports : Report*) extends Report {
	def handle(group : => String, msg : => String) = {
		reports.toList.map(_.apply(group, msg))
		"sent " + group + ": " + msg + " to multiple reports"
	}
	override def indent {reports.toList.map(_.indent)}
	override def unindent {reports.toList.map(_.unindent)}
}