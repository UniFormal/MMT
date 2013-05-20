package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import utils._

/** extended by all classes that use the logging aspect */
trait Logger {
   // fields are def's not val's to avoid surprises during instantiation/inheritance
   protected def report: Report
   def logPrefix: String
   protected def log(s : => String) = report(logPrefix, s)
   protected def logGroup[A](a: => A) : A = {
      report.indent
      try {a}
      finally {report.unindent}
   }
   protected def logError(s : => String) = report("error", "(" + logPrefix + ") " + s)
}

/** Instances of Report handle all output to the user */
class Report {
   /** output is categorized, the elements of group determine which categories are considered
    *  the categories "user" (for user input) and "error" are output by default */
   val groups = scala.collection.mutable.Set[String]("user", "error")
   /** logs a message if logging is switched on for the group */
   def apply(group : => String, msg : => String) : Unit =
	   if (groups.contains(group) || groups.contains("*")) log(group, msg)
   /** outputs an error (category "error") */
   def apply(e : Error) {
	   apply("error", e.msg)
	   apply("debug", "\n" + e.stackTrace)
	}
   /** definitely logs a message */
   private def log(group : => String, msg : => String) {
	   if (rec) mem ::= ind + group + ": " + msg  
	   handlers.foreach(_.apply(ind, group, msg))
	}

   private var handlers : List[ReportHandler] = Nil
   /** adds a ReportHandler */
   def addHandler(h: ReportHandler) {
      log("report", "logging to " + h)
      handlers ::= h
   }
   /** removes all ReportHandlers with a certain id */
   def removeHandler(id: String) {handlers = handlers.filter(_.id != id)}

   protected var ind : String = ""
   /** increase indentation */
   def indent {ind += "  "}
   /** decrease indentation */
   def unindent {ind = ind.substring(2)}

   //this should not be here, if needed, a special LogHandler should be added temporarily
   private var mem : List[String] = Nil
   private var rec = false
   def record {rec = true}
   def recall = mem
   def clear {mem = Nil; rec = false}
}

object Report {
   val groups = List("user", "error", "controller", "extman", "library", "archive", "backend")
}

abstract class ReportHandler(val id: String) {
   def apply(ind: String, group: String, msg: String)
   override def toString = id
}

/** outputs to standard output */
object ConsoleHandler extends ReportHandler("console") {
   def apply(ind: String, group: String, msg: String) = {
      val m = ind + group + ": " + msg
      println(m)
   }
}

/** outputs to a file */
class FileHandler(val filename : File) extends ReportHandler(filename.toString) {
   private val file = utils.File.Writer(filename)
   private val df = new java.text.SimpleDateFormat("HH:mm:ss.S")
   def time = df.format(new java.util.Date())
   def apply(ind: String, group : String, msg : String) = {
         val m = time + "\t" + ind + group + ": " + msg
         file.println(m)
         file.flush
   }
   override def toString = "file " + filename
}