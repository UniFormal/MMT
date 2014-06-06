package info.kwarc.mmt.api.frontend
import info.kwarc.mmt.api._
import utils._

/** extended by all classes that use the logging aspect */
trait Logger {
   // fields are def's not val's to avoid surprises during instantiation/inheritance
   protected def report: Report
   def logPrefix: String
   protected def log(s : => String) = report(logPrefix, s)
   protected def log(e: Error) = report(e)
   protected def logGroup[A](a: => A) : A = {
      report.indent
      try {a}
      finally {report.unindent}
   }
   protected def logTime {log(Report.time)}
   protected def logError(s : => String) = report("error", "(" + logPrefix + ") " + s)
}

/** Instances of Report handle all output to the user */
class Report {
   /** output is categorized, the elements of group determine which categories are considered
    *  the categories "user" (for user input) and "error" are output by default */
   private[api] val groups = scala.collection.mutable.Set[String]("user", "error")
   /** logs a message if logging is switched on for the group */
   def apply(group : => String, msg : => String) : Unit =
	   if (groups.contains(group) || groups.contains("*")) log(group, msg)
   /** outputs an error (categories "error" and "debug" for short and long message, respectively) */
   def apply(e : Error) {
	   apply("error", e.shortMsg)
	   apply("debug", e.getLongMessage)
	}
   /** definitely logs a message */
   private def log(group : => String, msg : => String) {
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
   /** the current indentation */
   private var ind : String = ""
   /** increase indentation */
   private[frontend] def indent {ind += "  "}
   /** decrease indentation */
   private[frontend] def unindent {if (ind.length >= 2) ind = ind.substring(2)}
   /** flushes all handlers */
   def flush {
      handlers foreach {_.flush}
   }
   /** closes all handlers */
   def cleanup {
      handlers foreach {_.cleanup}
   }
}

object Report {
   val groups = List("user", "error", "controller", "extman", "library", "archive", "backend")
   private val df = new java.text.SimpleDateFormat("HH:mm:ss.S")
   def time = df.format(new java.util.Date())
}

abstract class ReportHandler(val id: String) {
   def apply(ind: String, group: String, msg: String)
   def flush {}
   def cleanup {}
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
class FileHandler(val filename : File, timestamps: Boolean) extends ReportHandler(filename.toString) {
   private val file = utils.File.Writer(filename)
   def apply(ind: String, group : String, msg : String) = {
         val t = if (timestamps) Report.time + "\t" else ""
         val m = t + ind + group + ": " + msg
         file.println(m)
         flush
   }
   override def flush {file.flush}
   override def cleanup {file.close}
   override def toString = "file " + filename
}

/** remembers logged lines */
class CacheHandler(id: String) extends ReportHandler(id) {
   //this should not be here, if needed, a special LogHandler should be added temporarily
   private var memory : List[String] = Nil
   def recall = memory.reverse
   def clear {memory = Nil}
   def apply(ind: String, group: String, msg: String) {
      memory ::= ind + group + ": " + msg
   }
}