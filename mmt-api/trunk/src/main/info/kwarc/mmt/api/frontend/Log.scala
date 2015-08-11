package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.utils._

/** extended by all classes that use the logging aspect */
trait Logger {
  // fields are defs not vals to avoid surprises during instantiation/inheritance
  protected def report: Report

  def logPrefix: String

  protected def log(s: => String, subgroup: Option[String] = None) =
    report(logPrefix + subgroup.map("-" + _).getOrElse(""), s)

  protected def log(e: Error) = report(e)

  protected def logGroup[A](a: => A): A = {
    report.indent()
    try {
      a
    }
    finally {
      report.unindent()
    }
  }

  protected def logError(s: => String) = report("error", "(" + logPrefix + ") " + s)
}

/** Instances of Report handle all output to the user */
class Report extends Logger {
  val logPrefix = "report"
  val report = this

  /** output is categorized, the elements of group determine which categories are considered
    * the categories "user" (for user input) and "error" are output by default */
  private[api] val groups = scala.collection.mutable.Set[String]("user", "error")

  /** logs a message if logging is switched on for the group */
  def apply(prefix: => String, msg: => String) {
    lazy val caller = {
      val s = Thread.currentThread.getStackTrace
      //TODO could also be Logger.log etc.
      val i = s.indexWhere(e => e.getClassName == getClass.getName && e.getMethodName == "apply")
      s(i + 1).toString
    }
    val prefixList = utils.stringToList(prefix, "#")
    if (prefixList.forall(p => groups.contains(p))) handlers.foreach(_.apply(ind, caller, prefix, msg))
  }

  /** logs an error */
  def apply(e: Error) {
    val debug = groups contains "debug"
    if (groups.contains("error") || debug)
      handlers.foreach(_.apply(ind, e, debug))

  }

  /** flushes all handlers */
  def flush() {
    handlers.foreach(_.flush())
  }

  /** closes all handlers */
  def cleanup() {
    handlers.foreach(_.cleanup())
  }

  /** the registered handlers */
  private var handlers: List[ReportHandler] = Nil

  /** adds a ReportHandler */
  def addHandler(h: ReportHandler) {
    log("logging to " + h)
    h.init()
    handlers = (handlers ::: List(h)).distinct
  }

  /** removes all ReportHandlers with a certain id */
  def removeHandler(id: String) {
    handlers = handlers.filter(_.id != id)
  }

  /** the current indentation level */
  private var ind = 0

  /** increase indentation */
  private[frontend] def indent() {
    ind += 1
  }

  /** decrease indentation */
  private[frontend] def unindent() {
    if (ind >= 1) ind -= 1
  }

}

object Report {
  val groups = List("user", "error", "controller", "extman", "library", "archive", "backend")
  val df = new java.text.SimpleDateFormat("HH:mm:ss.S")
}

/**
 * takes a log message from [[Report]] and displays/stores etc. it
 * @param id an identifier for this handler
 */
abstract class ReportHandler(val id: String) {
  /**
   * logs a message
   * @param ind indentation level
   * @param group generating component
   * @param msg the message
   */
  def apply(ind: Int, caller: String, group: String, msg: String)

  /** logs as an error (categories "error" and "debug" for short and long message, respectively) */
  def apply(ind: Int, e: Error, debug: Boolean) {
    val caller = e.getStackTrace()(0).toString
    val msg = e match {
      case _: Invalid | _: ParseError => contentErrorHighlight(e.shortMsg)
      case _ => systemErrorHighlight(e.shortMsg)
    }
    apply(ind, caller, "error", msg)
    if (debug)
      apply(ind, caller, "debug", e.toStringLong)
  }

  def systemErrorHighlight(s: String): String = s

  def contentErrorHighlight(s: String): String = s

  /** produces a timestamp */
  def time: String = Report.df.format(new java.util.Date())

  /** turns indentation level into a string of spaces */
  def indentString(i: Int): String = Range(0, i).map(_ => "  ").mkString("")

  /** flushes the handler, nothing by default */
  def flush() {}

  /** initializes the handler, nothing by default */
  def init() {}

  /** closes the handler, nothing by default */
  def cleanup() {}

  /** returns the id */
  override def toString: String = id
}

/** outputs to standard output */
object ConsoleHandler extends ReportHandler("console") {
  def apply(ind: Int, caller: String, group: String, msg: String): Unit = {
    val m = indentString(ind) + group + ": " + msg
    println(m)
  }

  /* see http://mihai-nita.net/2013/06/03/eclipse-plugin-ansi-in-console/ for ANSI escape codes
   * e.g., 30-37: colors, 1 bold, 3 italic, 0 reset
   */
  override def systemErrorHighlight(s: String): String = "\u001b[31;1m" + s + "\u001b[0m"

  override def contentErrorHighlight(s: String): String = "\u001b[34;103m" + s + "\u001b[0m"
}

/** common methods for logging to a file */
abstract class FileHandler(val filename: File) extends ReportHandler(filename.toString) {
  protected val file = utils.File.Writer(filename)

  override def flush() {
    file.flush()
  }

  override def cleanup() {
    file.close()
  }
}

/** outputs to a file */
class TextFileHandler(filename: File, timestamps: Boolean) extends FileHandler(filename) {
  def apply(ind: Int, caller: String, group: String, msg: String) {
    val t = if (timestamps) time + "\t" else ""
    val m = t + indentString(ind) + group + ": " + msg
    file.println(m)
    flush()
  }

  override def toString: String = "file " + filename
}

/** outputs to a file in html syntax */
class HtmlFileHandler(filename: File) extends FileHandler(filename) {
  override def init() {
    super.init()
    val script = """<script type="text/javascript" src="script.js"></script>"""
    val jquery = "<script type=\"text/javascript\" src=" +
      "\"https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/resources/mmt-web/script/jquery/jquery.js\"></script>"
    val css = """<link rel="stylesheet" type="text/css" href="style.css"></link>"""
    val pref = """<?xml version="1.0" encoding="UTF-8"?>"""
    file.println(s"$pref\n<html>\n$jquery$script$css<body>\n")
  }

  def apply(ind: Int, caller: String, group: String, msg: String) {
    file.println( s"""<div class="log $group" style="margin-left: $ind%">""")
    file.println( s"""<div><span class="timestamp">$time</span><span class="caller">$caller</span></div>""")
    file.println( s"""<div><span class="group">$group:</span><span class="message">$msg</span></div>""")
    file.println("</div>")
    flush()
  }

  override def apply(ind: Int, e: Error, debug: Boolean) {
    file.println( s"""<div class="log error" style="margin-left: $ind%">""")
    file.println( s"""<div><span class="timestamp">$time</span><span class="error-short">${e.shortMsg}</span></div>""")
    if (debug) {
      e.toStringLong.split("\\n").toList.foreach { line =>
        file.println( s"""<div class="error-long"><span>$line</span></div>""")
      }
    }
    file.println("</div>")
  }

  override def toString: String = "html " + filename

  override def cleanup() {
    file.println("</body>\n</html>\n")
    super.cleanup()
  }
}

/** remembers logged lines */
class RecordingHandler(id: String) extends ReportHandler(id) {
  private var memory: List[String] = Nil
  private var recording = false

  def record() {
    recording = true
  }

  def stop: List[String] = {
    recording = false
    memory.reverse
  }

  def clear() {
    memory = Nil
  }

  def apply(ind: Int, caller: String, group: String, msg: String) {
    if (recording)
      memory ::= indentString(ind) + group + ": " + msg
  }
}
