package info.kwarc.mmt.api.frontend

import java.io.IOException

import info.kwarc.mmt.api._
import utils._

/** extended by all classes that use the logging aspect */
trait Logger {
  // fields are defs not vals to avoid surprises during instantiation/inheritance
  protected def report: Report

  def logPrefix: String

  /** logs a message with this logger's logprefix */
  protected def log(s: => String, subgroup: Option[String] = None): Unit =
    report(logPrefix + subgroup.map("-" + _).getOrElse(""), s)

  /** temporary logging - always logged */
  // calls to this method are for debugging; if they are committed, they should be removed
  protected def logTemp(s: => String): Unit =
    report("temp", s"($logPrefix) $s")

  /** log as an error message */
  protected def logError(s: => String): Unit = report("error", s"($logPrefix) $s")

  /** logs an error - always logged */
  protected def log(e: Error): Unit = report(e)

  /** wraps around a group to create nested logging */
  protected def logGroup[A](a: => A): A = {
    report.indent
    try {
      a
    } finally {
      report.unindent
    }
  }
}

/** Instances of Report handle all output to the user */
class Report extends Logger {
  val logPrefix = "report"
  val report = this

  /** output is categorized, the elements of group determine which categories are considered
    * the categories "user" (for user input), "error" are output by default, and "temp" (for temporary logging during debugging) */
  private[mmt] val groups = scala.collection.mutable.Set[String]("user", "error", "temp", "response")
  /**  true if debug logging is enabled */
  def checkDebug = groups contains "debug"
  
  private var counter = -1
  private def nextId = {counter += 1; counter}
  
  /** this does nothing and should only be called during debugging:
   *  it provides a convenient way to hit a breakpoint at a specific call to "log"
   *  to use, set a breakpoint in this method and call "breakOnId(i)" after the call to "log" at which you want to break
   */
  def breakOnId(id: Int): Unit = {
    if (id <= counter)
      return
  }
  
  /** gets a list of active groups */
  def active : List[String] = groups.toList

  /** logs a message if logging is switched on for the group */
  def apply(prefix: => String, msg: => String): Unit = {
    lazy val caller = {
      val s = Thread.currentThread.getStackTrace
      //TODO this is not always the most helpful entry
      var i = s.lastIndexWhere(e => e.getMethodName == "log")
      if (i == -1) i = 0
      s(i+1).toString
    }
    val prefixList = utils.stringToList(prefix, "#")
    if (prefixList.forall(p => groups.contains(p)) || groups.contains("all")) {
      var msgParts = utils.stringToList(msg, "\\n")
      if (checkDebug) {
        val id = nextId
        val (hd,tl) = msgParts match {
          case Nil => ("",Nil)
          case h::t => (h,t)
        }
        msgParts = (hd + s" [message id: $id]") :: tl
      }
      handlers.foreach(_.apply(ind, caller, prefix, msgParts))
    }
  }

  /** logs an error */
  def apply(e: Error): Unit = {
    val debug = checkDebug
    if (groups.contains("error") || debug)
      handlers.foreach(_.apply(ind, e, debug))
  }

  /** flushes all handlers */
  def flush: Unit = {
    handlers.foreach(_.flush)
  }

  /** closes all handlers */
  def cleanup: Unit = {
    handlers.foreach(_.cleanup)
  }

  /** the registered handlers */
  private var handlers: List[ReportHandler] = Nil

  /** adds a ReportHandler */
  def addHandler(h: ReportHandler): Unit = {
    log("logging to " + h)
    h.init
    handlers = (handlers ::: List(h)).distinct
  }

  /** removes all ReportHandlers with a certain id */
  def removeHandler(id: String): Unit = {
    handlers = handlers.filter(_.id != id)
  }

  /** the current indentation level */
  private var ind = 0

  /** increase indentation */
  private[frontend] def indent: Unit = {
    ind += 1
  }

  /** decrease indentation */
  private[frontend] def unindent: Unit = {
    if (ind >= 1) ind -= 1
  }

  /** enables logging for a set of groups within a block */
  def withGroups[A](groups: String*)(a: => A): A = {
    val theGroups = groups.filterNot(this.groups.contains) // the groups that we did not have beforehand
    theGroups.foreach(this.groups += _)
    try {
      a
    } finally {
      theGroups.foreach(this.groups -= _)
    }
  }
}

object Report {
  val df = new java.text.SimpleDateFormat("HH:mm:ss.S")
}

/**
 * takes a log message from [[Report]] and displays/stores etc. it
  *
  * @param id an identifier for this handler
 */
abstract class ReportHandler(val id: String) {
  /**
   * logs a message
    *
    * @param ind indentation level
   * @param group generating component
   * @param msgParts the multi-line message
   */
  def apply(ind: Int, caller: => String, group: String, msgParts: List[String]): Unit

  /** logs as an error (categories "error" and "debug" for short and long message, respectively) */
  def apply(ind: Int, e: Error, debug: Boolean): Unit = {
    val caller = e.getStackTrace()(0).toString
    val (msg,content) = e match {
      case _: ContentError => (contentErrorHighlight(e.shortMsg), e.getCausedBy.isEmpty)
      case _ => (systemErrorHighlight(e.shortMsg), false)
    }
    apply(ind, caller, "error", List(s"(${e.levelString}) " + msg))
    if (debug && !content) {
      apply(ind, caller, "debug", utils.stringToList(e.toStringLong, "\\n"))
    }
  }

  def systemErrorHighlight(s: String): String = s

  def contentErrorHighlight(s: String): String = s

  /** produces a timestamp */
  def time: String = Report.df.format(new java.util.Date())

  /** turns indentation level into a string of spaces */
  def indentString(i: Int): String = Range(0, i).map(_ => "  ").mkString("")

  /** flushes the handler, nothing by default */
  def flush: Unit = {}

  /** initializes the handler, nothing by default */
  def init: Unit = {}

  /** closes the handler, nothing by default */
  def cleanup: Unit = {}

  /** returns the id */
  override def toString: String = id
}

/** outputs to standard output */
object ConsoleHandler extends ReportHandler("console") {
  def apply(ind: Int, caller: => String, group: String, msgParts: List[String]): Unit = {
    msgParts.foreach { msg =>
      val m = indentString(ind) + group + ": " + msg
      println(m)
    }
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
  protected var alreadyClosed = false

  override def flush: Unit = {
    file.flush
  }

  override def cleanup: Unit = {
    if (!alreadyClosed) {
      file.close
      alreadyClosed = true
    }
  }
}

/** outputs to a file */
class TextFileHandler(filename: File, timestamps: Boolean) extends FileHandler(filename) {
  def apply(ind: Int, caller: => String, group: String, msgParts: List[String]): Unit = {
    val t = if (timestamps) time + "\t" else ""
    msgParts.foreach { msg =>
      val m = t + indentString(ind) + group + ": " + msg
      file.println(m)
    }
    flush
  }

  override def toString: String = "file " + filename
}

/** outputs to a file in html syntax */
class HtmlFileHandler(filename: File) extends FileHandler(filename) {
  override def init: Unit = {
    super.init
    val scrfile = filename.up / "logaux" / "script.js"
    val jqfile = filename.up / "logaux" / "jquery.js"
    val cssfile = filename.up / "logaux" / "style.css"

    if (!scrfile.exists) File.write(scrfile,MMTSystem.getResourceAsString("/log-html/script.js"))
    if (!jqfile.exists) File.write(jqfile,MMTSystem.getResourceAsString("/mmt-web/script/jquery/jquery.js"))
    if (!cssfile.exists) File.write(cssfile,MMTSystem.getResourceAsString("/log-html/style.css"))

    val script = """<script type="text/javascript" src="logaux/script.js"></script>"""
    val jquery = "<script type=\"text/javascript\" src=" +
      "\"logaux/jquery.js\"></script>"
    val css = """<link rel="stylesheet" type="text/css" href="logaux/style.css"></link>"""
    val pref = """<!DOCTYPE html><html><head><meta charset="UTF-8">"""
    file.println(s"$pref\n$jquery$script$css</head><body>\n")
  }

  def apply(ind: Int, caller: => String, group: String, msgParts: List[String]): Unit = {
    file.println( s"""<div class="log $group" style="margin-left: $ind%">""")
    file.println( s"""<div><span class="timestamp">$time</span><span class="caller">$caller</span></div>""")
    val msg = msgParts.mkString("<br/>")
    file.println( s"""<div><span class="group">$group:</span><span class="message">$msg</span></div>""")
    file.println("</div>")
    flush
  }

  override def apply(ind: Int, e: Error, debug: Boolean): Unit = {
    val (cls,refO) = e match {
      case e: ContentError => ("content-error", e.sourceRef)
      case _ => ("error", None)
    }
    file.println( s"""<div class="log $cls" style="margin-left: $ind%">""")
    file.println( s"""<div><span class="timestamp">$time</span><span class="error-short">${e.shortMsg}</span></div>""")
    refO.foreach {ref =>
      file.println(s"""<div class="sourceref"><span class="sourceref" data-mmt-ref="${ref.toString}">${ref.toString}</span></div>""")
    }
    if (debug) {
      e.toStringLong.split("\\n").toList.foreach { line =>
        file.println( s"""<div class="error-long"><span>$line</span></div>""")
      }
    }
    file.println("</div>")
  }

  override def toString: String = "html " + filename

  override def cleanup: Unit = {
    if (!alreadyClosed) {
      file.println("</body>\n</html>\n")
      super.cleanup
    }
  }
}

/** remembers logged lines */
class RecordingHandler(id: String) extends ReportHandler(id) {
  private var memory: List[String] = Nil
  private var recording = false

  def record: Unit = {
    recording = true
  }

  def stop: List[String] = {
    recording = false
    memory.reverse
  }

  def clear: Unit = {
    memory = Nil
  }

  def apply(ind: Int, caller: => String, group: String, msgParts: List[String]): Unit = {
    if (recording) {
      msgParts.foreach { msg =>
        memory ::= indentString(ind) + group + ": " + msg
      }
    }
  }
}
