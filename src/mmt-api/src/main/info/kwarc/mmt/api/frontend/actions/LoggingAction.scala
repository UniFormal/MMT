package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.frontend._

/** shared base class for actions related to logging */
sealed abstract class LoggingAction extends ActionImpl {}

case object ListReportGroups extends LoggingAction with ResponsiveAction {
  def apply(implicit controller: Controller) = {
    respond("the following log groups are active: ")

    logGroup {
      report.active.foreach(respond(_))
    }

    respond("use 'log+ <group>' to add a log group. ")
    respond("use 'log- <group>' to remove a log group. ")
  }
  def toParseString: String = "log"
}
object ListReportGroupsCompanion extends ActionObjectCompanionImpl("list active log groups", "log")

case class AddReportHandler(h: ReportHandler) extends LoggingAction {
  def apply(implicit controller: Controller): Unit = controller.report.addHandler(h)
  def toParseString = s"log $h"
}
object AddReportHandlerCompanion extends ActionCompanionImpl[AddReportHandler]("add a log handler", "log") {
  import Action._

  def parserActual(implicit state: ActionState) = logfilets | logfile | loghtml | logconsole
  private def logfile(implicit state: ActionState) = "file" ~> file ^^ { f => AddReportHandler(new TextFileHandler(f, false)) }
  private def logfilets(implicit state: ActionState) = "filets" ~> file ^^ { f => AddReportHandler(new TextFileHandler(f, true)) }
  private def logconsole(implicit state: ActionState) = "console" ^^ { case _ => AddReportHandler(ConsoleHandler) }
  private def loghtml(implicit state: ActionState) = "html" ~> file ^^ { f => AddReportHandler(new HtmlFileHandler(f)) }
}

case class LoggingOn(group: String) extends LoggingAction {
  def apply(implicit controller: Controller): Unit = report.groups += group
  def toParseString = s"log+ $group"
}
object LoggingOnCompanion extends ActionCompanionImpl[LoggingOn]("switch on logging for a certain group", "log+") {
  import Action._
  def parserActual(implicit state: ActionState) = str ^^ { s => LoggingOn(s) }
}

case class LoggingOff(group: String) extends LoggingAction {
  def apply(implicit controller: Controller): Unit = report.groups -= group
  def toParseString = s"log- $group"
}
object LoggingOffCompanion extends ActionCompanionImpl[LoggingOff]("switch off logging for a certain group", "log-") {
  import Action._
  def parserActual(implicit state: ActionState) = str ^^ { s => LoggingOff(s) }
}