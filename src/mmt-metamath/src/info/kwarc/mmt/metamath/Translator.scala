package info.kwarc.mmt.metamath

import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Controller, Logger}


class Translator(val controller: Controller, bt: BuildTask, index: Document => Unit) extends Logger {
  def logPrefix = "mm-omdoc"
  protected def report = controller.report

  val path = bt.narrationDPath.^!.^!

}