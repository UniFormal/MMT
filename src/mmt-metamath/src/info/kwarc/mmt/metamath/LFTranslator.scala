package info.kwarc.mmt.metamath

import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Controller, Logger}
import info.kwarc.mmt.api.objects._
import Metamath._
import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api.modules.DeclaredTheory


class LFTranslator(val controller: Controller, bt: BuildTask, index: Document => Unit) extends Logger {
  def logPrefix = "mm-omdoc"
  protected def report = controller.report

  val path = bt.narrationDPath.^!.^!
  
  def addDatabase(db: MMDatabase) {
    require(db.mod == Metamath.setmm)
    val theory = new DeclaredTheory(path, db.mod.name, Some(Metamath.prelude), Context.empty)
    db.Statements.list foreach {
      case Assert(label, formula, frame, proof) =>
        val term = ???
        theory add symbols.Constant(OMMOD(db.mod), label.name, Nil, Some(term), None, None)
      case _ =>
    }
    controller add theory
  }
}