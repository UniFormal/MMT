package info.kwarc.mmt.metamath

import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Controller, Logger}
import info.kwarc.mmt.api.objects._
import Metamath._
import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api.modules.DeclaredTheory
import org.metamath.scala.Database
import info.kwarc.mmt.api.MPath
import org.metamath.scala.Assert


class LFTranslator(val controller: Controller, bt: BuildTask, index: Document => Unit) extends Logger {
  def logPrefix = "mm-omdoc"
  protected def report = controller.report

  val path = bt.narrationDPath.^!.^!
  
  def addDatabase(db: Database) {
    val mod = Metamath.setmm
    val theory = new DeclaredTheory(path, mod.name, Some(Metamath.prelude), Context.empty)
    db.decls foreach {
      case a: Assert =>
        val term = ???
        theory add symbols.Constant(OMMOD(mod), LocalName(a.label), Nil, Some(term), None, None)
      case _ =>
    }
    controller add theory
  }
}