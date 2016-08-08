package info.kwarc.mmt.metamath

import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Controller, Logger}
import info.kwarc.mmt.api.objects._
import Metamath._
import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api.modules.DeclaredTheory


class Translator(val controller: Controller, bt: BuildTask, index: Document => Unit) extends Logger {
  def logPrefix = "mm-omdoc"
  protected def report = controller.report

  val path = bt.narrationDPath.^!.^!
  
  def addDatabase(db: MMDatabase) {
    val theory = new DeclaredTheory(path, db.mod.name, Some(Metamath.prelude), Context.empty)
    db.Statements.list foreach {
      case Assert(label, formula, frame, proof) =>
        val term = OMBINDC(FL, frame.hyps collect {
          case Floating(label, tc, v) =>
            VarDecl(LocalName(v.id), Some(OMS(db.mod ? tc.id)), None, None)
        }, frame.hyps.foldRight(formula.parse)((h, t) => h match {
          case Essential(_, hypf) => ES(hypf.parse, t)
          case _ => t
        }) :: frame.dv.map {
          d: Disjointness => OMA(DV, d.v.map(v => OMV(v.id)).toList)
        })
        theory add symbols.Constant(OMMOD(db.mod), label.name, Nil, Some(term), None, None)
      case _ =>
    }
    controller add theory
  }
}