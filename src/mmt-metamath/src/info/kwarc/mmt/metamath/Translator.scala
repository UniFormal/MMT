package info.kwarc.mmt.metamath

import info.kwarc.mmt.api.archives.BuildTask
import info.kwarc.mmt.api.documents.Document
import info.kwarc.mmt.api.frontend.{Controller, Logger}
import info.kwarc.mmt.api.objects._
import Metamath._
import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.symbols
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.api.opaque.OpaqueText
import info.kwarc.mmt.api.opaque.StringFragment
import org.metamath.scala._
import info.kwarc.mmt.api.MPath

/*
class Translator(val controller: Controller, bt: BuildTask, index: Document => Unit) extends Logger {
  def logPrefix = "mm-omdoc"
  protected def report = controller.report

  val path = bt.narrationDPath.^!.^!
  
  def addDatabase(db: Database, mod: MPath) {
    val theory = new DeclaredTheory(path, mod.name, Some(Metamath.prelude), Context.empty)
    controller add theory
    implicit def treeToTerm(t: ParseTree): Term = t match {
      case HypNode(v: Floating) => OMV(v.v.id)
      case HypNode(h) => OMS(mod ? h.label)
      case AssertNode(a, child) => OMA(OMS(mod ? a.label), child map treeToTerm)
    }
    implicit def formulaToTerm(f: Formula): Term = OMS(mod ? f.typecode.id)(f.parse)

    db.decls foreach {
      case a: Assert =>
        val term = OMBINDC(FL, a.frame.hyps collect {
          case Floating(label, tc, v) =>
            VarDecl(LocalName(v.id), Some(OMS(mod ? tc.id)), None, None)
        }, a.frame.hyps.foldRight(a.formula: Term)((h, t: Term) => h match {
          case Essential(_, hypf) => ES(hypf, t)
          case _ => t
        }) :: a.frame.dv.map { d => OMA(DV, d.v.map(v => OMV(v.id)).toList) })
        controller add symbols.Constant(OMMOD(mod), LocalName(a.label), Nil, Some(term), None, None)
      case c: Comment =>
        controller add new OpaqueText(mod.toDPath, List(StringFragment(c.text)))
      case _ =>
    }
    controller add theory
  }
}
*/