package info.kwarc.mmt.frameit.communication.server

import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.symbols.PlainInclude
import info.kwarc.mmt.api.{LocalName, NamespaceMap, Path}
import info.kwarc.mmt.frameit.business.Utils

/** Reproducing an MMT bug about implicit morphisms not getting added when nested modules are involved. **/
private object MMTImplicitBug {
  def main(args: Array[String]): Unit = {
    implicit val ctrl: Controller = new Controller()
    ctrl.report.addHandler(ConsoleHandler)
    ctrl.handleLine(s"mathpath archive C:\\Users\\nroux\\Desktop\\FrameIT\\archives")

    val doc = Path.parseD("http://cds.omdoc.org/urtheories", NamespaceMap.empty)
    val outerTheory = Theory.empty(doc, LocalName("Outer"), None)
    Utils.addModule(outerTheory)

    val innerTheory = Theory.empty(doc, LocalName("Outer") / LocalName("Inner"), None)
    Utils.addModule(innerTheory)

    // any other theory, doesn't matter which exactly
    val includedTheoryPath = Path.parseM("http://cds.omdoc.org/urtheories?LF")
    ctrl.add(PlainInclude(includedTheoryPath, innerTheory.path))
    ctrl.endAdd(PlainInclude(includedTheoryPath, innerTheory.path))

    Utils.endAddModule(innerTheory)
    Utils.endAddModule(outerTheory)

    // Actual value being printed: None
    // Expected value: Some(...)
    println(ctrl.globalLookup.getImplicit(includedTheoryPath, innerTheory.path))
  }
}
