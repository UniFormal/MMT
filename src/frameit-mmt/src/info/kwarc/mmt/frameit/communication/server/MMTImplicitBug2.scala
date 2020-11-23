package info.kwarc.mmt.frameit.communication.server

import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.symbols.{PlainInclude, Structure, TermContainer}
import info.kwarc.mmt.api.{LocalName, NamespaceMap, Path}
import info.kwarc.mmt.frameit.business.Utils

/** Reproducing an MMT bug about implicit morphisms not getting added when nested modules are involved. **/
private object MMTImplicitBug2 {
  def main(args: Array[String]): Unit = {
    implicit val ctrl: Controller = new Controller()
    ctrl.report.addHandler(ConsoleHandler)
    ctrl.handleLine(s"mathpath archive C:\\Users\\nroux\\Desktop\\mmt-archives")

    val doc = Path.parseD("http://cds.omdoc.org/urtheories", NamespaceMap.empty)

    val firstTheory = Theory.empty(doc, LocalName("A"), None)
    ctrl.add(firstTheory)

    val secondTheory = Theory.empty(doc, LocalName("B"), None)

    val tpC = new TermContainer()
    tpC.read = Some("?A")
    tpC.parsed = Some(firstTheory.toTerm)

    val s = new Structure(
      home = secondTheory.toTerm,
      name = LocalName(firstTheory.path),
      tpC = tpC,
      dfC = TermContainer.empty(),
      isImplicit = true,
      isTotal = false
    )

    // note: ctrl.

    // fails: does not add implicit
    ctrl.add(secondTheory)
    ctrl.add(s)
    ctrl.endAdd(secondTheory)
    ctrl.endAdd(s)

    /*// works: adds implicit
    ctrl.add(secondTheory)
    secondTheory.add(s)
    ctrl.add(secondTheory)

    // works: adds implicit
    ctrl.add(secondTheory)
    ctrl.add(s)
    ctrl.add(secondTheory)*/

    println(ctrl.globalLookup.getImplicit(firstTheory.path, secondTheory.path)) // actual: None, expected: Some(...)
  }
}
