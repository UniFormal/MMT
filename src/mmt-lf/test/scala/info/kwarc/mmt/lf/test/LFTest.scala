package info.kwarc.mmt.lf.test

import info.kwarc.mmt.api.test.utils.testers._
import info.kwarc.mmt.api.test.utils._

class LFTest extends MMTIntegrationTest(
  "MMT/urtheories",
  "MMT/LFX",
  "Test/General",
  "MMT/examples"
)(
  ExtensionSpec("info.kwarc.mmt.lf.Plugin")
) {

  bootstrapTests()

  shouldLoadExtensions()
  shouldInstallArchives()

  it should "build the scala-bin target" in {
    handleLine("log+ debug")
    handleLine("build MMT/LFX scala-bin")
    handleLine("log- debug")
  }

  shouldHandleLine("build MMT/LFX mmt-omdoc")

  shouldCheck("Test/General", Orders.testgeneral:_*)(mayfail = List("http://test.kwarc.info/Structure?C?test2?definition"))
  shouldCheck("MMT/examples",Orders.examples:_*)(onlyfiles = true)
}

/*
class MitMTest extends MMTTest("MMT/LFX","MitM/Foundation","MitM/smglom")("info.kwarc.mmt.lf.Plugin") {
  behavior of "MitM"
  // shouldhl("build MitM/Foundation mmt-omdoc")

  // shouldcheck("MitM/smglom",Orders.mitmsmglom:_*)()
}
*/

object Orders {
  val mitmsmglom = List(
    "arithmetics/naturals.mmt"
    ,"arithmetics/integers.mmt"
    ,"arithmetics/rationals.mmt"
    ,"arithmetics/reals.mmt"
    ,"arithmetics/complex.mmt"
    ,"typed_sets.mmt"
    ,"functions.mmt"
    ,"topology/basics.mmt"
    ,"algebra/basics.mmt"
    ,"algebra/ringsfields.mmt"
    ,"algebra/numberspaces.mmt"
    ,"algebra/modulsvectors.mmt"
    ,"calculus/normedmetric.mmt"
    ,"calculus/sequencesseries.mmt"
    ,"calculus/limitsderivatives.mmt"
    ,"calculus/domains.mmt"
    // "categories/basics.mmt"
  )

  val testgeneral = List(
    "hol.mmt"
  )

  val examples = List(
    "logic/pl.mmt"
    ,"logic/fol.mmt"
    ,"logic/prover.mmt"
    ,"programming/machine.mmt"
    ,"programming/rabe_encodings.mmt"
    ,"programming/syntax.mmt"
    ,"programming/semantics.mmt"
    // ,"tutorial/???" TODO broken!
    ,"arithmetic_rules.mmt"
    ,"base-arith.mmt"
    ,"hott.mmt"
    ,"instances.mmt" // currently mostly commented out?
    ,"nat.mmt"
    ,"int.mmt"
    ,"literals.mmt"
    ,"patterns.mmt"
    //,"module_expressions.mmt" TODO doesn't type check at all
    ,"program.mmt"
    ,"quantities.mmt"
    //,"sequences.mmt" TODO doesn't type check
    ,"set.mmt"
    ,"shallow_polymorphism.mmt"
    ,"sigma.mmt"
    ,"IFIP21_tutorial.mmt"
  )
}