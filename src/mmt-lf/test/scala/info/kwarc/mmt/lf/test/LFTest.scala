package info.kwarc.mmt.lf.test

import info.kwarc.mmt.api.test.MMTTest

class LFTest extends MMTTest("MMT/LFX","MMT/examples","Test/General")("info.kwarc.mmt.lf.Plugin") {
  behavior of "LF"
  // shouldhl("build MMT/urtheories scala-bin")
  // shouldhl("build MMT/urtheories mmt-omdoc")
  //shouldhl("build MMT/examples scala-bin")
  //shouldhl("build MMT/examples mmt-omdoc")
  shouldhl("build MMT/LFX mmt-omdoc")

  shouldcheck("Test/General",Orders.testgeneral:_*)(mayfail = List("http://test.kwarc.info/Structure?C?test2?definition"))
}

class MitMTest extends MMTTest("MMT/LFX","MitM/Foundation","MitM/smglom")("info.kwarc.mmt.lf.Plugin") {
  behavior of "MitM"
  // shouldhl("build MitM/Foundation mmt-omdoc")

  // shouldcheck("MitM/smglom",Orders.mitmsmglom:_*)()
}

object Orders {
  val mitmsmglom = List(
    "arithmetics/naturals.mmt",
    "arithmetics/integers.mmt",
    "arithmetics/rationals.mmt",
    "arithmetics/reals.mmt",
    "typed_sets.mmt",
    "functions.mmt",
    "topology/basics.mmt",
    "algebra/basics.mmt",
    "algebra/ringsfields.mmt",
    "algebra/numberspaces.mmt",
    "algebra/modulsvectors.mmt",
    "calculus/normedmetric.mmt",
    "calculus/sequencesseries.mmt",
    "calculus/limitsderivatives.mmt",
    "calculus/domains.mmt"
    // "categories/basics.mmt"
  )

  val testgeneral = List(
    "hol.mmt",
    "logic/pl.mmt",
    "logic/fol.mmt"
  )
}