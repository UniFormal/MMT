package info.kwarc.mmt.lf.test

import info.kwarc.mmt.api.archives.RedirectableDimension
import info.kwarc.mmt.api.archives.lmh.LMHHubArchiveEntry
import info.kwarc.mmt.api.test.utils.testers._
import info.kwarc.mmt.api.test.utils._
import info.kwarc.mmt.api.utils.File

class LFTest extends MMTIntegrationTest(
  TestArchive("MMT/urtheories", hasDevel = true),
  TestArchive("MMT/examples", hasDevel = true),
)(
  ExtensionSpec("info.kwarc.mmt.lf.Plugin")
) {

  bootstrapTests()

  shouldLoadExtensions()
  shouldInstallArchives()

  shouldCheck("MMT/urtheories")()

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
    ,"sets/typed_sets.mmt"
    ,"sets/relations.mmt"
    ,"sets/functions.mmt"
    ,"graphs/graphs.mmt"
    ,"collections/collections.mmt"
    ,"collections/matroids.mmt"
    ,"topology/topology.mmt"
    ,"algebra/basics.mmt"
    ,"algebra/ringsfields.mmt"
    ,"algebra/modulsvectors.mmt"
    ,"algebra/numberspaces.mmt"
    ,"algebra/permutation-group.mmt"
    ,"algebra/polynomials.mmt"
    ,"elliptic_curves/algebra_base.mmt"
    ,"elliptic_curves/Weierstrass-model.omf.mmt"
    ,"elliptic_curves/elliptic-curve.omf.mmt"
    ,"elliptic_curves/minimal-Weierstrass-model.omf.mmt"
    ,"elliptic_curves/a-invariants.omf.mmt"
    ,"elliptic_curves/complex-multiplication.omf.mmt"
    ,"elliptic_curves/conductor.omf.mmt"
    ,"elliptic_curves/discriminant.omf.mmt"
    ,"elliptic_curves/isogeny-class.omf.mmt"
    ,"elliptic_curves/modular-degree.omf.mmt"
    ,"elliptic_curves/q-expansion.omf.mmt"
    ,"elliptic_curves/elliptic-curve-toolchest.omf.mmt"
    ,"algebra/hecke.mmt"
    ,"sets/poset.mmt"
    ,"algebra/lattice.mmt"
    ,"calculus/normedmetric.mmt"
    ,"calculus/sequencesconvergence.mmt"
    ,"calculus/derivatives.mmt"
    ,"calculus/domains.mmt"
    ,"measures/setalgebras.mmt"
    ,"measures/measures.mmt"
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
    // ,"patterns.mmt" TODO broke the tests
    //,"module_expressions.mmt" TODO doesn't type check at all
    ,"program.mmt"
    // ,"quantities.mmt" TODO broke the tests
    //,"sequences.mmt" TODO doesn't type check
    ,"set.mmt"
    ,"shallow_polymorphism.mmt"
    ,"sigma.mmt"
    // ,"IFIP21_tutorial.mmt" TODO broke the tests
  )
}
