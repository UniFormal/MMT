package info.kwarc.mmt.test

import info.kwarc.mmt.test.LFTest.shouldClearTarget
import info.kwarc.mmt.test.testers._

object LFTest extends MMTIntegrationTest(
  "MMT/urtheories",
  "MMT/examples",
)(
  ExtensionSpec("info.kwarc.mmt.lf.Plugin")
) {
  def main(): Unit = {
    shouldClearTarget("MMT/urtheories", "bin")
    shouldHandleLine("build MMT/urtheories scala-bin")

    shouldCheck("MMT/urtheories")()

    shouldClearTarget("MMT/examples", "bin")
    shouldHandleLine("build MMT/examples scala-bin")

    shouldCheck("MMT/examples",Orders.examples:_*)(onlyfiles = true)
  }
}

object Latin2Test extends MMTIntegrationTest(
  "MMT/urtheories","MMT/LATIN2"
)(
  ExtensionSpec("info.kwarc.mmt.lf.Plugin")
) {
  def main() : Unit = {

  }
}

object Orders {
  val latin2 = List(
    "fundamentals/concepts.mmt",
    "fundamentals/relations.mmt",
    "fundamentals/equality.mmt",
    "fundamentals/subtyping.mmt",
    "logic/operators.mmt",
    "logic/pl.mmt",
    "logic/sfol.mmt",
    "logic/booleans.mmt",
    "logic/pl-sfol.mmt",
    "logic/fol.mmt",
    "logic/pl-tableaux.mmt",
    "logic/metalogic.mmt",
    "logic/pl-views.mmt",
    "logic/pl_hilbert.mmt",
    "logic/fol_hilbert.mmt",
    "logic/drt.mmt",
    "type_theory/operators.mmt",
    "type_theory/function_types.mmt",
    "type_theory/power_types.mmt",
    "logic/hol.mmt",
    "logic/modal.mmt",
    "logic/multimodal.mmt",
    "logic/dynamic.mmt",
    "logic/hol_andrews.mmt",
    "type_theory/type_equality.mmt",
    "type_theory/type_erasure.mmt",
    "type_theory/product_types.mmt",
    "type_theory/category_theory.mmt",
    "type_theory/congruence_types.mmt",
    "type_theory/predicate_subtypes.mmt",
    "type_theory/undefinedness.mmt",
    "type_theory/quotation.mmt",
    "type_theory/endofunctors/endofunctors.mmt",
    "type_theory/endofunctors/endomagmas.mmt",
    // "type_theory/endofunctors/monads.mmt", // TODO doesn't work
    "type_theory/collection_types.mmt",
    "set_theory/features.mmt",
    "curry-howard/concepts.mmt",
    "topology/topology.mmt",
    "topology/topology_morph.mmt",
    "proving/fundamentals.mmt",
    "numbers/nat.mmt",
    // "numbers/nat-induct.mmt", // TODO doesn't work
    //"algebra/sets.mmt",
    //"algebra/relations.mmt",
    "algebra/magmas.mmt",
    // "algebra/additive_magmas.mmt", // TODO doesn't work
  )
  val mitmcore = List(
    "arithmetics/naturals.mmt"
    ,"arithmetics/integers.mmt"
    ,"arithmetics/rationals.mmt"
    ,"arithmetics/reals.mmt"
    ,"arithmetics/complex.mmt"
    ,"sets/typed_sets.mmt"
    ,"sets/relations.mmt"
    ,"sets/functions.mmt"
    ,"sets/finiteSet.mmt"
    ,"sets/setSum.mmt"
    ,"graphs/graphs.mmt"
    ,"graphs/maniplexes.mmt"
    ,"collections/collections.mmt"
    ,"collections/matroids.mmt"
    ,"topology/topology.mmt"
    ,"algebra/basics.mmt"
    ,"algebra/ringsfields.mmt"
    ,"algebra/modulsvectors.mmt"
    ,"algebra/numberspaces.mmt"
    ,"algebra/polynomials.mmt"
    ,"algebra/permutation-group.mmt"
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
    ,"sets/filter.mmt"
    ,"sets/posetPowerset.mmt"
    ,"numbertheory/intervals.mmt"
    ,"numbertheory/additiveBases.mmt"
    ,"algebra/bands.mmt"
    ,"algebra/lattice.mmt"
    ,"calculus/FinSequences.mmt"
    ,"calculus/metric.mmt"
    ,"calculus/sequencesconvergence.mmt"
    ,"calculus/derivatives.mmt"
    ,"calculus/domains.mmt"
    ,"functional-analysis/norms.mmt"
    ,"topology/topologies.mmt"
    ,"measures/setalgebras.mmt"
    ,"measures/measures.mmt"
    ,"calculus/integration/SimpleFunctions.mmt"
    ,"calculus/integration/LebesgueIntegral.mmt"
    ,"categories/categories.mmt"
    ,"categories/functors.mmt"
    ,"geometry/common.mmt"
    ,"geometry/planar.mmt"
    ,"geometry/3D.mmt"
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
    // ,"hott.mmt"
    ,"instances.mmt" // currently mostly commented out?
    ,"nat.mmt"
    ,"int.mmt"
    ,"literals.mmt"
    // ,"patterns.mmt" TODO broke the tests
    //,"module_expressions.mmt" TODO doesn't type check at all
    ,"program.mmt"
    // ,"quantities.mmt" TODO broke the tests
    //,"sequences.mmt" TODO doesn't type check
    //,"set.mmt" TODO doesn't type check
    ,"shallow_polymorphism.mmt"
    ,"sigma.mmt"
    // ,"IFIP21_tutorial.mmt" TODO broke the tests
    ,"lists.mmt"
    ,"logic/itp.mmt"
    ,"logic/linear.mmt"
    ,"logic/sfol.mmt"
    ,"logic/hol.mmt"
    ,"magmas.mmt"
    // ,"mam.mmt"
    ,"patterns.mmt"
    ,"program.mmt"
    ,"real.mmt"
    ,"quantities.mmt"
    //,"sedel.mmt"
    ,"sequences.mmt"
  )
}