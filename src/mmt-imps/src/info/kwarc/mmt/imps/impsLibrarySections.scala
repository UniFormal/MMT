package info.kwarc.mmt.imps

package object impsLibrarySections
{
  // See some-sections.ool

  def pureGenericTheories : Section = Section(
    "pure-generic-theories",
    Nil,
    List("pure-generic-theories.t", "iota.t"),
    List("pure-generic-theory-1.json", "pure-generic-theory-2.json",
         "pure-generic-theory-3.json", "pure-generic-theory-4.json")
  )

  def preReals : Section = Section(
    "pre-reals",
    Nil,
    List("reals.t", "pre-reals.t", "mutual-interp.t"),
    List("h-o-real-arithmetic.json", "ordered-field.json", "complete-ordered-field.json")
  )

  def reals : Section = Section(
    "reals",
    Nil,
    List("reals.t", "some-elementary-macetes.t", "arithmetic-strategies.t"),
    List("h-o-real-arithmetic.json")
  )

  def basicRealArithmetic : Section = Section(
    "basic-real-arithmetic",
    List(reals),
    List("arithmetic-macetes.t", "some-lemmas.t", "number-theory.t"),
    Nil
  )

  def genericTheories : Section = Section(
    "generic-theories",
    List(basicRealArithmetic, pureGenericTheories),
    List("generic-theories.t"),
    List("generic-theory-1.json", "generic-theory-2.json", "generic-theory-3.json", "generic-theory-4.json")
  )

  def indicators : Section = Section(
    "indicators",
    Nil,
    List("indicators.t", "indicator-lemmas.t"),
    List("indicators.json", "family-indicators.json")
  )

  def mappings : Section = Section(
    "mappings",
    List(indicators, pureGenericTheories, genericTheories),
    List("mappings.t", "mapping-lemmas.t", "inverse-lemmas.t"),
    Nil
  )

  def foundation : Section = Section(
    "foundation",
    List(genericTheories, mappings),
    List("pure-generic-theories-with-subsorts.t", "reals-supplements.t", "some-obligations.t"),
    List("pure-generic-theory-1-with-1-subsort.json", "pure-generic-theory-2-with-1-subsort.json",
         "pure-generic-theory-2-with-2-subsorts.json", "pure-generic-theory-3-with-2-subsorts.json")
  )

  def foundationSupplements : Section = Section(
    "foundation-supplements",
    List(foundation),
    List("indicator-supplements.t", "family-indicator-supplements.t"),
    Nil
  )

  def numberTheory : Section = Section(
    "number-theory",
    List(foundation),
    List("primes.t", "modular-arithmetic.t", "modular-arithmetic-examples.t"),
    List("arithmetic-mod-n.json", "arithmetic-mod-5.json", "commutative-monoid-theory.json")
  )

  def calculusOverTheReals : Section = Section(
    "calculus-over-the-reals",
    List(foundation),
    List("limits.t", "examples.t"),
    Nil
  )

  def pairs : Section = Section(
    "pairs",
    List(foundation),
    List("pairs.t"),
    Nil
  )

  def binaryRelaions : Section = Section(
    "binary-relations",
    List(foundation),
    List("binary-relations.t", "quotients.t"),
    Nil
  )

  def iterate : Section = Section(
    "iterate",
    List(foundation),
    List("iterate.t", "iterate-supplements.t"),
    Nil
  )

  def basicCardinality : Section = Section(
    "basic-cardinality",
    List(foundation),
    List("cardinality.t", "finite-cardinality.t", "partition-lemmas.t"),
    Nil
  )

  def advancedCardinality : Section = Section(
    "advanced-cardinality",
    List(basicCardinality),
    List("omega-embedding-supplements.t", "cardinality-supplements.t", "combinatorics.t"),
    Nil
  )

  def sequences : Section = Section(
    "sequences",
    List(basicCardinality),
    List("omega-embedding.t","sequences.t", "sequences-supplements.t"),
    List("sequences.json")
  )

  def schroederBernsteinTheorem1 : Section = Section(
    "schroeder-bernstein-theorem-1",
    List(foundation),
    List("cardinality.t", "schroeder-bernstein-theorem.t"),
    List("schroeder-bernstein-theory.json")
  )

  def basicGroupTheory : Section = Section(
    "basic-group-theory",
    List(foundation),
    List("groups.t", "subgroups.t", "group-actions.t", "cosets.t", "group-lemmas.t", "normal-subgroups.t", "cyclic-groups.t"), // "cyclic-groups.t"?
    List("groups.json", "group-actions.json", "abelian-groups.json") // "abelian-groups.json"?
  )

  def fundamentalCountingTheorem : Section = Section(
    "fundamental-counting-theorem",
    List(basicCardinality, basicGroupTheory),
    List("group-cardinality.t", "counting-theorem.t"),
    List("counting-theorem-theory.json")
  )

  def countingTheoremsForGroups : Section = Section(
    "counting-theorems-for-groups",
    List(fundamentalCountingTheorem),
    List("little-counting-theorem.t"),
    List("lct-theory.json")
  )

  def groupInterpretations : Section = Section(
    "group-interpretations",
    List(basicGroupTheory),
    List("group-to-field-interpretations.t"),
    List("fields.json")
  )

  def partialOrders : Section = Section(
    "partial-orders",
    List(foundation),
    List("partial-order.t", "real-order-properties.t", "convergence-and-order.t", "misc-convergence-and-order.t"),
    List("partial-order.json", "complete-partial-order.json", "mappings-into-a-partial-order.json")
  )

  def knasterFixedPointTheorem : Section = Section(
    "knaster-fixed-point-theorem",
    List(partialOrders),
    List("knaster-fixed-point-theorem.t", "applications.t", "schroeder-bernstein.t"),
    List("interval-in-cpo.json")
  )

  def schroederBernsteinTheorem2 : Section = Section(
    "schroeder-bernstein-theorem-2",
    List(partialOrders),
    List("schroeder-bernstein.t", "cardinality.t", "schroeder-bernstein-supplements.t"),
    List("indicator-pairs.json")
  )

  def realArithmeticExponentiation : Section = Section(
    "real-arithmetic-exponentiation",
    List(knasterFixedPointTheorem),
    List("exponentiation.t", "polynomials.t"),
    Nil
  )

  def basicMonoids : Section = Section(
    "basic-monoids",
    List(foundation),
    List("monoids.t", "monoids-supplements.t"),
    List("monoid-theory.json", "commutative-monoid-theory.json", "monoids-with-index-set.json")
  )

  def auxiliaryMonoids : Section = Section(
    "auxiliary-monoids",
    List(basicMonoids),
    List("monoids-and-cardinality.t", "monoids-examples.t"),
    Nil
  )

  def groupsAsMonoids : Section = Section(
    "groups-as-monoids",
    List(basicMonoids, basicGroupTheory),
    List("groups-as-monoids.t", "telescoping.t"),
    Nil
  )

  def basicFields : Section = Section(
    "basic-fields",
    List(foundation, preReals, basicMonoids, basicGroupTheory), //basicGroupTheory
    List("fields.t", "fields-supplements.t"),
    List("fields.json")
  )

  def binomialTheorem : Section = Section(
    "binomial-theorem",
    List(basicFields),
    List("comb-ident.t", "recursive-formulas.t"),
    Nil
  )

  def countingTheoremForSubsets : Section = Section(
    "counting-theorem-for-subsets",
    List(basicCardinality),
    List("comb-ident.t", "combinatorics.t"),
    Nil
  )

  def metricSpaces : Section = Section(
    "metric-spaces",
    List(foundation,iterate),
    List("metric-spaces.t", "metric-space-supplements.t"),
    List("metric-spaces.json", "metric-spaces-0.json", "metric-spaces-1.json", "metric-spaces-2.json")
  )

  def metricSpaceConvergence : Section = Section(
    "metric-space-convergence",
    List(partialOrders, metricSpaces),
    List("criteria-for-convergence.t"),
    Nil
  )

  def metricSpacePairs : Section = Section(
    "metric-space-pairs",
    List(metricSpaces),
    List("metric-space-pairs.t", "metric-space-pairs-supplements.t", "metric-space-self-mappings.t"),
    List("metric-spaces-2-tuples.json")
  )

  def metricSpaceSubspaces : Section = Section(
    "metric-space-subspaces",
    List(metricSpacePairs),
    List("subspaces.t"),
    List("ms-subspace.json")
  )

  def metricSpaceContinuity : Section = Section(
    "metric-space-continuity",
    List(partialOrders, metricSpacePairs),
    List("metric-space-triples.t", "examples-of-continuity.t", "examples-of-uniform-continuity.t",
                                   "mappings-from-an-interval.t", "inequalities-and-continuity.t"),
    List("metric-spaces-3-tuples.json", "mappings-from-an-interval.json", "fixed-interval-theory.json")
  )

  def mappingSpaces : Section = Section(
    "mapping-spaces",
    List(partialOrders, metricSpacePairs),
    List("pointed-metric-spaces.t", "mappings-into-pointed-metric-spaces.t", "continuous-mapping-spaces.t",
         "ptwise-continuous-mapping-spaces.t", "uniformly-continuous-mapping-spaces.t"),
    List("pointed-ms-2-tuples.json", "pointed-metric-spaces.json", "mappings-into-a-pointed-metric-space.json")
  )

  def banachFixedPointTheorem : Section = Section(
    "banach-fixed-point-theorem",
    List(partialOrders,metricSpaceSubspaces),
    List("fixed-point-theorem.t"),
    List("ms-closed-ball.json")
  )

  def abstractCalculus : Section = Section(
    "abstract-calculus",
    List(banachFixedPointTheorem, basicFields, metricSpaceContinuity),
    List("vector-spaces.t", "normed-spaces.t", "one-sided-limits.t", "derivatives.t", "integrals.t", "open-mapping.t",
      "derivatives-supplements.t"),
    List("normed-linear-spaces.json", "metric-spaces-2-tuples.json", "vector-spaces-over-rr.json", "vector-spaces.json",
      "mappings-from-an-interval.json",
    "mappings-from-an-interval-to-a-normed-space.json", "mappings-from-an-interval-with-endpoints-to-a-normed-space.json")
  )

  def machineArithmetic : Section = Section(
    "machine-arithmetic",
    Nil,
    List("machine-arithmetic.t", "gcd.t", "octets.t"),
    List("pre-octets.json", "octets.json", "machine-arithmetic.json", "h-o-real-arithmetic.json")
  )

  /* Monster Section of Basically Everything */
  def impsMathLibrary : Section = Section(
    "imps-math-library",
    List(preReals,foundation,foundationSupplements,numberTheory,machineArithmetic,calculusOverTheReals,pairs,
      sequences,binaryRelaions,iterate,advancedCardinality,schroederBernsteinTheorem1,countingTheoremsForGroups,
      groupInterpretations,realArithmeticExponentiation,auxiliaryMonoids,groupsAsMonoids,metricSpaceSubspaces,
      metricSpaceContinuity,abstractCalculus,binomialTheorem,schroederBernsteinTheorem2),
    List("quotient-structures.t", "ptwise-continuous-mapping-spaces.t", "normed-groups.t", "real-derivatives.t",
      "intermediate-value-thm.t", "more-convergence-and-order.t", "linear-order.t", "more-applications.t", "additional-arithmetic-macetes.t"),
    List("relational-theory.json", "relational-theory-2-tuples.json", "pointed-ms-2-tuples.json", "groups-alt.json",
    "normed-groups.json", "linear-order.json")
  )

  def allSections : List[Section] = List(
    pureGenericTheories,
    preReals,
    reals,
    basicRealArithmetic,
    genericTheories,
    indicators,
    mappings,
    foundation,
    foundationSupplements,
    numberTheory,
    calculusOverTheReals,
    pairs,
    sequences,
    binaryRelaions,
    iterate,
    basicCardinality,
    advancedCardinality,
    schroederBernsteinTheorem1,
    basicGroupTheory,
    fundamentalCountingTheorem,
    countingTheoremsForGroups,
    groupInterpretations,
    partialOrders,
    knasterFixedPointTheorem,
    schroederBernsteinTheorem2,
    realArithmeticExponentiation,
    basicMonoids,
    auxiliaryMonoids,
    groupsAsMonoids,
    basicFields,
    binomialTheorem,
    countingTheoremForSubsets,
    metricSpaces,
    metricSpaceConvergence,
    metricSpacePairs,
    metricSpaceSubspaces,
    metricSpaceContinuity,
    mappingSpaces,
    banachFixedPointTheorem,
    abstractCalculus,
    machineArithmetic,
    impsMathLibrary
  )
}
