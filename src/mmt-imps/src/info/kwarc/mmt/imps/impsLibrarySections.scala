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

  def reals : Section = Section(
    "reals",
    Nil,
    List("reals.t", "some-elementary-macetes.t", "arithmetic-strategies.t"),
    List("h-o-real-arithmetic.json")
  )

  def basicRealArithmetic : Section = Section(
    "basic-real-arithmetic",
    List("reals"),
    List("arithmetic-macetes.t", "some-lemmas.t", "number-theory.t"),
    Nil
  )

  def genericTheories : Section = Section(
    "generic-theories",
    List("basic-real-arithmetic", "pure-generic-theories"),
    List("generic-theories.t"),
    List("generic-theory-1.json", "generic-theory-2.json",
         "generic-theory-3.json", "generic-theory-4.json")
  )

  def indicators : Section = Section(
    "indicators",
    Nil,
    List("indicators.t", "indicator-lemmas.t"),
    List("indicators.json", "family-indicators.json")
  )

  def mappings : Section = Section(
    "mappings",
    List("indicators", "pure-generic-theories"),
    List("mappings.t", "mapping-lemmas.t", "inverse-lemmas.t"),
    Nil
  )

  def foundation : Section = Section(
    "foundation",
    List("generic-theories", "mappings"),
    List("pure-generic-theories-with-subsorts.t", "reals-supplements.t", "some-obligations.t"),
    List("pure-generic-theory-1-with-1-subsort.json", "pure-generic-theory-2-with-1-subsort.json",
         "pure-generic-theory-2-with-2-subsorts.json", "pure-generic-theory-2-with-2-subsorts.json")
  )

  def allSections : List[Section] = List(
    pureGenericTheories,
    reals,
    basicRealArithmetic,
    genericTheories,
    indicators,
    mappings,
    foundation
  )

}
