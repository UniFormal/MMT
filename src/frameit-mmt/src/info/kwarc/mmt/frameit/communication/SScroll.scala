package info.kwarc.mmt.frameit.communication

sealed case class SScroll(problemTheory: SimpleOMDoc.SURI, solutionTheory: SimpleOMDoc.SURI, label: String, description: String, requiredFacts: List[SFact])

sealed case class SScrollApplication(scroll: SScroll, view: String)
