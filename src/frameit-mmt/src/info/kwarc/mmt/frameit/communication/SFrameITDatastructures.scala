package info.kwarc.mmt.frameit.communication

import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.{GlobalName, MPath}

/**
  * Facts received by the game engine
  */
sealed case class SIncomingFact(label: String, tp: Term, df: Option[Term])

sealed case class SScrollReference(problemTheory: MPath, solutionTheory: MPath)

/**
  * Tentative scroll applications communicated from the game engine to MMT
  */
sealed case class SScrollApplication(scroll: SScrollReference, assignments: List[(GlobalName, Term)])
