package info.kwarc.mmt.frameit.communication

import info.kwarc.mmt.api.{GlobalName, MPath}
import info.kwarc.mmt.frameit.communication.SOMDoc.{SFinalConstant, STerm}
import io.circe.generic.extras.ConfiguredJsonCodec

// vvv DO NOT REMOVE (even if IntelliJ marks it as unused)
// vvv
import info.kwarc.mmt.frameit.communication.SOMDoc.JsonConfig.jsonConfig
import info.kwarc.mmt.frameit.communication.SOMDoc.PathCodecs._

/**
  * Facts already known to MMT and communicated from MMT to the game engine
  */
@ConfiguredJsonCodec
sealed case class SFact(declaration: SFinalConstant, label: String)
@ConfiguredJsonCodec
sealed case class SFactReference(uri: GlobalName)

/**
  * Facts received by the game engine
  */
@ConfiguredJsonCodec
case class SNewFact(label: String, tp: STerm, df: Option[STerm])

@ConfiguredJsonCodec
sealed case class SScrollReference(problemTheory: MPath, solutionTheory: MPath)

/**
  * Scrolls communicated from MMT to the game engine
  */
@ConfiguredJsonCodec
sealed case class SScroll(ref: SScrollReference, label: String, description: String, requiredFacts: List[SFact])

/**
  * Tentative scroll applications communicated from the game engine to MMT
  */
@ConfiguredJsonCodec
sealed case class SScrollApplication(scroll: SScrollReference, assignments: List[(SFactReference, SOMDoc.STerm)])
