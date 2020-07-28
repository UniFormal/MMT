package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.metadata.{MetaData, MetaDatum}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.{GlobalName, MPath}
import info.kwarc.mmt.frameit.archives.Foundation.StringLiterals
import info.kwarc.mmt.frameit.communication.{SFact, SOMDoc, SScroll, SScrollReference}

sealed case class Scroll(problemTheory: MPath, solutionTheory: MPath, label: String, description: String, requiredFacts: List[Fact]) {
  def simplified: SScroll = SScroll(
    SScrollReference(problemTheory, solutionTheory),
    label,
    description,
    requiredFacts.map(_.simplified)
  )
}

final case class InvalidMetaData(private val message: String = "",
                                 private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

object Scroll {
  /**
    * Check if the given theory is a solution theory, if so, extract the full scroll information.
    * @param thy solution theory
    * @todo eliminate dependence on controller, needed to look up problem theory
    */
  def fromTheory(thy: Theory)(implicit lookup: Lookup): Either[InvalidMetaData, Scroll] = {
    import info.kwarc.mmt.frameit.archives.Archives.FrameWorld.MetaKeys

    try {
      val name = readStringMetaDatum(thy.metadata, MetaKeys.scrollName)
      val problemThy = readMPathMetaDatum(thy.metadata, MetaKeys.problemTheory)
      val solutionThy = readMPathMetaDatum(thy.metadata, MetaKeys.solutionTheory)
      val description = readStringMetaDatum(thy.metadata, MetaKeys.scrollDescription)

      val requiredFacts = lookup.getTheory(problemThy)
        .getDeclarations
        // enrich with fact labels
        .map(Fact.parseFromDeclaration)

      Right(Scroll(problemThy, solutionThy, name, description, requiredFacts))
    } catch {
      case err: InvalidMetaData => Left(err)
    }
  }

  private def readSingleMetaDatum(metadata: MetaData, key: GlobalName): MetaDatum = metadata.get(key) match {
    case Nil => throw InvalidMetaData(s"No metadatum found for ${key} (expected exactly one matching metadatum entry)")
    case List(datum) => datum
    case _ => throw InvalidMetaData(s"Multiple metadatums matching for ${key} (expected exactly one matching metadatum entry)")
  }

  private def readMPathMetaDatum(metadata: MetaData, key: GlobalName): MPath = readSingleMetaDatum(metadata, key).value match {
    case OMMOD(mpath) => mpath
    case x => throw InvalidMetaData(s"Expected MPath matching for ${key}, but got ${x}")
  }

  private def readStringMetaDatum(metadata: MetaData, key: GlobalName): String = readSingleMetaDatum(metadata, key).value match {
    case StringLiterals(str) => str
    case x => throw InvalidMetaData(s"Expected string literal matching for ${key}, but got ${x}")
  }
}