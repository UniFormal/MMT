package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.metadata.{MetaData, MetaDatum}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.{GlobalName, MPath}
import info.kwarc.mmt.frameit.archives.Foundation.StringLiterals

case class Scroll(problemTheory: MPath, solutionTheory: MPath, label: String, description: String, declarations: List[Declaration])

final case class InvalidMetaData(private val message: String = "",
                                 private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

object Scroll {
  /**
    * Check if the given theory is a solution theory, if so, extract the full scroll information.
    * @param thy solution theory
    * @todo eliminate dependence on controller, needed to look up problem theory
    */
  def fromTheory(thy: Theory)(implicit lookup: Lookup): Option[Scroll] = {
    import info.kwarc.mmt.frameit.archives.Archives.FrameWorld.ScrollKeys

    try {
      val name = readStringMetaDatum(thy.metadata, ScrollKeys.name)
      val problemThy = readMPathMetaDatum(thy.metadata, ScrollKeys.problemTheory)
      val solutionThy = readMPathMetaDatum(thy.metadata, ScrollKeys.solutionTheory)
      val description = readStringMetaDatum(thy.metadata, ScrollKeys.description)

      Some(Scroll(problemThy, solutionThy, name, description, lookup.getTheory(problemThy).getDeclarations))
    } catch {
      case _: InvalidMetaData => None
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

  //private def readMPathMetaDatum(metadatums: List[MetaDatum]): URI = Path.parseM(readURIMetaDatum(metadatums), NamespaceMap.empty)

  private def readStringMetaDatum(metadata: MetaData, key: GlobalName): String = readSingleMetaDatum(metadata, key).value match {
    case StringLiterals(str) => str
    case x => throw InvalidMetaData(s"Expected string literal matching for ${key}, but got ${x}")
  }
}