package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.metadata.{MetaData, MetaDatum}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.api.{GlobalName, MPath}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaKeys
import info.kwarc.mmt.frameit.archives.MitM.Foundation.StringLiterals
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.{KnownFact, SFact}

sealed case class Scroll(problemTheory: MPath, solutionTheory: MPath, label: String, description: String, requiredFacts: List[SFact with KnownFact])

object Scroll {
  /**
    * Check if the given theory is a solution theory, if so, extract the full scroll information.
    *
    * We deliberately return an [[Either]] instead of throwing an exception (as [[KnownFact.fromConstant()]]
    * does) since we want to call [[fromTheory()]] also on theories to just test whether they are the solution
    * theory of a scroll.
 *
    * @todo rework this, we shouldn't try just because we can
    * @param thy solution theory
    */
  def fromTheory(thy: Theory)(implicit ctrl: Controller): Either[InvalidMetaData, Scroll] = {
    try {
      val name = readStringMetaDatum(thy.metadata, MetaKeys.scrollName)
      val problemThy = readMPathMetaDatum(thy.metadata, MetaKeys.problemTheory)
      val solutionThy = readMPathMetaDatum(thy.metadata, MetaKeys.solutionTheory)
      val description = readStringMetaDatum(thy.metadata, MetaKeys.scrollDescription)

      val requiredFacts = ctrl.getTheory(problemThy).getDeclarations.collect {
        case c: Constant => SFact.fromConstant(c)
      }

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