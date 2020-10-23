package info.kwarc.mmt.frameit.business.datastructures

import info.kwarc.mmt.api.MPath
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.ontology.IsTheory
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaKeys
import info.kwarc.mmt.frameit.business.InvalidMetaData
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.SScroll

/**
  * A reference to a scroll -- without accompanying information.
  */
sealed case class ScrollReference(problemTheory: MPath, solutionTheory: MPath)

sealed case class Scroll(
                          ref: ScrollReference,
                          meta: UserMetadata,
                          requiredFacts: List[Fact]
                        ) {
  def renderNaive()(implicit ctrl: Controller): SScroll = {
    SScroll(
      ref,
      meta.label.toStr(true),
      meta.description.toStr(true),
      requiredFacts.map(_.render())
    )
  }
}

object Scroll {
  def findAll()(implicit ctrl: Controller): List[Scroll] = {
    val allTheories: Seq[Theory] = ctrl.depstore
      .getInds(IsTheory)
      .map(_.asInstanceOf[MPath])
      .flatMap(path => ctrl.getO(path) match {
        case Some(t) if t.isInstanceOf[Theory] => Some(t.asInstanceOf[Theory])
        case _ => None
      }).toSeq

    allTheories.flatMap(tryParseAsScroll(_)).toList
  }

  /**
    * Check if the given theory is a solution theory, if so, extract the full scroll information.
    *
    * We deliberately return an [[Either]] instead of throwing an exception since we want to call
    * [[fromTheory()]] also on theories to just test whether they are the solution theory of a scroll.
    *
    * @todo rework this, we shouldn't try just because we can
    * @todo actually we don't require thy to be the solution theory, reread my own code, please :-)
    * @param thy solution theory
    */
  private def tryParseAsScroll(thy: Theory)(implicit ctrl: Controller): Option[Scroll] = {
    try {
      val problemThy = MetadataUtils.readMPathMetaDatum(thy.metadata, MetaKeys.problemTheory)
      val solutionThy = MetadataUtils.readMPathMetaDatum(thy.metadata, MetaKeys.solutionTheory)

      var scrollRef = ScrollReference(problemThy, solutionThy)

      Some(Scroll(
        scrollRef,
        UserMetadata.parse(thy),
        Fact.findAllIn(ctrl.getTheory(problemThy), recurseOnInclusions = true)
      ))
    } catch {
      case _: InvalidMetaData => None

        // really catch all [[Throwable]]s for the best debug experience
      case err : Throwable => throw err
    }
  }
}