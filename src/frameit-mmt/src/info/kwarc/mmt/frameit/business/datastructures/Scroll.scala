package info.kwarc.mmt.frameit.business.datastructures

import info.kwarc.mmt.api.{GetError, MPath, Path, StructuralElement}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.{Context, Term}
import info.kwarc.mmt.api.ontology.IsTheory
import info.kwarc.mmt.api.symbols.ApplyMorphism
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations.MetaKeys
import info.kwarc.mmt.frameit.business.{InvalidMetaData, Utils}
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
  def renderDynamicScroll(viewRenderer: ScrollViewRenderer)(implicit ctrl: Controller): Scroll = this.copy(
    meta = meta.render(viewRenderer),
    requiredFacts = requiredFacts.map(_.renderDynamicFact(viewRenderer))
  )

  def render(view: Option[View] = None)(implicit ctrl: Controller): SScroll = {
    val scroll = view match {
      case Some(view) =>
        renderDynamicScroll(new StandardViewRenderer(view)(ctrl))
      case _ => this
    }

    SScroll(
      scroll.ref,
      scroll.meta.label.toStr(true),
      scroll.meta.description.toStr(true),
      scroll.requiredFacts.map(_.renderStatic())
    )
  }
}

object Scroll {
  def fromReference(ref: ScrollReference)(implicit ctrl: Controller): Option[Scroll] = {
    Utils.getAsO(classOf[Theory], ref.solutionTheory)(ctrl.globalLookup).flatMap(tryParseAsScroll)
  }

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