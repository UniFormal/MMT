package info.kwarc.mmt.frameit.business.datastructures

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.ontology.IsTheory
import info.kwarc.mmt.api.ontology.QMTProp.And
import info.kwarc.mmt.api.{LocalName, MPath}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations.MetaKeys
import info.kwarc.mmt.frameit.business.{InvalidMetaData, Utils}
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.SScroll

import scala.collection.mutable
import scala.util.Random

/**
  * A reference to a scroll -- without accompanying information.
  */
sealed case class ScrollReference(problemTheory: MPath, solutionTheory: MPath)

/**
  * Scrolls
  */
sealed case class Scroll(
                          ref: ScrollReference,
                          meta: UserMetadata,
                          requiredFacts: List[Fact],
                          acquiredFacts: List[Fact]
                        ) {
  def renderDynamicScroll(viewRenderer: ScrollViewRenderer)(implicit ctrl: Controller): Scroll = this.copy(
    meta = meta.render(viewRenderer),
    requiredFacts = requiredFacts.map(_.renderDynamicFact(viewRenderer)),
    acquiredFacts = acquiredFacts.map(_.renderDynamicFact(viewRenderer))
  )

  /**
    * Render dynamic scroll to a simplified [[SScroll]] for transmission to the game engine.
    *
    * @param view An optional view with its domain being the scroll's problem theory (i.e. [[ref.problemTheory]]).
    *             The view does *not* need to be total, not even partial (implying some "respect" to dependency
    *             relations). In other words, it may lack assignments for arbitrary declarations of the problem
    *             theory. Let's call it "utterly partial".
    *
    *             In fact, if [[None]] is given, an empty view from [[ref.problemTheory]] to itself will be constructed
    *             and used instead.
    *
    *             The returned [[SScroll]] is computed from ''this'' scroll by applying the view
    *
    *             - homomorphically on the scroll's [[UserMetadata metadata]] (such as labels, descriptions)
    *             - homomorphically on the type, definiens of every required and acquired fact.
    *             - and as follows on the [[UserMetadata metadata]] of every required and acquired fact:
    *               if the view contains an assignment for a fact, the metadata of the new fact (i.e.
    *               the one appearing in the return value of this function) is the one of the assigned
    *               expression. (todo this fails for complex assignments, no?)
    *               Otherwise, if the view did *not* contain an assignment, then the view is applied
    *               homomorphically on the metadata.
    *               Doing so instead of naive copying is useful for unassigned-to facts whose labels
    *               interpolate labels of already-assigned-to facts.
    *
    *
    *             Since the view may be utterly partial, we have to be precise what it means to apply it
    *             homomorphically: the action on complex terms is as usual (homomorphic). The result on
    *             ''OMID(path)'' is ''t'' if the view contained the assignment ''path := t''. Otherwise,
    *             if the symbol referred to by path was defined, then the homomorphic action on its definiens
    *             is chosen. Finally, if it not even had a definiens, it is left as-is.
    *             It is the last case where we deviate from the usual definitions.
    *
    *             Note that MMT probably assumed at several places that views are at least partial.
    *             Moreover, this makes the whole implementation a bit hacky.
    *
    * @param ctrl A controller instance used for applying the view homomorphically and doing simplification.
    */
  def render(view: Option[View] = None)(implicit ctrl: Controller): SScroll = {
    val renderedScroll = view match {
      case Some(v) => renderDynamicScroll(new StandardViewRenderer(v)(ctrl))
      case None =>
        val emptyView = View(
          doc = ref.problemTheory.doc,
          name = LocalName.random("empty_view_for_scroll_rendering"),
          from = OMMOD(ref.problemTheory),
          to = OMMOD(ref.problemTheory),
          isImplicit = false
        )

        ctrl.add(emptyView)
        val renderedScroll = renderDynamicScroll(new StandardViewRenderer(emptyView)(ctrl))
        ctrl.delete(emptyView.path)

        renderedScroll
    }

    SScroll(
      renderedScroll.ref,
      renderedScroll.meta.label.toStr(true),
      renderedScroll.meta.description.toStr(true),
      renderedScroll.requiredFacts.map(_.renderStatic()),
      renderedScroll.acquiredFacts.map(_.renderStatic())
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
  /*
  def findIncludedIn(theory: Theory)(implicit ctrl: Controller): List[Scroll] = {
    ctrl.depstore.getInds(And(IsTheory, IsTheory))
    def getAllIncludes(mpath: MPath): Set[MPath] = {
      val seen = mutable.HashSet[MPath]()
      val queue = mutable.Queue[MPath]()
      while (queue.nonEmpty) {
        val path = queue.dequeue()

        ctrl.getTheory(path).getIncludesWithoutMeta
      }
      val s: Set[MPath] = (ctrl.getTheory(mpath).getIncludesWithoutMeta.toSet - visited)
      s
    }
  }*/

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
        Fact.findAllIn(ctrl.getTheory(problemThy), recurseOnInclusions = true),

        // todo: this will not pick up acquired facts that have been included
        //       we need to disallow recursion, though, as to not recurse into the problem theory!
        Fact.findAllIn(ctrl.getTheory(solutionThy), recurseOnInclusions = false)
      ))
    } catch {
      case _: InvalidMetaData => None

        // really catch all [[Throwable]]s for the best debug experience
      case err : Throwable => throw err
    }
  }
}