package info.kwarc.mmt.frameit.business.datastructures

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.ontology.{HasType, IsTheory}
import info.kwarc.mmt.api.ontology.QMTProp.And
import info.kwarc.mmt.api.ontology.RelationExp.Imports
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
    ctrl.depstore
      .getInds(IsTheory)
      .collect {
        case mpath: MPath => ctrl.getTheory(mpath)
      }
      .flatMap(tryParseAsScroll(_))
      .toList
  }

  def findIncludedIn(theory: Theory)(implicit ctrl: Controller): List[Scroll] = {
    ctrl.depstore
      .querySet(theory.path, (Imports^*) * HasType(IsTheory))
      .collect {
        case mpath: MPath => ctrl.getTheory(mpath)
      }
      .flatMap(tryParseAsScroll(_))
      .toList
  }

  /**
    * Tries to parse a theory as a "scroll theory".
    *
    * A scroll theory is a [[Theory]] with two special metadatums: [[MetaKeys.problemTheory]] and [[MetaKeys.solutionTheory]] referencing the problem and the solution theories via their [[MPath MPaths]].
    *
    * The scroll theory may be completely independent of the problem and solution theories, act as its own problem theory, or act as its own solution theory. Important is just that it has those meta datums.
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

        // todo: here `recurseOnInclusions = false` will prevent modular solution theories
        //       we need to disallow recursion, though, as to not recurse into the problem theory!
        Fact.findAllIn(ctrl.getTheory(solutionThy), recurseOnInclusions = false)
      ))
    } catch {
      case _: InvalidMetaData => None
    }
  }
}