package info.kwarc.mmt.frameit.business.datastructures

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.OMSReplacer
import info.kwarc.mmt.api.objects.{Context, OMS, OMMOD, Term}
import info.kwarc.mmt.api.ontology.RelationExp.Imports
import info.kwarc.mmt.api.ontology.{HasType, IsTheory}
import info.kwarc.mmt.api.symbols.PlainInclude
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api.{GlobalName, GetError, MPath, RuleSet, LocalName}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld.MetaAnnotations.MetaKeys
import info.kwarc.mmt.frameit.archives.LabelVerbalizationRule
import info.kwarc.mmt.frameit.business.{InvalidMetaData, Utils}
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.SScroll

/**
  * A reference to a scroll -- without accompanying information.
  */
sealed case class ScrollReference(declaringTheory: MPath)
private[business] sealed case class ElaboratedScrollReference(declaringTheory: MPath, problemTheory: MPath, solutionTheory: MPath) {
  def toSimple: ScrollReference = ScrollReference(declaringTheory)
}

/**
  * A possibly non-total scroll application from a scroll to a situation theory.
  *
  * @param ref the scroll
  * @param situationTheory the situation theory
  * @param assignments The assignments for the scroll application. This may represent an [[View MMT view]]
  *                    (i.e. a total one) from [[ref.problemTheory]] to [[situationTheory]], but does not
  *                    need to. In the extreme case, it can also be [[Map.empty]].
  */
sealed case class ScrollApplication(ref: ElaboratedScrollReference, situationTheory: MPath, assignments: Map[GlobalName, Term])

/**
  * Scrolls
  */
sealed case class Scroll(
                          ref: ElaboratedScrollReference,
                          meta: UserMetadata,
                          requiredFacts: List[Fact],
                          acquiredFacts: List[Fact]
                        ) {
  def toSimple(implicit ctrl: Controller): SScroll = SScroll(
    ref.toSimple,
    meta.label.toStr(true),
    meta.description.toStr(true),
    requiredFacts.map(_.toSimple),
    acquiredFacts.map(_.toSimple)
  )

  /**
    * Utility method to first [[render()]] and then convert [[toSimple]].
    */
  def renderSimple(scrollApp: Option[ScrollApplication] = None)(implicit ctrl: Controller): SScroll = render(scrollApp).toSimple

  /**
    * Render a dynamic scroll, possibly along a [[ScrollApplication]].
    *
    * Rendering entails verbalization of labels and descriptions of the scroll itself, and all required, and
    * acquired facts.
    *
    * Moreover, if a (possibly non-total) [[ScrollApplication]] is given, its assignments are first used
    * to translate the current scroll *before* verbalization.
    * This has the effect that the returned scroll contains labels and descriptions interpolated with
    * those from the target of the scroll application (the situation theory).
    *
    * @param scrollApp An optional scroll application to take into account for translation performed
    *                  prior to verbalization.
    */
  def render(scrollApp: Option[ScrollApplication] = None)(implicit ctrl: Controller): Scroll = {
    val translatedScroll = scrollApp.map(translateScroll).getOrElse(this)

    val verbalizationContext = scrollApp match {
      // In any case, we need the solution theory in context in order
      // to be able to simplify acquiredFacts, too.
      //
      // In case of a provided scroll application, facts in requiredFacts and
      // acquiredFacts may (actually, are encouraged to) reference symbols in
      // the situationTheory.
      //
      // Note: including the solutionTheory in the context also covers the (allowed)
      //       case of non-total scroll applications where even the requiredFacts in
      //       translatedScroll may reference symbols in both the solutionTheory and
      //       the situationTheory.
      case Some(app) => Context(ref.solutionTheory) ++ app.situationTheory
      case None => Context(ref.solutionTheory)
    }

    translatedScroll.verbalizeScroll(verbalizationContext)
  }

  /**
    * Translates a scroll along a (possibly non-total) [[ScrollApplication]].
    *
    * - Meta data (labels, descriptions) of the scroll itself, and types and definitions of all
    *   facts are homomorphically translated.
    * - Meta data of facts are treated specially:
    *
    *   If the scroll application assigns something to a fact ''f'', then the new meta data
    *   of ''f'' becomes the verbalization of the assigned expression.
    *   You have to still call [[verbalizeScroll()]] on the returned scroll to actually turn
    *   the modified verbalization into strings.
    *
    *   Otherwise, if ''f'' is unassigned, then its original meta data is kept.
    */
  private def translateScroll(scrollApp: ScrollApplication)(implicit ctrl: Controller): Scroll = {
    val replacer = new OMSReplacer {
      override def replace(p: GlobalName): Option[Term] = scrollApp.assignments.get(p)
    }

    // the context needs to be the solution theory as we also translate acquiredFacts below
    def termTranslator(term: Term): Term = replacer(term, Context(scrollApp.ref.solutionTheory))

    def factTranslator(fact: Fact): Fact = {
      val newMetaData = (if (scrollApp.assignments.contains(fact.ref.uri)) {
        // If the scroll application contained an assignment, the rendered fact should have the metadata
        // of the assigned fact (possibly a fact expression even).
        //
        // Hence, we set up the new meta data such that after verbalization it will render
        // precisely as desired in the last sentence.
        val assignedFact = termTranslator(OMS(fact.ref.uri))
        UserMetadata(
          label = MetaAnnotations.LabelVerbalization(assignedFact),
          description = MetaAnnotations.DescriptionVerbalization(assignedFact)
        )
      } else {
        // Otherwise, if the scroll application did *not* map our fact, we should still
        // translate its metadata. For instance, our fact's description might be the MMT term
        //
        //  s"Angle between ${labelVerbalization A B C}".
        //
        // If the scroll now *did* map A, B, C, the new metadata for our fact should render the
        // individual metadata for A, B, C within the term above, too.

        fact.meta.map(termTranslator)
      })

      fact.copy(
        meta = newMetaData,
        tp = termTranslator(fact.tp),
        df = fact.df.map(termTranslator)
      )
    }

    this.copy(
      meta = meta.map(termTranslator),
      requiredFacts = requiredFacts.map(factTranslator),
      acquiredFacts = acquiredFacts.map(factTranslator),
    )
  }

  /**
    * Verbalizes the scroll, by simplifying using the [[LabelVerbalizationRule]].
    *
    * In the ideal case, the labels and descriptions of the returned scroll and all of its facts
    * are simple terms representing strings (i.e. [[StringLiterals]]). That is, concatenation
    * or [[LabelVerbalization]]s do not occur anymore in those terms.
    *
    * @param context The context in which the facts live. Needs to include at least [[ref.solutionTheory]].
    */
  private def verbalizeScroll(context: Context)(implicit ctrl: Controller): Scroll = {
    val simplificationUnit = SimplificationUnit(context, expandDefinitions = false, fullRecursion = true)

    val ruleSet: RuleSet = {
      val foundRules = RuleSet.collectRules(ctrl, simplificationUnit.context)
      foundRules.add(new LabelVerbalizationRule()(ctrl.globalLookup))

      foundRules
    }

    def verbalize(t: Term): Term = {
      ctrl.simplifier(t, simplificationUnit, ruleSet)
    }

    def verbalizeFact(fact: Fact): Fact = fact.copy(
      meta = UserMetadata(
        label = verbalize(fact.meta.label),
        description = verbalize(fact.meta.description)
      )
    )

    this.copy(
      meta = meta.map(verbalize),
      requiredFacts = requiredFacts.map(verbalizeFact),
      acquiredFacts = acquiredFacts.map(verbalizeFact)
    )
  }
}

object Scroll {
  def fromReference(ref: ScrollReference)(implicit ctrl: Controller): Option[Scroll] = {
    Utils.getAsO(classOf[Theory], ref.declaringTheory)(ctrl.globalLookup).flatMap(tryParseAsScroll)
  }

  /**
    * Finds all scrolls known to [[Controller.depstore]].
    *
    * Using the depstore implies that only scrolls are found and returned that are built
    * to mmt-omdoc.
    *
    * @return A list of all known scrolls (no duplicates).
    */
  def findAll()(implicit ctrl: Controller): List[Scroll] = {
    ctrl.depstore
      .getInds(IsTheory)
      .collect {
        case mpath: MPath => ctrl.getTheory(mpath)
      }
      .distinct
      .flatMap(tryParseAsScroll(_))
      .toList
  }

  /**
    * Finds all scrolls in scope (i.e., transitively included) in `theory`.
    * @param theory Usually the current situation theory from which you would like to infer
    *               the accessible scrolls.
    *               Neither this theory nor any transitively included theories need to be
    *               built to mmt-omdoc before calling this function.
    *               (This is in contrast to [[findAll()]].)
    * @return A list of all scrolls in scope in `theory` (no duplicates).
    */
  def findIncludedIn(theory: Theory)(implicit ctrl: Controller): List[Scroll] = {
    theory.getIncludesWithoutMeta.map(ctrl.getTheory).flatMap(t => tryParseAsScroll(t) match {
      case Some(scroll) => List(scroll)
      case None => findIncludedIn(t)
    }).distinct

    // If everything was built mmt-omdoc (and known to ctrl.depstore), we could do the above a bit more elegantly:
    /* ctrl.depstore
      .querySet(theory.path, (Imports^*) * HasType(IsTheory))
      .collect { case mpath: MPath => ctrl.getTheory(mpath) }
      .flatMap(tryParseAsScroll(_))
      .toList
    */
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
      val problemPath = MetadataUtils.readMPathMetaDatum(thy.metadata, MetaKeys.problemTheory)
      val solutionPath = MetadataUtils.readMPathMetaDatum(thy.metadata, MetaKeys.solutionTheory)

      val problemTheory = ctrl.getTheory(problemPath)
      val solutionTheory = ctrl.getTheory(solutionPath)

      val scrollRef = ElaboratedScrollReference(thy.path, problemPath, solutionPath)

      Some(Scroll(
        scrollRef,
        UserMetadata.parse(solutionTheory),
        Fact.findAllIn(problemTheory, recurseOnInclusions = true),

        // todo: here `recurseOnInclusions = false` will prevent modular solution theories
        //       we need to disallow recursion, though, as to not recurse into the problem theory!
        Fact.findAllIn(solutionTheory, recurseOnInclusions = false)
      ))
    } catch {
      case _: InvalidMetaData => None
      case err: GetError =>
        err.printStackTrace()
        None
    }
  }
}