package info.kwarc.mmt.frameit.communication.server

// vvvvvvv CAREFUL WHEN REMOVING IMPORTS (IntelliJ might wrongly mark them as unused)
import cats.effect.IO
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.symbols.FinalConstant
import info.kwarc.mmt.api.{AddError, InvalidUnit, LocalName, presentation}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.business.datastructures.{Fact, FactReference, Scroll}
import info.kwarc.mmt.frameit.business.{DebugUtils, InvalidScroll, ViewCompletion}
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.{SDynamicScrollApplicationInfo, SFact, SScroll, SScrollApplication}
import info.kwarc.mmt.moduleexpressions.operators.NewPushoutUtils
import io.finch._
import io.finch.circe._
// ^^^^^^^ END

/**
  * All endpoints for the FrameIT MMT server.
  *
  * Ultimately these endpoints are
  *
  * - enhanced with logging and error recovery by [[ServerEndpoints]],
  * - and served by [[Server]].
  */
object ConcreteServerEndpoints extends ServerEndpoints {
  // vvvvvvv CAREFUL WHEN REMOVING IMPORTS (IntelliJ might wrongly mark them as unused)
  import info.kwarc.mmt.frameit.communication.datastructures.Codecs
  import Codecs.DataStructureCodecs._
  // ^^^^^^^ END

  private def getEndpointsForState(state: ServerState) =
    printHelp(state) :+: buildArchiveLight(state) :+: buildArchive(state) :+: reloadArchive(state) :+:
      addFact(state) :+: listFacts(state) :+: listScrolls(state) :+: applyScroll(state) :+: dynamicScroll(state) :+: printSituationTheory(state) :+: forceError

  override protected def getCompiledOverallEndpoint(state: ServerState): Endpoint.Compiled[IO] = Bootstrap.serve[Application.Json](getEndpointsForState(state)).compile

  // ENDPOINTS (all private functions)
  // ======================================
  private def printHelp(state: ServerState): Endpoint[IO, String] = get(path("help")) {
    Ok(getEndpointsForState(state).toString)
  }

  private def forceError: Endpoint[IO, Unit] = get(path("debug") :: path("forceerror")) {
    throw new Exception("A deliberate error produced by /debug/forceerror.")

    Ok(()) // unreachable anyway, but needed for typechecking
  }

  private def buildArchiveLight(state: ServerState): Endpoint[IO, Unit] = post(path("archive") :: path("build-light")) {
    state.ctrl.handleLine(s"build ${FrameWorld.archiveID} mmt-omdoc Scrolls")

    Ok(())
  }

  private def buildArchive(state: ServerState): Endpoint[IO, Unit] = post(path("archive") :: path("build")) {
    state.ctrl.handleLine(s"build ${FrameWorld.archiveID} mmt-omdoc")

    Ok(())
  }

  private def reloadArchive(state: ServerState): Endpoint[IO, Unit] = post(path("archive") :: path("reload")) {
    state.ctrl.backend.getArchive(FrameWorld.archiveID).map(frameWorldArchive => {
      val root = frameWorldArchive.root

      state.ctrl.backend.removeStore(frameWorldArchive)
      state.ctrl.addArchive(root)

      Ok(())
    }).getOrElse(NotFound(new Exception("MMT backend did not know FrameWorld archive by ID, but upon server start it did apparently (otherwise we would have aborted there). Something is inconsistent.")))
  }

  private def addFact(state: ServerState): Endpoint[IO, FactReference] = post(path("fact") :: path("add") :: jsonBody[SFact]) {
    (fact: SFact) => {
      val factConstant = fact.toFinalConstant(state.situationTheory.toTerm)

      state.synchronized {
        (if (state.doTypeChecking) state.contentValidator.checkDeclarationAgainstTheory(state.situationTheory, factConstant) else Nil) match {
          case Nil =>
            // success (i.e. no errors)
            try {
              state.ctrl.add(factConstant)
              Ok(FactReference(factConstant.path))
            } catch {
              case err: AddError =>
                NotAcceptable(err)
            }

          case errors =>
            NotAcceptable(FactValidationException(
              message = "Could not validate fact, errors were:\n\n" + errors.map {
                // for [[InvalidUnit]] also elaborate their history for better feedback
                case err: InvalidUnit => err.toString + "\n" + err.history
                case err => err
              }.mkString("\n"),
              processedFacts = List(ProcessedFactDebugInfo.fromConstant(factConstant)(state.ctrl, state.presenter))
            ))
        }
      }
    }
  }

  private def listFacts(state: ServerState): Endpoint[IO, List[SFact]] = get(path("fact") :: path("list")) {
    Ok(
      Fact
        .findAllIn(state.situationTheory, recurseOnInclusions = true)(state.ctrl)
        .map(_.renderStatic()(state.ctrl))
    )
  }

  private def printSituationTheory(state: ServerState): Endpoint[IO, String] = get(path("debug") :: path("situationtheory") :: path("print")) {
    val stringRenderer = new presentation.StringBuilder
    DebugUtils.syntaxPresentRecursively(state.situationTheory)(state.ctrl, state.presenter, stringRenderer)

    Ok(stringRenderer.get)
  }

  private def listScrolls(state: ServerState): Endpoint[IO, List[SScroll]] = get(path("scroll") :: path("list")) {
    if (!state.readScrollData) {
      // TODO hack to read latest scroll meta data, should not be needed
      //      due to https://github.com/UniFormal/MMT/issues/528
      state.ctrl.handleLine(s"build ${FrameWorld.archiveID} mmt-omdoc Scrolls/")

      state.readScrollData = true
    }

    Ok(Scroll.findAll()(state.ctrl).map(_.render()(state.ctrl)))
  }

  private def applyScroll(state: ServerState): Endpoint[IO, List[SFact]] = post(path("scroll") :: path("apply") :: jsonBody[SScrollApplication]) { (scrollApp: SScrollApplication) => {

    val scrollViewDomain = scrollApp.scroll.problemTheory
    val scrollViewCodomain = state.situationTheoryPath

    // create view out of [[SScrollApplication]]
    val scrollView = scrollApp.toView(
      state.situationDocument ? LocalName.random("frameit_scroll_view"),
      OMMOD(scrollViewCodomain)
    )(state.ctrl)

    // collect all assignments such that if typechecking later fails, we can conveniently output
    // debug information
    val scrollViewAssignments = scrollView.getDeclarations.collect {
      case c: FinalConstant => c
    }

    (if (state.doTypeChecking) state.contentValidator.checkView(scrollView) else Nil) match {
      case Nil =>
        val (situationTheoryExtension, _) = NewPushoutUtils.computeNamedPushoutAlongDirectInclusion(
          state.ctrl.getTheory(scrollViewDomain),
          state.ctrl.getTheory(scrollViewCodomain),
          state.ctrl.getTheory(scrollApp.scroll.solutionTheory),
          state.situationDocument ? LocalName.random("situation_theory_extension"),
          scrollView,
          w_to_generate = state.situationDocument ? LocalName.random("pushed_out_scroll_view")
        )(state.ctrl)

        state.setSituationTheory(situationTheoryExtension)

        Ok(
          Fact
            .findAllIn(situationTheoryExtension, recurseOnInclusions = false)(state.ctrl)
            .map(_.renderStatic()(state.ctrl))
        )

      case errors =>
        state.ctrl.delete(scrollView.path)

        NotAcceptable(FactValidationException(
          "View for scroll application does not validate, errors were:\n\n" + errors.mkString("\n"),
          scrollViewAssignments.map(d => ProcessedFactDebugInfo.fromConstant(d)(state.ctrl, state.presenter))
        ))
    }
  }}

  private def dynamicScroll(state: ServerState): Endpoint[IO, SDynamicScrollApplicationInfo] = post(path("scroll") :: path("dynamic") :: jsonBody[SScrollApplication]) { (scrollApp: SScrollApplication) =>
    Scroll.fromReference(scrollApp.scroll)(state.ctrl) match {
      case Some(scroll) =>

        val completions = List(
            ViewCompletion.closeGaps(
               scrollApp.assignments.map(asgn => (asgn._1.uri, asgn._2)),
               state.situationTheory.meta
            )(state.ctrl).map(asgn => (FactReference(asgn._1), asgn._2))
        )

        val scrollView = scrollApp.toView(
          target = state.situationDocument ? "blah",
          codomain = state.situationTheory.toTerm
        )(state.ctrl)

        val scrollAppInfo = SDynamicScrollApplicationInfo(
          original = scroll.render(None)(state.ctrl),
          rendered = scroll.render(Some(scrollView))(state.ctrl),
          completions = completions
        )

        Ok(scrollAppInfo)

      case _ =>
        NotFound(InvalidScroll("Scroll not found or (meta)data invalid"))
    }}
}