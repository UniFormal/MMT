package info.kwarc.mmt.frameit.communication.server

// vvvvvvv CAREFUL WHEN REMOVING IMPORTS (IntelliJ might wrongly mark them as unused)
import cats.effect.IO
import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.View
import info.kwarc.mmt.api.objects.{Context, OMMOD}
import info.kwarc.mmt.api.symbols.{FinalConstant, NestedModule}
import info.kwarc.mmt.api.{AddError, InvalidUnit, LocalName, Path, presentation}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.business.datastructures.{Fact, FactReference, Scroll, ScrollApplication}
import info.kwarc.mmt.frameit.business.{DebugUtils, InvalidScroll, Utils, ViewCompletion}
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.{SCheckingError, SDynamicScrollInfo, SFact, SScroll, SScrollApplication, SScrollApplicationResult, SScrollAssignments}
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
  import Codecs.MiscCodecs._
  import ServerErrorHandler._
  // ^^^^^^^ END

  /**
    * Aggregates all endpoints whose output should be encoded to HTTP responses with [[Application.Json]] bodies.
    *
    * Since JSON is the default encoding in the frameit-mmt project, almost all endpoints should be aggregated by
    * this function. Only some debugging endpoints might go to [[getPlaintextEndpointsForState()]].
    */
  private def getJSONEndpointsForState(state: ServerState) =
      addFact(state) :+: listFacts(state) :+: listAllScrolls(state) :+: listScrolls(state) :+:
      applyScroll(state) :+: dynamicScroll(state) :+:
      // meta/debug endpoints:
      buildArchiveLight(state) :+: buildArchive(state) :+: reloadArchive(state) :+: forceError :+:
    checkSituationSpace(state)

  /**
    * Aggregates endpoints whose output should be encoded to HTTP responses with [[Text.Plain]] bodies.
    *
    * Since JSON is the default encoding in the frameit-mmt project, only some debugging endpoints should be
    * aggregated here.
    */
  private def getPlaintextEndpointsForState(state: ServerState) = printSituationTheory(state)

  override protected def getCompiledOverallEndpoint(state: ServerState): Endpoint.Compiled[IO] = {
    Bootstrap
      .serve[Application.Json](getJSONEndpointsForState(state))
      .serve[Text.Plain](getPlaintextEndpointsForState(state))
      .compile
  }

  // ENDPOINTS (all private functions)
  // ======================================

  // META/DEBUG ENDPOINTS
  // ---------------------------------------
  private def forceError: Endpoint[IO, Unit] = get(path("debug") :: path("forceerror")) {
    throw new Exception("A deliberate error produced by /debug/forceerror.")

    Ok(()) // unreachable anyway, but needed for typechecking
  }

  private def checkSituationSpace(state: ServerState): Endpoint[IO, List[api.Error]] = get(path("debug") :: path("space") :: path("check")) {
    Ok(state.contentValidator.checkTheory(state.situationSpace))
  }

  private def printSituationTheory(state: ServerState): Endpoint[IO, String] = get(path("debug") :: path("space") :: path("print")) {
    val stringRenderer = new presentation.StringBuilder
    state.presenter(state.situationSpace)(stringRenderer)

    Ok(stringRenderer.get)
  }
  // ---------------------------------------

  // REAL ENDPOINTS FOR USE BY GAME ENGINE
  // --------------------------------------- (up to the end of the file)
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
      val factConstant = fact.toFinalConstant(state.newFactPath())

      state.synchronized {
        // todo: bug in checkDeclarationAgainstTheory
        // state.contentValidator.checkDeclarationAgainstTheory(state.situationTheory, factConstant) match {
        List[Error]() match {
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
        .map(_.toSimple(state.ctrl))
    )
  }

  private def listAllScrolls(state: ServerState): Endpoint[IO, List[SScroll]] = get(path("scroll") :: path("listall")) {
    Ok(Scroll.findAll()(state.ctrl).map(_.renderSimple()(state.ctrl)))
  }

  private def listScrolls(state: ServerState): Endpoint[IO, List[SScroll]] = get(path("scroll") :: path("list")) {
    Ok(Scroll.findIncludedIn(state.situationTheory)(state.ctrl).map(_.renderSimple()(state.ctrl)))
  }

  private def applyScroll(state: ServerState): Endpoint[IO, SScrollApplicationResult] = post(path("scroll") :: path("apply") :: jsonBody[SScrollApplication]) { (scrollApp: SScrollApplication) =>
    Scroll.fromReference(scrollApp.scroll)(state.ctrl) match {
      case Some(scroll) =>
        implicit val ctrl: Controller = state.ctrl
        implicit val _state: ServerState = state

        val (scrollView, scrollViewPaths, errors) = prepScrollApplication(scrollApp)

        try {
          if (errors.isEmpty) {
            state.descendSituationTheory(LocalName.random("after_scroll_application"))

            val viewToGenerate: View = {
              val path = state.situationTheory.path / LocalName.random("pushed_out_scroll_view")

              val view = View(
                path.doc,
                path.name,
                from = OMMOD(scroll.ref.solutionTheory),
                to = state.situationTheory.toTerm,
                isImplicit = false
              )
              Utils.addModuleToController(view)

              view
            }

            NewPushoutUtils.injectPushoutAlongDirectInclusion(
              state.ctrl.getTheory(scrollView.from.toMPath),
              state.ctrl.getTheory(scrollView.to.toMPath),
              state.ctrl.getTheory(scroll.ref.solutionTheory),
              state.situationTheory,
              scrollView,
              viewToGenerate
            )(state.ctrl)

            Ok(SScrollApplicationResult.success(Fact
                .findAllIn(state.situationTheory, recurseOnInclusions = false)(state.ctrl)
                .map(_.toSimple(state.ctrl))
            ))
          } else {
            Ok(SScrollApplicationResult.failure(errors))
          }
        } finally {
          if (errors.nonEmpty) {
            scrollViewPaths.foreach(ctrl.delete)
          }
        }

      case _ =>
        NotFound(InvalidScroll("Scroll not found or (meta)data invalid"))
    }}

  private def prepScrollApplication(scrollApp: SScrollApplication)(implicit state: ServerState): (View, List[Path], List[SCheckingError]) = {
    // the scroll view and all paths (for later deletion from [[Controller]])
    val (scrollView, scrollViewPaths) = {
      val scrollViewName = LocalName.random("scroll_view")
      val view = scrollApp.toView(
        target = state.situationTheory.path / scrollViewName,
        codomain = state.situationTheory.toTerm
      )(state.ctrl)
      val paths = List(state.situationTheory.path ? scrollViewName, state.situationTheory.path / scrollViewName)

      (view, paths)
    }
    val errors = state.contentValidator.checkScrollView(scrollView, scrollApp.assignments)

    (scrollView, scrollViewPaths, errors)
  }

  private def getCompletions(scrollApp: SScrollApplication)(implicit state: ServerState): List[SScrollAssignments] = {
    val canonicalCompletion = ViewCompletion.closeGaps(
      scrollApp.assignments.toMMTList,
      state.situationTheory.meta
    )(state.ctrl)

    if (canonicalCompletion.isEmpty) {
      Nil
    } else {
      List(SScrollAssignments.fromMMTList(canonicalCompletion))
    }
  }

  private def dynamicScroll(state: ServerState): Endpoint[IO, SDynamicScrollInfo] = post(path("scroll") :: path("dynamic") :: jsonBody[SScrollApplication]) { (scrollApp: SScrollApplication) =>
    Scroll.fromReference(scrollApp.scroll)(state.ctrl) match {
      case Some(scroll) =>
        implicit val ctrl: Controller = state.ctrl
        implicit val _state: ServerState = state

        val (_, scrollViewPaths, errors) = prepScrollApplication(scrollApp)
        val completions = getCompletions(scrollApp)

        try {
          val scrollAppInfo = SDynamicScrollInfo(
            original = scroll.renderSimple(),
            rendered = scroll.renderSimple(Some(ScrollApplication(
              scroll.ref,
              state.situationTheory.path,
              scrollApp.assignments.toMMTMap
            ))),
            completions = completions,
            valid = errors.isEmpty,
            errors = errors
          )
          Ok(scrollAppInfo)
        } finally {
          scrollViewPaths.foreach(ctrl.delete)
        }

      case _ =>
        NotFound(InvalidScroll("Scroll not found or (meta)data invalid"))
    }}
}