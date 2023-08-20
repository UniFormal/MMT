package info.kwarc.mmt.frameit.communication.server

// vvvvvvv CAREFUL WHEN REMOVING IMPORTS (IntelliJ might wrongly mark them as unused)
import cats.effect
import cats.effect.IO
import com.twitter.finagle.{ListeningServer, Service}
import com.twitter.finagle.http.{Request, Response}
import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.View
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.{AddError, Before, InvalidUnit, LocalName, Path}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.business.datastructures.{Fact, FactReference, Scroll, ScrollApplication}
import info.kwarc.mmt.frameit.business.{InvalidScroll, Pushout, Utils, ViewCompletion}
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures.{SCheckingError, SDynamicScrollInfo, SFact, SScroll, SScrollApplication, SScrollApplicationResult, SScrollAssignments}
import io.finch._
import io.finch.circe._

import java.nio.charset.{Charset, StandardCharsets}
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
  private def getJSONEndpointsForState(state: LobbyState) =
      addFact(state) :+: bulkaddFacts(state) :+: listFacts(state) :+: listAllScrolls(state) :+: listScrolls(state) :+:
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
  private def getPlaintextEndpointsForState(state: LobbyState) = printSituationTheory(state)

  override def createServer(state: LobbyState, address: String): effect.Resource[IO, ListeningServer] = {
    def asUTF8[T](endpoint: Endpoint[IO, T]): Endpoint[IO, T] = {
      endpoint.transformOutput(_.map(_.withCharset(StandardCharsets.UTF_8)))
    }

    Bootstrap[IO]
      .serve[Application.Json](asUTF8(getJSONEndpointsForState(state)))
      .serve[Text.Plain](asUTF8(getPlaintextEndpointsForState(state)))
      .middleware(filters)
      .listen(address)
  }

  // ENDPOINTS (all private functions)
  // ======================================

  // META/DEBUG ENDPOINTS
  // ---------------------------------------
  private def forceError: Endpoint[IO, Unit] = get(path("debug") :: path("forceerror")) {
    throw new Exception("A deliberate error produced by /debug/forceerror.")

    Ok(()) // unreachable anyway, but needed for typechecking
  }

  // ServerState to LobbyState
  private def checkSituationSpace(state: LobbyState): Endpoint[IO, List[api.Error]] = get(path("debug") :: path("space") :: path("check")) {
    Ok(state.check())
  }

  private def printSituationTheory(state: LobbyState): Endpoint[IO, String] = get(path("debug") :: path("space") :: path("print")) {
    Ok(state.presenter.asString(state.situationSpace))
  }
  // ---------------------------------------

  // REAL ENDPOINTS FOR USE BY GAME ENGINE
  // --------------------------------------- (up to the end of the file)
  private def buildArchiveLight(state: LobbyState): Endpoint[IO, Unit] = post(path("archive") :: path("build-light")) {
    state.ctrl.handleLine(s"build ${FrameWorld.archiveID} mmt-omdoc Scrolls")
    Ok(())
  }

  private def buildArchive(state: LobbyState): Endpoint[IO, Unit] = post(path("archive") :: path("build")) {
    state.ctrl.handleLine(s"build ${FrameWorld.archiveID} mmt-omdoc")

    Ok(())
  }

  private def reloadArchive(state: LobbyState): Endpoint[IO, Unit] = post(path("archive") :: path("reload")) {
    state.ctrl.backend.getArchive(FrameWorld.archiveID).map(frameWorldArchive => {
      val root = frameWorldArchive.root

      state.ctrl.backend.removeStore(frameWorldArchive)
      state.ctrl.addArchive(root)

      Ok(())
    }).getOrElse(NotFound(new Exception("MMT backend did not know FrameWorld archive by ID, but upon server start it did apparently (otherwise we would have aborted there). Something is inconsistent.")))
  }

  private def addFact_(state: LobbyState, fact: SFact): Either[FactValidationException, FactReference] = {
    val factConstant = fact.toFinalConstant(state.nextFactPath())

    def makeException(errors: List[api.Error]): FactValidationException = {
      FactValidationException(
        message = "Could not validate fact, errors were:\n\n" + errors.map {
          // for [[InvalidUnit]] also elaborate their history for better feedback
          case err: InvalidUnit => err.toString + "\n" + err.history
          case err => err
        }.mkString("\n"),
        processedFacts = List(ProcessedFactDebugInfo.fromConstant(factConstant)(state.ctrl, state.presenter))
      )
    }

    state.synchronized {
      //state.contentValidator.checkDeclarationAgainstTheory(state.situationTheory, factConstant) match {
      List[info.kwarc.mmt.api.Error]() match { // todo: mmt bug
        case Nil =>
          // success (i.e. no errors)
          try {
            state.ctrl.add(factConstant)
            Utils.endAddModule(state.situationTheory)(state.ctrl)
            Right(FactReference(factConstant.path))
          } catch {
            case err: AddError => Left(makeException(List(err)))
          }

        case errors => Left(makeException(errors))
      }
    }
  }

  private def addFact(state: LobbyState): Endpoint[IO, FactReference] = post(path("fact") :: path("add") :: jsonBody[SFact]) { (fact: SFact) => {
    addFact_(state, fact) match {
      case Left(exception) => NotAcceptable(exception)
      case Right(factRef) => Ok(factRef)
    }
  }}

  private def bulkaddFacts(state: LobbyState): Endpoint[IO, List[(Option[FactReference], String)]] = post(path("fact") :: path("bulkadd") :: jsonBody[List[SFact]]) { (facts: List[SFact]) => {
    Ok(facts.map(addFact_(state, _)).map {
      case Left(exception) => (None, exception.toString)
      case Right(factRef) => (Some(factRef), "")
    })
  }}

  private def listFacts(state: LobbyState): Endpoint[IO, List[SFact]] = get(path("fact") :: path("list")) {
    Ok(
      Fact
        .findAllIn(state.situationTheory, recurseOnInclusions = true)(state.ctrl)
        .map(_.toSimple(state.ctrl))
    )
  }

  private def listAllScrolls(state: LobbyState): Endpoint[IO, List[SScroll]] = get(path("scroll") :: path("listall")) {
    Ok(Scroll.findAll()(state.ctrl).map(_.renderSimple()(state.ctrl)))
  }

  private def listScrolls(state: LobbyState): Endpoint[IO, List[SScroll]] = get(path("scroll") :: path("list")) {
    Ok(Scroll.findIncludedIn(state.situationTheory)(state.ctrl).map(_.renderSimple()(state.ctrl)))
  }

  private def applyScroll(state: LobbyState): Endpoint[IO, SScrollApplicationResult] = post(path("scroll") :: path("apply") :: jsonBody[SScrollApplication]) { (scrollApp: SScrollApplication) =>
    Scroll.fromReference(scrollApp.scroll)(state.ctrl) match {
      case Some(scroll) =>
        implicit val ctrl: Controller = state.ctrl
        implicit val _state: LobbyState = state

        val (scrollView, scrollViewPaths, errors) = prepScrollApplication(scrollApp)

        try {
          if (errors.isEmpty) {
            // After this line, `state.situationTheory` will be the new situation theory
            // into which we will generate (a) the pushed out scroll solution and (b)
            // the view into the pushout (i.e., `state.situationTheory` itself)
            state.descendSituationTheory(state.nextSituationTheoryName())

            val viewIntoPushout: View = {
              // the pushout view should be analogously named to the corresponding scroll view
              val path = state.situationTheory.path / LocalName(scrollView.name.last.toString + "_pushout")

              val view = View(
                path.doc,
                path.name,
                from = OMMOD(scroll.ref.solutionTheory),
                to = state.situationTheory.toTerm,
                isImplicit = false
              )

              view
            }

            Utils.addModule(viewIntoPushout)

            Pushout.injectPushoutAlongDirectInclusion(
              state.ctrl.getTheory(scrollView.from.toMPath),
              state.ctrl.getTheory(scrollView.to.toMPath),
              state.ctrl.getTheory(scroll.ref.solutionTheory),
              state.situationTheory,
              scrollView,
              viewIntoPushout,
              addPosition = Before(viewIntoPushout.name.dropPrefix(state.situationTheory.name).get)
            )(state.ctrl)

            // finalize new modules
            Utils.endAddModule(viewIntoPushout)
            Utils.endAddModule(state.situationTheory)

            // sanity check
            require(state.check().isEmpty, "Situation space doesn't typecheck after scroll application, " +
              "but scroll view did (and hence should have guaranteed the situation space to remain well-typed. " +
              "Implementation error?")

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

  private def prepScrollApplication(scrollApp: SScrollApplication)(implicit state: LobbyState): (View, List[Path], List[SCheckingError]) = {
    // the scroll view and all paths (for later deletion from [[Controller]])
    val (scrollView, scrollViewPaths) = {
      val scrollViewName = state.nextScrollViewName()
      val view = scrollApp.toView(
        target = state.situationTheory.path / scrollViewName,
        codomain = state.situationTheory.toTerm
      )(state.ctrl)
      val paths = List(state.situationTheory.path ? scrollViewName, state.situationTheory.path / scrollViewName)

      (view, paths)
    }
    val errors = state.contentValidator.checkScrollView(scrollView, scrollApp.assignments)
    if (errors.nonEmpty) {
      state.log(state.presenter.asString(state.situationSpace))
    }

    (scrollView, scrollViewPaths, errors)
  }

  private def getCompletions(scrollApp: SScrollApplication)(implicit state: LobbyState): List[SScrollAssignments] = {
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

  private def dynamicScroll(state: LobbyState): Endpoint[IO, SDynamicScrollInfo] = post(path("scroll") :: path("dynamic") :: jsonBody[SScrollApplication]) { (scrollApp: SScrollApplication) =>
    Scroll.fromReference(scrollApp.scroll)(state.ctrl) match {
      case Some(scroll) =>
        implicit val ctrl: Controller = state.ctrl
        implicit val _state: LobbyState = state

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