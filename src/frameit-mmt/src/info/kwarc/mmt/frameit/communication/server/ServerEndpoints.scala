package info.kwarc.mmt.frameit.communication.server

import cats.effect.IO
import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.View
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.ontology.IsTheory
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.symbols.{ApplyMorphism, Constant, FinalConstant, TermContainer, Visibility}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.business._
import info.kwarc.mmt.frameit.business.datastructures.{Fact, FactReference, Scroll}
import info.kwarc.mmt.frameit.communication.datastructures.DataStructures._
import info.kwarc.mmt.moduleexpressions.operators.NewPushoutUtils
import io.circe.Json
import io.finch._
import io.finch.circe._

import scala.util.{Random, Try}

sealed abstract class ValidationException(message: String, cause: Throwable = None.orNull)
  extends Exception(message, cause)

sealed case class ProcessedFactDebugInfo(tpAST: String, dfAST: String, presentedString: String, omdocXml: String) {
  def asJson: Json = Json.obj(
    "tpAST" -> Json.fromString(tpAST),
    "dfAST" -> Json.fromString(dfAST),
    "presentedString" -> Json.fromString(presentedString),
    "omdocXml" -> Json.fromString(omdocXml)
  )
}
object ProcessedFactDebugInfo {
  def fromConstant(c: Constant)(implicit ctrl: Controller, presenter: MMTSyntaxPresenter): ProcessedFactDebugInfo = {
    ProcessedFactDebugInfo(
      // avoid bubbling up exceptions to always have at least some debug information instead of none
      c.tp.map(_.toString).getOrElse("<no type available>"),
      c.df.map(_.toString).getOrElse("<no definiens available>"),
      Try(presenter.presentToString(c)).getOrElse("<presentation threw exception>"),
      Try(c.toNode.toString()).getOrElse("<conversion to XML threw exception>")
    )
  }
}

final case class FactValidationException(message: String, processedFacts: List[ProcessedFactDebugInfo], cause: Throwable = None.orNull) extends ValidationException(message, cause) {
  def asJson: Json = Json.obj(
    "message" -> Json.fromString(message),
    "processedFacts" -> Json.arr(processedFacts.map(_.asJson) : _*),
    "cause" -> Json.fromString(Option(cause).toString)
  )
}

/**
  * A collection of REST routes for our [[Server server]]
  */
object ServerEndpoints extends EndpointModule[IO] {
  // vvvvvvv DO NOT REMOVE IMPORTS (even if IntelliJ marks it as unused)
  import info.kwarc.mmt.frameit.communication.datastructures.Codecs
  import Codecs.DataStructureCodecs._
  import ServerErrorHandler._
  // ^^^^^^^ END: DO NOT REMOVE

  private def getEndpointsForState(state: ServerState) =
    printHelp(state) :+: buildArchiveLight(state) :+: buildArchive(state) :+: addFact(state) :+: listFacts(state) :+: listScrolls(state) :+: applyScroll(state) :+: printSituationTheory(state)

  def getServiceForState(state: ServerState): Service[Request, Response] =
    getEndpointsForState(state).toServiceAs[Application.Json]

  // ENDPOINTS (all private functions)
  // ======================================
  private def printHelp(state: ServerState): Endpoint[IO, String] = get(path("help")) {
    Ok(getEndpointsForState(state).toString)
  }

  private def buildArchiveLight(state: ServerState): Endpoint[IO, Unit] = post(path("archive") :: path("build-light")) {
    state.ctrl.handleLine(s"build ${FrameWorld.archiveID} mmt-omdoc Scrolls")

    Ok(())
  }

  private def buildArchive(state: ServerState): Endpoint[IO, Unit] = post(path("archive") :: path("build")) {
    state.ctrl.handleLine(s"build ${FrameWorld.archiveID} mmt-omdoc")

    Ok(())
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
        .map(_.render()(state.ctrl))
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

    Ok(Scroll.findAll()(state.ctrl).map(_.renderNaive()(state.ctrl)))
  }

  private sealed case class ScrollApplicationNames(view: LocalName, pushedOutView: LocalName, situationTheoryExtension: LocalName)

  /**
    * Generate names for the scroll view and the view generated by pushing over it.
    */
  private def generateScrollApplicationNames(state: ServerState): ScrollApplicationNames = {
    val r = Random.nextInt()
    ScrollApplicationNames(
      LocalName(s"frameit_scroll_view_${r}"), // TODO: improve this, let game engine dictate one?
      LocalName(s"frameit_pushed_scroll_view_${r}"),  // TODO: improve this, let game engine dictate one?
      LocalName(s"frameit_ext_situation_theory_${r}")
    )
  }

  private def applyScroll(state: ServerState): Endpoint[IO, List[SFact]] = post(path("scroll") :: path("apply") :: jsonBody[SScrollApplication]) { (scrollApp: SScrollApplication) => {

    val scrollViewDomain = scrollApp.scroll.problemTheory
    val scrollViewCodomain = state.situationTheoryPath

    val ScrollApplicationNames(scrollViewName, pushedOutScrollViewName, situationTheoryExtensionName) = generateScrollApplicationNames(state)

    // create view out of [[SScrollApplication]]
    val scrollView = new View(
      doc = state.situationDocument,
      name = scrollViewName,
      fromC = TermContainer.asParsed(OMMOD(scrollViewDomain)),
      toC = TermContainer.asParsed(OMMOD(scrollViewCodomain)),
      dfC = TermContainer.empty(),
      isImplicit = false
    )

    state.ctrl.add(scrollView)

    // collect all assignments such that if typechecking later fails, we can conveniently output
    // debug information
    val scrollViewAssignments = scrollApp.assignments.map {
      case (factRef, assignedTerm) =>
        // create new assignment
        new FinalConstant(
          home = scrollView.toTerm,
          name = LocalName(ComplexStep(factRef.uri.module) :: factRef.uri.name),
          alias = Nil,
          tpC = TermContainer.empty(),
          dfC = TermContainer.asParsed(assignedTerm),
          rl = None,
          notC = NotationContainer.empty(),
          vs = Visibility.public,
        )
    }
    scrollViewAssignments.foreach(state.ctrl.add(_))

    (if (state.doTypeChecking) state.contentValidator.checkView(scrollView) else Nil) match {
      case Nil =>
        val (situationTheoryExtension, _) = NewPushoutUtils.computeNamedPushoutAlongDirectInclusion(
          state.ctrl.getTheory(scrollViewDomain),
          state.ctrl.getTheory(scrollViewCodomain),
          state.ctrl.getTheory(scrollApp.scroll.solutionTheory),
          state.situationDocument ? situationTheoryExtensionName,
          scrollView,
          w_to_generate = state.situationDocument ? pushedOutScrollViewName
        )(state.ctrl)

        state.setSituationTheory(situationTheoryExtension)

        Ok(
          Fact
            .findAllIn(situationTheoryExtension, recurseOnInclusions = false)(state.ctrl)
            .map(_.render()(state.ctrl))
        )

      case errors =>
        state.ctrl.delete(scrollView.path)

        NotAcceptable(FactValidationException(
          "View for scroll application does not validate, errors were:\n\n" + errors.mkString("\n"),
          scrollViewAssignments.map(d => ProcessedFactDebugInfo.fromConstant(d)(state.ctrl, state.presenter))
        ))
    }
  }}

  /*private def dynamicScroll(state: ServerState): Endpoint[IO, Scroll] = post(path("scroll") :: path("dynamic") :: jsonBody[SScrollApplication]) { (scrollApp: SScrollApplication) => {

    val scroll = Scroll.fromReference(scrollApp.scroll)(state.ctrl)

    ApplyMorphism(state.ctrl.globalLookup, ???)(???, ???)

    Ok(Scroll(???, ???, ???, ???))

  }}*/
}