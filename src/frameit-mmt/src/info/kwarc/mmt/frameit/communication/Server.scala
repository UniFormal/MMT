package info.kwarc.mmt.frameit.communication

import cats.effect.IO
import com.twitter.finagle.Http
import com.twitter.util.Await
import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller, Logger, Report}
import info.kwarc.mmt.api.metadata.MetaDatum
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.ontology.IsTheory
import info.kwarc.mmt.api.symbols.{FinalConstant, TermContainer, Visibility}
import info.kwarc.mmt.api.utils.{File, FilePath}
import info.kwarc.mmt.api._
import info.kwarc.mmt.frameit.archives.Archives
import info.kwarc.mmt.frameit.archives.Foundation.StringLiterals
import info.kwarc.mmt.frameit.business.{ContentValidator, Scroll}
import info.kwarc.mmt.moduleexpressions.operators.NamedPushoutUtils
import io.finch.circe._
import io.finch._
import io.circe.generic.extras.auto._

import scala.util.Random

sealed abstract class ValidationException(message: String, cause: Throwable = None.orNull)
  extends Exception(message, cause)

final case class FactValidationException(message: String, cause: Throwable = None.orNull) extends ValidationException(message, cause)

object ServerEntrypoint extends App {
  Server.start(args.toList)
}

object Server extends EndpointModule[IO] {
  /**
    * An object wrapping all mutable state our server endpoints below are able to mutate.
    *
    * It serves encapsulating state to be as immutable as possible.
    */
  class State(val ctrl: Controller, val situationDocument: DPath, var situationTheory: MPath) extends Logger {
    override def logPrefix: String = "frameit-server"
    override protected def report: Report = ctrl.report

    val contentValidator : ContentValidator = new ContentValidator(ctrl)

    // make log methods from Logger public by trivially overriding them
    // TODO logging does not work currently, messages go nowhere
    override def log(e: api.Error): Unit = super.log(e)
    override def log(s: => String, subgroup: Option[String] = None): Unit = super.log(s, subgroup)
  }

  private def getEndpoints(state: State) =
    buildArchive(state) :+: buildArchiveLight(state) :+: addFact(state) :+: listScrolls(state) :+: applyScroll(state)

  def start(args: List[String]): Unit = args match {
    case List(archiveRoot, port) =>
      println(s"Starting server on port ${port} with archive root ${archiveRoot}...")
      val state = initState(Archives.getPaths(File(archiveRoot)))

      Await.result(Http.server.serve(s":${port}", getEndpoints(state).toServiceAs[Application.Json]))

    case _ =>
      sys.error("Invalid usage")
      println(s"Usage: ${args.headOption.getOrElse("<java -jar ... see readme>")} <PathToArchiveRoot> <Port>")
      println("PathToArchiveRoot should point to your working copy of https://github.com/UFrameIT/archives")
  }

  // Endpoints
  // =================

  private def initState(archivePaths: List[File]): State = {
    val ctrl = new Controller()
    ctrl.report.addHandler(ConsoleHandler)
    archivePaths.foreach(ctrl.addArchive)
    val frameitArchive = ctrl.backend.getArchive(Archives.FrameWorld.archiveID).getOrElse {
        throw info.kwarc.mmt.api.GetError(s"Archive ${Archives.FrameWorld.archiveID} could not be found!")
    }

    // TODO hack to read latest scroll meta data, should not be needed
    //      due to https://github.com/UniFormal/MMT/issues/528
    ctrl.handleLine(s"build ${Archives.FrameWorld.archiveID} mmt-omdoc Scrolls/OppositeLen.mmt")

    frameitArchive.allContent

    // force-read relational data as somewhere (TODO say where) we use the depstore
    // to get meta tags on things
    frameitArchive.readRelational(FilePath("/"), ctrl, "rel")

    val situationTheory = Theory.empty(
      DPath(frameitArchive.narrationBase),
      LocalName("SituationTheory"),
      Some(Archives.FrameWorld.FactCollection)
    )
    ctrl.add(situationTheory)

    val state = new State(ctrl, situationTheory.path.parent, situationTheory.path)

    state.contentValidator.checkTheory(situationTheory) match {
      case Nil =>
        state

      case errors =>
        sys.error("Created situation theory, but cannot successfully typecheck it. Server will not be started. Errors below:")
        sys.error(errors.mkString("\n"))
        throw new Exception("")
    }
  }

  private def buildArchiveLight(state: State): Endpoint[IO, Unit] = post(path("archive") :: path("build-light")) {
    state.ctrl.handleLine(s"build ${Archives.FrameWorld.archiveID} mmt-omdoc Scrolls/OppositeLen.mmt")

    Ok(())
  }

  private def buildArchive(state: State): Endpoint[IO, Unit] = post(path("archive") :: path("build")) {
    state.ctrl.handleLine(s"build ${Archives.FrameWorld.archiveID} mmt-omdoc")

    Ok(())
  }

  private def addFact(state: State): Endpoint[IO, Unit] = post(path("fact") :: path("add") :: jsonBody[SNewFact]) {
    (fact: SNewFact) => {
      // create MMT declaration out [[SNewFact]]
      val factConstant = new FinalConstant(
        home = OMMOD(state.situationTheory),
        name = LocalName(SimpleStep(fact.label)),
        alias = Nil,
        tpC = TermContainer.asParsed(SOMDoc.OMDocBridge.decode(fact.tp)),
        dfC = TermContainer.asParsed(fact.df.map(SOMDoc.OMDocBridge.decode)),
        rl = None,
        notC = new NotationContainer,
        vs = Visibility.public
      )

      factConstant.metadata.add(MetaDatum(Archives.FrameWorld.MetaKeys.factLabel, StringLiterals(fact.label)))

      state.contentValidator.checkDeclarationAgainstTheory(state.situationTheory, factConstant) match {
        case Nil =>
          // success (i.e. no errors)
          state.ctrl.add(factConstant)
          Ok(())

        case errors =>
          NotAcceptable(FactValidationException(
            "Could not validate fact, errors were:\n\n" + errors.mkString("\n")
          ))
      }
    }
  }

  private def listScrolls(state: State): Endpoint[IO, List[SScroll]] = get(path("scroll") :: path("list")) {
    val allTheories = state.ctrl.depstore.getInds(IsTheory).map(_.asInstanceOf[MPath]).map(state.ctrl.getTheory)

    val scrolls = allTheories.flatMap(t => Scroll.fromTheory(t)(state.ctrl.globalLookup) match {
      case Right(scroll) => Some(scroll)
      case Left(err) =>
        state.log(s"Ignoring theory ${t} due to error below. Note that theories that are not scrolls also emit such errors.")
        state.log(err.toString)
        None
    }).map(_.simplified).toList

    Ok(scrolls)
  }

  private sealed case class ScrollApplicationNames(view: LocalName, pushedOutView: LocalName, situationTheoryExtension: LocalName)

  /**
    * Generate names for the scroll view and the view generated by pushing over it.
    */
  private def generateScrollApplicationNames(state: State): ScrollApplicationNames = {
    val r = Random.nextInt()
    ScrollApplicationNames(
      LocalName(s"frameit_scroll_view_${r}"), // TODO: improve this, let game engine dictate one?
      LocalName(s"frameit_pushed_scroll_view_${r}"),  // TODO: improve this, let game engine dictate one?
      LocalName(s"frameit_ext_situation_theory_${r}")
    )
  }

  private def applyScroll(state: State): Endpoint[IO, List[SFact]] = post(path("scroll") :: path("apply") :: jsonBody[SScrollApplication]) { (scrollApp: SScrollApplication) => {

    val scrollViewDomain = Path.parseM(scrollApp.scroll.problemTheory, NamespaceMap.empty)
    val scrollViewCodomain = state.situationTheory

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

    val scrollViewAssignments = scrollApp.assignments.map {
      case (factRef, assignedTerm) =>

        val factUri = Path.parseS(factRef.uri, NamespaceMap.empty)

        // create new assignment
        new FinalConstant(
          home = scrollView.toTerm,
          name = LocalName(ComplexStep(factUri.module) :: factUri.name),
          alias = Nil,
          tpC = TermContainer.empty(),
          dfC = TermContainer.asParsed(SOMDoc.OMDocBridge.decode(assignedTerm)),
          rl = None,
          notC = new NotationContainer,
          vs = Visibility.public,
        )
    }

    state.ctrl.add(scrollView)
    scrollViewAssignments.foreach(state.ctrl.add(_))

    state.contentValidator.checkView(scrollView) match {
      case Nil =>
        // TODO: perform pushout, add new facts to situation theory, and return new facts

        val (situationTheoryExtension, _) = NamedPushoutUtils.computeCanonicalPushoutAlongDirectInclusion(
          state.ctrl.getTheory(scrollViewDomain),
          state.ctrl.getTheory(scrollViewCodomain),
          state.ctrl.getTheory(Path.parseM(scrollApp.scroll.solutionTheory, NamespaceMap.empty)),
          state.situationDocument ? situationTheoryExtensionName,
          scrollView,
          w_to_generate = state.situationDocument ? pushedOutScrollViewName
        )

        state.situationTheory = situationTheoryExtension.path

        Ok(List[SFact]())

      case errors =>
        state.ctrl.delete(scrollView.path)

        NotAcceptable(FactValidationException(
          "View for scroll application does not validate, errors were:\n\n" + errors.mkString("\n")
        ))
    }
  }}
/*
  def getHintsForPartialScroll(gameLogic: FrameItLogic): Endpoint[IO,String] = post(path("scroll") :: path("hints") :: stringBody) {data: String => {
    /**
      * Example:
      * {
      *   domainTheory: http://...?SituationTheory,
      *   scroll: {
      *     "problem": http://...?OppositeLen_Problem,
      *     "solution": // unused
      *   },
      *   assignments: {
      *     "http://...?pA": {x: ..., y: ..., z: ...}
      *     ...
      *   }
      * }
      */
    JSON.parseFull(data)
    Ok("abc")
  }}

  def getPushOut(gameLogic: FrameItLogic): Endpoint[IO,String ] =
    get(
      path("pushout")
        ::param("problem")
        ::param("solution")
        ::param("view")
    ){
    (prob : String,sol:String,view:String ) =>{
      val ret = gameLogic.applyScroll(prob, sol,view)
      if(ret.isDefined) Ok(ret.get) else BadRequest(new IllegalArgumentException())
    }
  }

*/
}
