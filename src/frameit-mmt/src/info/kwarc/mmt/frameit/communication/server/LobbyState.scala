package info.kwarc.mmt.frameit.communication.server

import com.twitter.server.TwitterServer
import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.{Controller, Logger, Report}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.Context
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.utils.{File, FilePath}
import info.kwarc.mmt.api.{ArchiveError, GeneralError, GlobalName, InvalidElement, LocalName, MPath}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.business.{ContentValidator, SituationTheory, StandardContentValidator}

//?
import info.kwarc.mmt.api.frontend.{ConsoleHandler, Controller}

import java.util.concurrent.atomic.AtomicInteger

/**
  * An object wrapping all mutable state our server endpoints below are able to mutate.
  *
  * It serves encapsulating state to be as immutable as possible.
  *
  * @todo Some parts are thread-safe (e.g. to multiple requests to the server), some are not.
  *       Current assumption is that everything is sequentially run only.
  */
class LobbyState(archiveRoot: File, debug: Boolean) extends Logger{

  implicit val ctrl: Controller = new Controller() //private? But in then in ConcreteServerEndpoints Problems
  // ctrl = new Controller()
  ctrl.report.addHandler(ConsoleHandler)
  ctrl.handleLine(s"log+ ${ServerState.logPrefix}")

  ctrl.handleLine(s"mathpath archive ${archiveRoot}")
  val frameitArchive = ctrl.backend.getArchive(FrameWorld.archiveID).getOrElse {
    throw ArchiveError(FrameWorld.archiveID, "archive could not be found")
  }

  // force-read relational data as [[info.kwarc.mmt.frameit.business.datastructures.Scroll]] uses
  // the depstore
  frameitArchive.readRelational(FilePath("/"), ctrl, "rel")
  // increase performance by prefetching archive content? ctrl.backend.getArchives.foreach(_.allContent)

  val situationTheory_val = if (debug) Some(new SituationTheory(FrameWorld.debugSituationTheory)) else None
  //val state = new ServerState(new StandardContentValidator, situationTheory)

  //println(state.contentValidator.checkTheory(ctrl.getTheory(Path.parseM("http://mathhub.info/FrameIT/frameworld/integrationtests?SituationSpace"))))
  //sys.exit(0)


  (if (debug) check() else Nil) match {
    case Nil =>
      println("Situation space successfully set-up and typechecked (the latter only in release mode).")
    //state

    case errors =>
      errors.foreach(System.err.println)
      throw InvalidElement(situationSpace, "Initial situation space does not typecheck, see stderr output for errors.")
  }








  val contentValidator = new StandardContentValidator //privat?
  private val initialSituationTheory = if (debug) Some(new SituationTheory(FrameWorld.debugSituationTheory)) else None


  // @SR: manage the following variables within lobbies
  private val factCounter = new AtomicInteger(1)
  private val scrollViewCounter = new AtomicInteger(1)
  private val situationTheoryCounter = new AtomicInteger(1)

  // @SR: maintain a list of lobbies here

  // @SR: every pair (player, lobby) should have the notion of a current situation theory
  var curSituationTheory: SituationTheory = initialSituationTheory.getOrElse {
    new SituationTheory(FrameWorld.defaultRootSituationTheory).descend(nextSituationTheoryName())
  }
  println(s"Using situation space+theory: $situationSpace")

  override val logPrefix: String = LobbyState.logPrefix
  override protected def report: Report = ctrl.report

  val presenter : MMTSyntaxPresenter = ctrl.extman.getOrAddExtension(classOf[MMTSyntaxPresenter], "present-text-notations").getOrElse(
    throw GeneralError("could not get MMTSyntaxPresenter extension required for printing")
  )

  def situationSpace: Theory = curSituationTheory.spaceTheory
  def situationTheory: Theory = curSituationTheory.theory


  def nextScrollViewName(): LocalName = {
    LocalName(s"ScrollView${scrollViewCounter.getAndAdd(1)}")
  }

  def nextSituationTheoryName(): LocalName = {
    LocalName(s"SituationTheory${situationTheoryCounter.getAndAdd(1)}")
  }

  def nextFactPath(): GlobalName = {
    this.synchronized {
      val (factName, _) = Context.pickFresh(situationTheory.getInnerContext, LocalName(s"fact${factCounter.getAndAdd(1)}"))
      situationTheory.path ? factName
    }
  }

  def descendSituationTheory(name: LocalName): Theory = {
    curSituationTheory = curSituationTheory.descend(name)
    curSituationTheory.theory
  }

  def getPathForDescendedSituationTheory(name: LocalName): MPath = {
    curSituationTheory.path.descend(name).module
  }

  def setSituationTheory(newSituationTheory: Theory): Unit = {
    newSituationTheory.path match {
      case MPath(doc, LocalName(steps :+ newName))
        if doc == curSituationTheory.space.doc && steps == curSituationTheory.space.name.steps =>

        curSituationTheory = new SituationTheory(curSituationTheory.path.descend(LocalName(newName)))

      case _ =>
        throw new Exception("New situation theory doesn't adhere to convention of where to store situation theories." +
          s"Path was `${newSituationTheory.path}`.")
    }
  }

  def getPathForView(name: LocalName): MPath = {
    curSituationTheory.space.doc ? (curSituationTheory.space.name / name)
  }

  def check(): List[api.Error] = {
    contentValidator.checkTheory(situationSpace)
  }


/*
  def initTwitterServer(): Unit = {
    val restService = ConcreteServerEndpoints.getServiceForState(this)
    val server = Http.serve(bindAddress(), restService)
    onExit {
      server.close()
    }
    Await.ready(server)
  }*/


  // make log methods from Logger public by trivially overriding them
  // TODO logging does not work currently, messages go nowhere
  override def log(e: api.Error): Unit = super.log(e)
  override def log(s: => String, subgroup: Option[String] = None): Unit = super.log(s, subgroup)
}

object LobbyState {
  val logPrefix = "frameit-mmt-server_lobby"
}