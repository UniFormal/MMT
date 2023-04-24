package info.kwarc.mmt.frameit.communication.server

import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.{Controller, Logger, Report}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.objects.Context
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.{GeneralError, GlobalName, LocalName, MPath}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.business.{ContentValidator, SituationTheory}

import java.util.concurrent.atomic.AtomicInteger

/**
  * An object wrapping all mutable state our server endpoints below are able to mutate.
  *
  * It serves encapsulating state to be as immutable as possible.
  *
  * @todo Some parts are thread-safe (e.g. to multiple requests to the server), some are not.
  *       Current assumption is that everything is sequentially run only.
  */
class ServerState(val contentValidator: ContentValidator, initialSituationTheory: Option[SituationTheory])(implicit val ctrl: Controller) extends Logger {

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

  override val logPrefix: String = ServerState.logPrefix
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

  // make log methods from Logger public by trivially overriding them
  // TODO logging does not work currently, messages go nowhere
  override def log(e: api.Error): Unit = super.log(e)
  override def log(s: => String, subgroup: Option[String] = None): Unit = super.log(s, subgroup)
}

object ServerState {
  val logPrefix = "frameit-mmt-server"
}