package info.kwarc.mmt.frameit.communication.server

import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.{Controller, Logger, Report}
import info.kwarc.mmt.api.modules.{Theory, View}
import info.kwarc.mmt.api.objects.{Context, OMMOD}
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.{DPath, GeneralError, GlobalName, LocalName, MPath, SimpleStep}
import info.kwarc.mmt.frameit.archives.FrameIT.FrameWorld
import info.kwarc.mmt.frameit.business.datastructures.ScrollReference
import info.kwarc.mmt.frameit.business.{ContentValidator, SituationTheory, Utils}

/**
  * An object wrapping all mutable state our server endpoints below are able to mutate.
  *
  * It serves encapsulating state to be as immutable as possible.
  */
class ServerState(private var curSituationTheory: SituationTheory, val contentValidator: ContentValidator)(implicit val ctrl: Controller) extends Logger {
  override def logPrefix: String = "frameit-server"
  override protected def report: Report = ctrl.report

  val presenter : MMTSyntaxPresenter = ctrl.extman.getOrAddExtension(classOf[MMTSyntaxPresenter], "present-text-notations").getOrElse(
    throw GeneralError("could not get MMTSyntaxPresenter extension required for printing")
  )

  def situationSpace: Theory = curSituationTheory.spaceTheory
  def situationTheory: Theory = curSituationTheory.theory

  private var factCounter = 1
  def newFactPath(): GlobalName = {
    this.synchronized {
      val (factName, _) = Context.pickFresh(situationTheory.getInnerContext, LocalName(s"fact${factCounter.toString}"))
      factCounter += 1
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

  // make log methods from Logger public by trivially overriding them
  // TODO logging does not work currently, messages go nowhere
  override def log(e: api.Error): Unit = super.log(e)
  override def log(s: => String, subgroup: Option[String] = None): Unit = super.log(s, subgroup)
}