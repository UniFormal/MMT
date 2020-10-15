package info.kwarc.mmt.frameit.communication.server

import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.{Controller, Logger, Report}
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.presentation.MMTSyntaxPresenter
import info.kwarc.mmt.api.{DPath, GeneralError, MPath}
import info.kwarc.mmt.frameit.business.ContentValidator

/**
  * An object wrapping all mutable state our server endpoints below are able to mutate.
  *
  * It serves encapsulating state to be as immutable as possible.
  */
class ServerState(val ctrl: Controller, val situationDocument: DPath, private var _situationTheoryPath: MPath) extends Logger {
  override def logPrefix: String = "frameit-server"
  override protected def report: Report = ctrl.report

  var doTypeChecking: Boolean = true

  val contentValidator : ContentValidator = new ContentValidator(ctrl)

  val presenter : MMTSyntaxPresenter = ctrl.extman.getOrAddExtension(classOf[MMTSyntaxPresenter], "present-text-notations").getOrElse(
    throw GeneralError("could not get MMTSyntaxPresenter extension required for printing")
  )

  private var _situationTheory = ctrl.getTheory(situationTheoryPath)

  def situationTheory: Theory = _situationTheory
  def situationTheoryPath: MPath = _situationTheoryPath

  def setSituationTheory(newSituationTheory: Theory): Unit = {
    _situationTheory = newSituationTheory
    _situationTheoryPath = _situationTheoryPath
  }

  // make log methods from Logger public by trivially overriding them
  // TODO logging does not work currently, messages go nowhere
  override def log(e: api.Error): Unit = super.log(e)
  override def log(s: => String, subgroup: Option[String] = None): Unit = super.log(s, subgroup)
}