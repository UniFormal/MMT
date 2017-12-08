package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.GeneralError
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.{URI, stringToList}

/** Shared base class for Actions relating to OAF (lmh) */
sealed abstract class OAFAction extends ActionImpl {}

/** clone an archive from a remote OAF
  *
  * concrete syntax: oaf close path:STRING
  */
case class OAFInit(path: String) extends OAFAction {
  def apply(controller: Controller): Unit = controller.getOAFOrError.init(path)
  override def toParseString = s"oaf init $path"
}
object OAFInitCompanion extends ActionCompanionImpl[OAFInit]("clone an archive from a remote OAF", "oaf init"){
  import Action._
  def parserActual(implicit state: ActionState) = str ^^ OAFInit
}

/** clone an archive from a remote OAF
  *
  * concrete syntax: oaf close path:STRING
  */
case class OAFClone(path: String) extends OAFAction {
  def apply(controller: Controller): Unit = controller.cloneOAFArchiveRecursively(path)
  def toParseString = s"oaf clone $path"
}
object OAFCloneCompanion extends ActionCompanionImpl[OAFClone]("clone an archive from a remote OAF", "oaf clone"){
  import Action._
  def parserActual(implicit state: ActionState) = str ^^ OAFClone
}

/** pulls all repositories from remote OAF
  *
  * concrete syntax: oaf pull
  */
case object OAFPull extends OAFAction {
  def apply(controller: Controller) : Unit = controller.getOAFOrError.pullAll
  def toParseString = "oaf pull"
}
object OAFPullCompanion extends ActionObjectCompanionImpl[OAFPull.type]("pulls all repositories from remote OAF", "oaf pull"){
  def parse(implicit state: ActionState) = OAFPull
}

/** pushes all repositories to remote OAF
  *
  * concrete syntax: oaf push
  */
case object OAFPush extends OAFAction {
  def apply(controller: Controller) : Unit = controller.getOAFOrError.pushAll
  def toParseString = "oaf push"
}
object OAFPushCompanion extends ActionObjectCompanionImpl[OAFPush.type]("pushes all repositories to remote OAF", "oaf push")

/** sets all remotes for the remote OAF
  *
  * concrete syntax: oaf push
  */
case object OAFSetRemote extends OAFAction {
  def apply(controller: Controller) = controller.getOAFOrError.setRemoteURL
  def toParseString = "oaf setremote"
}
object OAFSetRemoteCompanion extends ActionObjectCompanionImpl[OAFSetRemote.type]("pushes all repositories to remote OAF", "oaf setremote")

/** helper functions for [[OAFAction]]s */
trait OAFActionHandling {
  self: Controller =>

  /** gets an [OAF] instance or throws an error */
  private[actions] def getOAFOrError = getOAF.getOrElse {
    throw GeneralError("no OAF configuration entry found")
  }

  /** clone an OAF Archive Recursively (i.e. including dependencies) */
  def cloneOAFArchiveRecursively(p: String) {
    val oaf = getOAFOrError
    report("user", "trying to clone " + p)
    val lc = oaf.clone(p) getOrElse {
      logError("cloning failed, trying to download")
      oaf.download(p)
    }.getOrElse {
      logError("downloading failed, giving up")
      return
    }
    val archs = backend.openArchive(lc)
    archs foreach {a =>
      val depS = a.properties.getOrElse("dependencies", "")
      // TODO lmh falsely uses , as separator instead of space
      val deps = if (depS.contains(",")) stringToList(depS, ",").map(_.trim) else stringToList(depS)
      deps foreach {d => cloneOAFArchiveRecursively(URI(d).pathAsString)}
    }
  }
}