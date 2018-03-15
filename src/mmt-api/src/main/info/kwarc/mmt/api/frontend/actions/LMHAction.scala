package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api._
import archives.lmh._
import frontend._
import utils._

/** shared base class for Actions relating to OAF (lmh) */
sealed abstract class LMHAction extends ActionImpl {
  protected def mathHub(implicit controller: Controller): MathHub = controller.getMathHub.getOrElse {
    throw GeneralError("no LMH configuration entry found")
  }
}

case object ShowLMH extends LMHAction with ResponsiveAction {
  override def toParseString: String = "show lmh"
  def apply(implicit controller: Controller) {
    controller.getMathHub match {
      case Some(mh) => {
        respond("Local LMH Root at: " + mh.local)
        respond("Remote MathHub at: " + mh.remote)
      }
      case None => respond("No lmh configured, use 'lmh root <path> [ssh|https]' to set one. ")
    }
  }
}
object ShowLMHCompanion extends ActionObjectCompanionImpl[ShowLMH.type]("print information about lmh", "show lmh")

case class SetLMHRoot(path: String, https: Boolean) extends LMHAction {
  def apply(implicit controller: Controller) {
    // create a new lmh instance
    controller.lmh = Some(new MathHub(controller, File(path), MathHub.defaultURL, https))
  }
  override def toParseString = s"lmh root $path ${if(https) "https" else "ssh"}"
}
object SetLMHRootCompanion extends ActionCompanionImpl[SetLMHRoot]("set the lmh root folder", "lmh root"){
  import Action._
  def parserActual(implicit state: ActionState) = str ~ (("ssh" | "https")?) ^^ {
    case p ~ Some(https) => SetLMHRoot(p, https == "https")
    case p ~ None => SetLMHRoot(p, true)
  }
}

case class LMHInit(path: String, template: Option[String]) extends LMHAction {
  def apply(implicit controller: Controller) {mathHub.createEntry(path)}
  override def toParseString = s"lmh init $path"
}
object LMHInitCompanion extends ActionCompanionImpl[LMHInit]("create a new lmh archive", "lmh init", "oaf init"){
  import Action._
  def parserActual(implicit state: ActionState) = str ~ (str?) ^^ {case p ~ t => LMHInit(p, t)}
}

case class LMHClone(id: String, version: Option[String]) extends LMHAction {
  def apply(implicit controller: Controller) {
    // install all the entries
    mathHub.installEntry(id, version, recursive=true)
  }
  def toParseString = s"lmh clone $id${version.map(" " + ).getOrElse("")}"
}
object LMHCloneCompanion extends ActionCompanionImpl[LMHClone]("clone specific versions of archives from MathHub", "lmh clone"){
  import Action._
  def parserActual(implicit state: ActionState) = str ~ (str?) ^^ { case i ~ v => LMHClone(i, v)}
}


case class LMHInstall(spec: List[String]) extends LMHAction {
  def apply(implicit controller: Controller) {
    val resolved = mathHub.available(spec: _*)
    resolved.foreach {case (id, version) => mathHub.installEntry(id, version, recursive=true) }
  }
  def toParseString = s"lmh install ${spec.mkString(" ")}".trim
}
object LMHInstallCompanion extends ActionCompanionImpl[LMHInstall]("install a set of archives from MathHub", "lmh install", "oaf clone"){
  import Action._
  override val addKeywords : Boolean = false
  def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ LMHInstall
}

case class LMHListRemote(spec: List[String]) extends LMHAction with ResponsiveAction {
  def apply(implicit controller: Controller) {
    mathHub.available(spec: _*).foreach {case (id, version) => respond(s"$id")}
  }
  def toParseString = s"lmh search ${spec.mkString(" ")}".trim
}
object LMHListRemoteCompanion extends ActionCompanionImpl[LMHListRemote]("show remote archives matching a pattern", "lmh search"){
  import Action._
  override val addKeywords : Boolean = false
  def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ LMHListRemote
}


/** shared trait for local lmh actions */
sealed trait LocalAction extends LMHAction {
  /** list archives that this action will iterate over */
  val spec: List[String]

  def apply(implicit controller: Controller) {mathHub.entries(spec: _*) foreach applyActual}
  protected def applyActual(archive: LMHHubEntry)(implicit controller: Controller): Unit
}


case class LMHList(spec: List[String]) extends LMHAction with LocalAction with ResponsiveAction {
  protected def applyActual(archive: LMHHubEntry)(implicit controller: Controller) {archive.version match {
    case Some(v) => respond(s"${archive.id}: $v")
    case None => respond(s"${archive.id}: (unversioned)")
  }}
  def toParseString = s"lmh ls ${spec.mkString(" ")}".trim
}
object LMHListCompanion extends ActionCompanionImpl[LMHList]("show archives that are installed locally along with their versions", "lmh ls"){
  import Action._
  override val addKeywords : Boolean = false
  def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ LMHList
}


case class LMHPull(spec: List[String]) extends LMHAction with LocalAction {
  def applyActual(entry: LMHHubEntry)(implicit controller: Controller) {entry.pull}
  def toParseString = s"lmh pull ${spec.mkString(" ")}".trim
}
object LMHPullCompanion extends ActionCompanionImpl[LMHPull]("pull updates to locally installed archives", "lmh pull", "oaf pull"){
  import Action._
  override val addKeywords : Boolean = false
  def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ LMHPull
}


case class LMHPush(spec: List[String]) extends LMHAction with LocalAction {
  def applyActual(entry: LMHHubEntry)(implicit controller: Controller) {entry.push}
  def toParseString = s"lmh push ${spec.mkString(" ")}".trim
}
object LMHPushCompanion extends ActionCompanionImpl[LMHPush]("push updates to locally installed archives", "lmh push", "oaf push"){
  import Action._
  override val addKeywords : Boolean = false
  def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ LMHPush
}


case class LMHSetRemote(spec: List[String]) extends LMHAction with LocalAction {
  def applyActual(entry: LMHHubEntry)(implicit controller: Controller) = entry.fixRemote
  def toParseString = s"lmh setremote ${spec.mkString(" ")}".trim
}
object LMHSetRemoteCompanion extends ActionCompanionImpl[LMHSetRemote]("fix all remote urls for installed archives", "lmh setremote", "oaf setremote") {
  import Action._
  override val addKeywords : Boolean = false
  def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ LMHSetRemote
}

trait LMHActionHandling {self: Controller =>

  /** contains a loaded MathHub instance (if any) */
  private[api] var lmh: Option[MathHub] = None
  /** get the current MathHub, if any */
  def getMathHub : Option[MathHub] = lmh
}
