package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api._
import archives._
import frontend._
import utils._

/** shared base class for Actions relating to lmh (formerly oaf) */
sealed abstract class LMHAction extends Action {
  protected def mathHub = controller.getMathHub.getOrElse {
    throw GeneralError("no LMH configuration entry found")
  }
}

// region "LMH Information"

case object ShowLMH extends LMHAction with ResponsiveAction {
  override def toParseString: String = "show lmh"
  def apply(): Unit = {
    controller.getMathHub match {
      case Some(mh) =>
        respond("Local LMH Root at: " + mh.local)
        respond("Remote MathHub at: " + mh.remote)
      case None => respond("No lmh configured, use 'lmh root <path> [ssh|https]' to set one. ")
    }
  }
}
object ShowLMHCompanion extends ObjectActionCompanion(ShowLMH, "print information about lmh", "show lmh")

case class LMHList(spec: List[String]) extends LMHAction with LocalAction with ResponsiveAction {
  protected def applyActual(archive: LMHHubEntry): Unit = {
    val name = archive match {
      case ge: LMHHubGroupEntry =>   s"Group   ${ge.group}"
      case ae: LMHHubArchiveEntry => s"Archive ${ae.id}"
      case _ =>                      s"Repo    ${archive.id}"
    }
    val version = archive.version.getOrElse("unknown version")
    respond(s"$name: $version in '${archive.root.toJava.toString}'")
  }
  def toParseString = s"lmh ls ${spec.mkString(" ")}".trim
}
object LMHListCompanion extends ActionCompanion("show archives that are installed locally along with their versions", "lmh ls"){
  import Action._
  override val addKeywords : Boolean = false
  def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ LMHList
}

case class SetLMHRoot(path: String, https: Boolean) extends LMHAction {
  def apply(): Unit = {
    // create a new lmh instance
    controller.lmh = Some(new MathHub(controller, File(path), MathHub.defaultURL, https))
  }
  override def toParseString = s"lmh root $path ${if(https) "https" else "ssh"}"
}
object SetLMHRootCompanion extends ActionCompanion("set the lmh root folder", "lmh root"){
  import Action._
  def parserActual(implicit state: ActionState) = str ~ (("ssh" | "https")?) ^^ {
    case p ~ Some(https) => SetLMHRoot(p, https == "https")
    case p ~ None => SetLMHRoot(p, true)
  }
}

// #endregion

case class LMHInit(path: String, template: Option[String]) extends LMHAction {
  def apply(): Unit = {mathHub.createEntry(path)}
  override def toParseString = s"lmh init $path"
}
object LMHInitCompanion extends ActionCompanion("create a new lmh archive", "lmh init", "oaf init"){
  import Action._
  def parserActual(implicit state: ActionState) = str ~ (str?) ^^ {case p ~ t => LMHInit(p, t)}
}

case class LMHOpen(id: String) extends LMHAction {
  def apply(): Unit = {
    val archive = mathHub.getEntry(id)
    try {
      val root = archive.get.root
      root.openInOS()
      log(s"Opened $root")
    } catch {
      case e: Exception => log(s"Unable to open: ${e.toString}")
    }

  }
  override def toParseString = s"lmh open $id"
}
object LMHOpenCompanion extends ActionCompanion("opens the folder belonging to an archive", "lmh open"){
  import Action._
  def parserActual(implicit state: ActionState) = str ^^ LMHOpen
}

case class LMHInstall(spec: List[String]) extends LMHAction {
  def apply(): Unit = {
    val resolved = mathHub.available(spec: _*)
    mathHub.installEntries(resolved, recursive=true)
  }
  def toParseString = s"lmh install ${spec.mkString(" ")}".trim
}
object LMHInstallCompanion extends ActionCompanion("install a set of archives from MathHub", "lmh install", "oaf clone"){
  import Action._
  override val addKeywords : Boolean = false
  def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ LMHInstall
}

case class LMHUse(spec: List[String]) extends LMHAction {
  def apply(): Unit = {
    /* find all local entries matching the pattern */
    val resolved = mathHub.available(spec: _*)
    mathHub.installEntries(resolved, recursive=false)
  }
  def toParseString = s"lmh use ${spec.mkString(" ")}".trim
}
object LMHUseCompanion extends ActionCompanion("use a specific version of specific archives", "lmh use", "oaf clone"){
  import Action._
  override val addKeywords : Boolean = false
  def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ LMHInstall
}

case class LMHListRemote(spec: List[String]) extends LMHAction with ResponsiveAction {
  def apply(): Unit = {
    mathHub.available(spec: _*).foreach {case (id, version) => respond(s"$id")}
  }
  def toParseString = s"lmh search ${spec.mkString(" ")}".trim
}
object LMHListRemoteCompanion extends ActionCompanion("show remote archives matching a pattern", "lmh search"){
  import Action._
  override val addKeywords : Boolean = false
  def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ LMHListRemote
}


/** shared trait for local lmh actions */
sealed trait LocalAction extends LMHAction {
  /** list archives that this action will iterate over */
  val spec: List[String]

  def apply(): Unit = {mathHub.entries(spec: _*) foreach applyActual}
  protected def applyActual(archive: LMHHubEntry): Unit
}


case class LMHPull(spec: List[String]) extends LMHAction {
  def apply(): Unit = {
    val updates = mathHub.entries(spec: _*).map({a => (a.id, None)})
    mathHub.installEntries(updates, recursive = true)
  }
  def applyActual(entry: LMHHubEntry): Unit = {entry.pull}
  def toParseString = s"lmh pull ${spec.mkString(" ")}".trim
}
object LMHPullCompanion extends ActionCompanion("pull updates to locally installed archives", "lmh pull", "oaf pull"){
  import Action._
  override val addKeywords : Boolean = false
  def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ LMHPull
}


case class LMHPush(spec: List[String]) extends LMHAction with LocalAction {
  def applyActual(entry: LMHHubEntry): Unit = {entry.push}
  def toParseString = s"lmh push ${spec.mkString(" ")}".trim
}
object LMHPushCompanion extends ActionCompanion("push updates to locally installed archives", "lmh push", "oaf push"){
  import Action._
  override val addKeywords : Boolean = false
  def parserActual(implicit state: ActionState) = strs(keyRegEx) ^^ LMHPush
}


case class LMHSetRemote(spec: List[String]) extends LMHAction with LocalAction {
  def applyActual(entry: LMHHubEntry) = entry.fixRemote
  def toParseString = s"lmh setremote ${spec.mkString(" ")}".trim
}
object LMHSetRemoteCompanion extends ActionCompanion("fix all remote urls for installed archives", "lmh setremote", "oaf setremote") {
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
