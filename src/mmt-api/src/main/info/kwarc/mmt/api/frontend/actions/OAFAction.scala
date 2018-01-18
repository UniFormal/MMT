package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.GeneralError
import info.kwarc.mmt.api.frontend.{Controller, MMTConfig, OAFConf}
import info.kwarc.mmt.api.utils.{File, URI, stringToList}

/** Shared base class for Actions relating to OAF (lmh) */
sealed abstract class OAFAction extends ActionImpl {}

/** set the oaf root folder
  *
  * concrete syntax: oaf close path:STRING
  */
case class SetOAFRoot(path: String, https: Boolean) extends OAFAction {
  def apply(implicit controller: Controller): Unit = {
    val config = new MMTConfig
    config.addEntry(OAFConf(File(path), https, None))
    controller.loadConfig(config, true)
  }
  override def toParseString = s"oaf root $path ${if(https) "https" else "ssh"}"
}
object SetOAFRootCompanion extends ActionCompanionImpl[SetOAFRoot]("set the oaf root", "oaf root"){
  import Action._
  def parserActual(implicit state: ActionState) = str ~ (("ssh" | "https")?) ^^ {
    case p ~ Some(https) => SetOAFRoot(p, https == "https")
    case p ~ None => SetOAFRoot(p, true)
  }
}

/** get the oaf root folder */
case object GetOAFRoot extends OAFAction with ResponsiveAction {
  override def toParseString: String = "oaf root"
  def apply(implicit controller: Controller): Unit = {
    controller.getOAF match {
      case Some(mh) => respond(mh.local)
      case None => respond("No oaf root configured")
    }
  }
}
object GetOAFRootCompanion extends ActionObjectCompanionImpl[GetOAFRoot.type]("get the oaf root folder", "oaf root")


/** clone an archive from a remote OAF
  *
  * concrete syntax: oaf close path:STRING
  */
case class OAFInit(path: String) extends OAFAction {
  def apply(implicit controller: Controller): Unit = controller.getOAFOrError.createEntry(path)
  override def toParseString = s"oaf init $path"
}
object OAFInitCompanion extends ActionCompanionImpl[OAFInit]("clone an archive from a remote OAF", "oaf init"){
  import Action._
  def parserActual(implicit state: ActionState) = str ^^ OAFInit
}

/** clone an archive from a remote OAF
  *
  * concrete syntax: oaf close id:STRING [version:String]
  */
case class OAFClone(id: String, version: Option[String]) extends OAFAction {
  def apply(implicit controller: Controller): Unit = controller.getOAFOrError.installEntry(id, version, version.isEmpty)
  def toParseString = s"oaf clone $id${version.map(" " + ).getOrElse("")}"
}
object OAFCloneCompanion extends ActionCompanionImpl[OAFClone]("clone an archive from a remote OAF", "oaf clone"){
  import Action._
  def parserActual(implicit state: ActionState) = str ~ (str?) ^^ { case i ~ v => OAFClone(i, v)}
}

/** shows the current version of a locally installed archive
  *
  * concrete syntax: oaf show id:STRING
  */
case class OAFShow(id: String) extends OAFAction {
  def apply(implicit controller: Controller): Unit = {
    val oaf = controller.getOAFOrError.getEntry(id) match {
      case Some(entry) => entry.version match {
        case Some(version) => controller.report("user", s"$id: $version")
        case None => controller.report("user", s"$id: not versioned")
      }
      case None => controller.report("user", s"$id: not installed")
    }
  }
  def toParseString = s"oaf show $id"
}
object OAFShowCompanion extends ActionCompanionImpl[OAFShow]("shows the current version of a locally installed archive", "oaf show"){
  import Action._
  def parserActual(implicit state: ActionState) = str ^^ OAFShow
}

/** pulls all repositories from remote OAF
  *
  * concrete syntax: oaf pull
  */
case object OAFPull extends OAFAction {
  def apply(implicit controller: Controller) : Unit = controller.getOAFOrError.pull()
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
  def apply(implicit controller: Controller) : Unit = controller.getOAFOrError.push()
  def toParseString = "oaf push"
}
object OAFPushCompanion extends ActionObjectCompanionImpl[OAFPush.type]("pushes all repositories to remote OAF", "oaf push")

/** sets all remotes for the remote OAF
  *
  * concrete syntax: oaf push
  */
case object OAFSetRemote extends OAFAction {
  def apply(implicit controller: Controller) = controller.getOAFOrError.remote()
  def toParseString = "oaf setremote"
}
object OAFSetRemoteCompanion extends ActionObjectCompanionImpl[OAFSetRemote.type]("sets all remotes for the remote OAF", "oaf setremote")

/** helper functions for [[OAFAction]]s */
trait OAFActionHandling {
  self: Controller =>

  /** gets an [OAF] instance or throws an error */
  private[actions] def getOAFOrError = getOAF.getOrElse {
    throw GeneralError("no OAF configuration entry found")
  }
}