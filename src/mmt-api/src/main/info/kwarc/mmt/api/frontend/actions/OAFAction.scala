package info.kwarc.mmt.api.frontend.actions

import info.kwarc.mmt.api.GeneralError
import info.kwarc.mmt.api.frontend.{Controller, MMTConfig, OAFConf}
import info.kwarc.mmt.api.utils.{File, URI, stringToList}

/** Shared base class for Actions relating to OAF (lmh) */
sealed abstract class OAFAction extends ActionImpl {}

/** configure the oaf root
  *
  * concrete syntax: oaf close path:STRING
  */
case class OAFRoot(path: String, https: Boolean) extends OAFAction {
  def apply(controller: Controller): Unit = {
    val config = new MMTConfig
    config.addEntry(OAFConf(File(path), https, None))
    controller.loadConfig(config, true)
  }
  override def toParseString = s"oaf root $path ${if(https) "https" else "ssh"}"
}
object OAFRootCompanion extends ActionCompanionImpl[OAFRoot]("clone an archive from a remote OAF", "oaf root"){
  import Action._
  def parserActual(implicit state: ActionState) = str ~ (("ssh" | "https")?) ^^ {
    case p ~ Some(https) => OAFRoot(p, https == "https")
    case p ~ None => OAFRoot(p, true)
  }
}


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
  * concrete syntax: oaf close id:STRING [version:String]
  */
case class OAFClone(id: String, version: Option[String]) extends OAFAction {
  def apply(controller: Controller): Unit = controller.cloneArchive(id, version)
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
  def apply(controller: Controller): Unit = {
    controller.report.report("user", controller.getOAFOrError.version(id))
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
object OAFSetRemoteCompanion extends ActionObjectCompanionImpl[OAFSetRemote.type]("sets all remotes for the remote OAF", "oaf setremote")

/** helper functions for [[OAFAction]]s */
trait OAFActionHandling {
  self: Controller =>

  /** gets an [OAF] instance or throws an error */
  private[actions] def getOAFOrError = getOAF.getOrElse {
    throw GeneralError("no OAF configuration entry found")
  }

  /**
    * clones an OAF Archive (either a specific version, or the current one recursively)
    * @param archive Name of archive to clone
    * @param version If provided, clone the specific version of the archive. If not, clone it recursively
    */
  def cloneArchive(archive: String, version: Option[String] = None) {
    val oaf = getOAFOrError
    report("user", s"trying to clone $archive${version.map("@" +).getOrElse("")}")

    val lc = oaf.clone(archive, version) getOrElse {
      logError("cloning failed, trying to download")
      oaf.download(archive, version)
    }.getOrElse {
      logError("downloading failed, giving up")
      return
    }

    val archs = addArchive(lc)

    if(version.isEmpty){
      archs foreach {a =>
        val depS = a.properties.getOrElse("dependencies", "")
        // TODO lmh falsely uses , as separator instead of space
        val deps = if (depS.contains(",")) stringToList(depS, ",").map(_.trim) else stringToList(depS)
        deps foreach {d => cloneArchive(URI(d).pathAsString)}
      }
    }
  }
}