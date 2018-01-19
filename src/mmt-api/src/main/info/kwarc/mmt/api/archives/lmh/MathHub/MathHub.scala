package info.kwarc.mmt.api.archives.lmh.MathHub

import info.kwarc.mmt.api.GeneralError
import info.kwarc.mmt.api.archives.lmh._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.{File, OS, URI, Windows}

class MathHub(val controller: Controller, val local: File, val remote: URI, val https: Boolean = true)
  extends LMHHub with MathHubInstaller with MathHubCreator with MathHubLister
{
  @deprecated("use local instead, to prevent compilation errors", "")
  val root: File = local
  /** implements git */
  protected val git: Git = OS.detect match {case Windows => new WindowsGit() case _ => UnixGit }

  // PATHS

  def remoteURL(id : String): String = if(https) {
    "https://" + remote.authority.getOrElse("") + "/" + id + ".git"
  } else {
    "git@" + remote.authority.getOrElse("") + ":" + id + ".git"
  }
  protected def download_(id : String, version: Option[String]): URI = {
    (remote / id / "repository" / "archive.zip") ? s"ref=${version.getOrElse("master")}"
  }
  protected def api_(page: Int): URI = {
    (remote / "api" / "v4" / "projects") ? s"per_page=100&page=$page"
  }
  def localPath(id : String) : File = {
    val ret = (local / id).canonical
    // make sure dots in id do not cause trouble
    if (!(local <= ret)) throw GeneralError("local path escapes root path: " + ret)
    ret
  }

  /** represents a single entry of a MathHub controller */
  class MathHubEntry(val root: File) extends LMHHubEntry {
    val hub: MathHub = MathHub.this

    def version: Option[String] = hub.git(root, "show-rev", "HEAD").successOption
    def pull: Boolean = hub.git(root, "pull").success
    def push: Boolean = hub.git(root, "push").success
    def setRemote(remote : String) : Boolean = hub.git(root, "remote", "set-url", "origin", remote).success
  }

  /** gets a single entry from the MathHub root */
  def getEntry(root: File): Option[MathHubEntry] = {
    // if the folder exists and is a valid git root
    // TODO: Check if we are in the **root** folder of a repository
    if(root.exists && git(root, "rev-parse").success){
      Some(new MathHubEntry(root))
    } else None
  }
}

object MathHub {
  val defaultURL = URI("https://gl.mathhub.info/")
}