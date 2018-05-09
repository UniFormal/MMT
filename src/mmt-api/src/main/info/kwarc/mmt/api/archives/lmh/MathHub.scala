package info.kwarc.mmt.api.archives.lmh

import info.kwarc.mmt.api._
import utils._
import frontend._
import info.kwarc.mmt.api.archives.Archive

import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
  * Represents a (mutable) MathHub instance
  * @param controller the controller to use with this MathHub instance
  * @param local the local path of the MathHub instance
  * @param remote the remote URI of the MathHUb instance
  * @param https should we use https or ssh for cloning?
  */
class MathHub(val controller: Controller, var local: File, var remote: URI, var https: Boolean = true) extends LMHHub {

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

  // GETTING and LOADING existing entries

  /** represents a single entry of a MathHub controller */
  abstract class MathHubEntry(val root: File) extends LMHHubEntry {
    val hub: MathHub = MathHub.this

    def version: Option[String] = hub.git(root, "show-ref", "HEAD") match {
      case ShellCommand.Success(op) => Some(op.split(" ").head)
      case _ => None
    }
    def pull: Boolean = {
      log(s"pulling $id")
      hub.git(root, "pull").success
    }
    def push: Boolean = {
      log(s"pushing $id")
      hub.git(root, "push").success
    }
    def setRemote(remote : String) : Boolean = hub.git(root, "remote", "set-url", "origin", remote).success
  }

  object MathHubEntry {
    /** creates a new MathHubEntry of the most specific kind and loads it */
    def apply(root: File) : MathHubEntry = {
      // try to load an archive
      val ae = new MathHubArchiveEntry(root)
      try {
        ae.load()
        return ae
      } catch {
        case e: NotLoadableArchiveEntry =>
      }

      // try to load a group
      val ge = new MathHubGroupEntry(root)
      try {
        ge.load()
        return ge
      } catch {
        case e: NotLoadableGroupEntry =>
      }

      new MathHubDirectoryEntry(root)
    }
  }

  class MathHubDirectoryEntry(override val root: File) extends MathHubEntry(root) with LMHHubDirectoryEntry
  class MathHubArchiveEntry(override val root: File) extends MathHubEntry(root) with LMHHubArchiveEntry
  class MathHubGroupEntry(override val root: File) extends MathHubEntry(root) with LMHHubGroupEntry

  /** gets a single entry from the MathHub root */
  def getEntry(root: File): Option[MathHubEntry] = {
    // TODO: Check if we are under git control
    if(root.exists){
      Some(MathHubEntry(root))
    } else None
  }

  override def getEntry(id: String): Option[MathHubEntry] =
    // the implementation of the super method recurses into getEntry(root: File)
    // thus we can safely type cast here
    super.getEntry(id).map(_.asInstanceOf[MathHubEntry])

  def getEntry(archive: Archive) : Option[MathHubArchiveEntry] = {
    val ae = new MathHubArchiveEntry(archive.root)
    try {
      ae.load()
      Some(ae)
    } catch {
      case e: NotLoadableArchiveEntry => None
    }
  }

  // code for listing entries

  /** find all the archives known to the controller */
  def entries_ : List[MathHubEntry] = {
    val folders = new ListBuffer[File]

    // add all the folders from all backend archives
    controller.backend.getArchives.foreach {a => folders += a.root}

    // add all the level-2 subdirectories
    local.children.foreach { c =>
      if(c.isDirectory) {
        c.children.filter(_.isDirectory).foreach(d => folders += d)
      }
    }

    // get all the archive instances
    folders.result().distinct.map(root => MathHubEntry(root))
  }

  /** tries to get some json form a given URL */
  private def get_json(url: URI) : Option[JSON] = {
    log(s"fetching $url")
    val attempt = Try(io.Source.fromURL(url.toString))
    if (attempt.isFailure) None else Some(attempt.get.toBuffer.mkString).map(JSON.parse)
  }
  private def getAvailablePage(page : Int) : List[String] = {
    get_json(api_(page)).map(a => a.asInstanceOf[JSONArray].values.map( e => {
      e.asInstanceOf[JSONObject].getAsString("path_with_namespace")
    }).toList).getOrElse(Nil)
  }
  
  /** return a list of available pages, at most 10 */
  protected def available_() : List[String] = Try({
    var buffer: List[String] = Nil
    var i = 1
    while (i <= 10 && {
      val newEntries = getAvailablePage(i)
      buffer :::= newEntries
      i += 1
      newEntries.nonEmpty
    }) {}
    buffer.distinct.sorted
  }).getOrElse(Nil)

  /** creates a new repository of the given ID */
  def createEntry(id: String): Option[LMHHubEntry] = {
    // find the local root folder to create the repository in
    // and return nothing it nothing is created
    val root  = localPath(id)
    if (root.exists){
      return None
    }
    // and return the repository
    root.mkdirs
    // create all the files needed by the repository
    // TODO: Build a proper templating system into the resources folder
    File.WriteLineWise(root / "META-INF" / "MANIFEST.MF", List(
      s"id: $id",
      s"narration-base: http://mathhub.info/$id"
    ))
    File.write(root / "source" / "README.txt", "commit your sources in this folder")
    // create a git repository
    git(root, "init")
    git(root, "add", (root / "META-INF" / "MANIFEST.MF").toString)
    git(root, "add", (root / "source" / "README.txt").toString)
    git(root, "commit", "-m", "\"automatically created by MMT\"")
    git(root, "remote", "add", "origin", remoteURL(id))

    // create and load the actual entry
    Some(MathHubEntry(root))
  }

  // code for installing new entries
  
  /** installs a new entry by cloning it from the server */
  def installEntry(id: String, version: Option[String], recursive: Boolean = false, visited: List[LMHHubEntry] = Nil): Option[LMHHubEntry] = {
    // if we visited this entry already, we do not want to re-install it
    // to prevent infinite recursion into this archive
    if(visited.exists(_.id.matches(id))){
      log(s"$id has already been installed and re-scanned for dependencies, skipping. ")
      return None
    }

    installActual(id, version) match {
      case Some(entry: MathHubEntry) =>
        if (recursive) {
          entry match {
            case ae: MathHubArchiveEntry => {
              // Find the dependencies

              log(s"checking for dependencies of $id")

              // the corresponding meta-inf repository
              val metainf = ae.group + "/meta-inf"

              // the dependencies
              val depS = ae.archive.properties.getOrElse("dependencies", "")
              val deps = if (depS.contains(",")) stringToList(depS, ",").map(_.trim) else stringToList(depS)

              // and install each of the sub-entries, and keeping track of all the archives we have already visited
              // failing silently
              (metainf :: deps).distinct foreach {d =>
                logGroup {
                  log(s"installing dependency ${deps.mkString("")} of $id")
                  installEntry(d, version = None, recursive = true, visited = entry :: visited)
                }
              }
            }
            case _ =>
              log(s"$id is not an archive, skipping dependency check")
          }
        }
        Some(entry)
      case None =>
        None
    }
  }
 
  private def installGit(id : String, version: Option[String]) : Option[MathHubEntry] = {
    val lp = localPath(id)
    val rp = remoteURL(id)
    log(s"attempting to 'git clone $rp' into '$lp'")
    // if the path exists, finish
    if (lp.exists) {
      if(version.isDefined){
        log("target directory exists, skipping. Local version may differ from requested version. ")
      } else {
        log("target directory exists, skipping")
      }
    } else {
      // try to clone the repository or fail
      log(s"trying to clone $id")
      val success = git(local, "clone", rp, id).success
      if (!success) {
        if (lp.exists) {
          logError(s"git clone $rp failed, deleting $lp")
          lp.deleteDir
        } else {
          logError(s"git clone $rp failed")
        }
        return None
      }
      log("git clone successful")
      // checkout specific version, fail silently with a simple log message
      if(version.isDefined){
        log(s"checking out ${version.get}")
        val vSuccess = git(lp, "checkout", "-f", version.get).success
        if (!vSuccess) {
          logError("checkout failed, Local version may differ from requested version. ")
        }
      }
    }
    Some(MathHubEntry(lp))
  }
  
  private def installGet(id: String, version: Option[String]) : Option[MathHubEntry] = {
    log(s"trying to install $id (version $version) via download")
    val lp = localPath(id)
    val zip = local.addExtension("zip")
    val url = download_(id, version)
    try {
      log(s"attempting to download '$url' into '$zip'")
      File.download(url, zip)
      log(s"unpacking '$url' into '$lp'")
      File.unzip(zip, lp, skipRootDir = true)
      Some(MathHubEntry(lp))
    } catch {
      case _: Exception =>
        logError(s"download of '$url' failed, aborting")
        None
    } finally {
      zip.delete
    }
  }

  private def installActual(id : String, version: Option[String]) : Option[MathHubEntry] = {
    log(s"Attempting to install archive $id (version=$version)")

    // if the archive is already installed, we should not install it again
    // however we return it, so that we can scan dependencies again
    val entry = getEntry(id)
    if(entry.isDefined){
      log(s"$id has already been installed at ${entry.get.root}, re-scanning dependencies. ")
      return Some(MathHubEntry(entry.get.root))
    }
    // first try to install via git
    val gitInstall = installGit(id, version)
    // if that has failed, try to download normally
    gitInstall orElse {
      installGet(id, version)
    }
  }
}

object MathHub {
  val defaultURL = URI("https://gl.mathhub.info/")
}
