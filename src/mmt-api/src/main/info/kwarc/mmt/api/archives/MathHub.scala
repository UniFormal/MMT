package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api._
import utils._
import frontend._

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
  protected lazy val git: Git = utils.OS.git

  // PATHS
  def remoteURL(id : String): String = if(https) {
    "https://" + remote.authority.getOrElse("") + "/" + id + ".git"
  } else {
    "git@" + remote.authority.getOrElse("") + ":" + id + ".git"
  }
  /*
  protected def download_(id : String, version: Option[String]): URI = {
    (remote / id / "repository" / "archive.zip") ? s"ref=${version.getOrElse("master")}"
  }*/
  protected def api_(page: Int): URI = {
    (remote / "api" / "v4" / "projects") ? s"per_page=100&page=$page"
  }
  protected def groupmf_(name: String): URI = {
    remote / name / "meta-inf" / "raw" / "master" / "GROUP_MANIFEST.MF"
  }
  def localPath(id : String) : File = {
    val ret = (local / id).canonical
    // make sure dots in id do not cause trouble
    if (!(local <= ret)) throw GeneralError("local path escapes root path: " + ret)
    ret
  }
  /** checks if a group exists on the remote MathHub */
  def hasGroup(name: String): Boolean = Try(io.Source.fromURL(groupmf_(name).toString)).isSuccess

  // GETTING and LOADING existing entries

  /** represents a single entry of a MathHub controller */
  abstract class MathHubEntry(val root: File) extends LMHHubEntry {
    val hub: MathHub = MathHub.this

    def physicalVersion: Option[String] = hub.git(root, "show-ref", "HEAD") match {
      case ShellCommand.Success(op) => Some(op.split(" ").head.trim)
      case _ => None
    }
    def logicalVersion: Option[String] = hub.git(root, "symbolic-ref", "HEAD") match {
      case ShellCommand.Success(op) if op.startsWith("refs/heads/") => Some(op.substring("refs/heads/".length).trim)
      case _ => None
    }
    def fetch: Boolean = {
      log(s"fetching $id")
      hub.git(root, "fetch", "--all").success
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

      debug(s"trying to load entry at ${root.toJava.toString}")

      logGroup {
        try {
          val ae = new MathHubArchiveEntry(root)
          ae.load()
          debug(s"found archive '${ae.id}' in $root")
          return ae
        } catch {
          case e: NotLoadableArchiveEntry => report("debug", s"lmh failed to add archive in $root")
        }

        // try to load a group
        try {
          val ge = new MathHubGroupEntry(root)
          ge.load()
          debug(s"found group '${ge.group}' in $root")
          return ge
        } catch {
          case e: NotLoadableGroupEntry => report("debug", s"lmh failed to add group in $root")
        }

        debug(s"falling back to Directory at $root")
        new MathHubDirectoryEntry(root)
      }
    }
  }

  class MathHubDirectoryEntry(root: File) extends MathHubEntry(root) with LMHHubDirectoryEntry
  class MathHubArchiveEntry(root: File) extends MathHubEntry(root) with LMHHubArchiveEntry
  class MathHubGroupEntry(root: File) extends MathHubEntry(root) with LMHHubGroupEntry

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

  /** logs a debug message */
  private def debug(message: String): Unit ={
    report("debug", s"lmh $message")
  }

  /** find all the archives known to the controller */
  def installedEntries : List[MathHubEntry] = {
    val folders = new ListBuffer[File]

    debug("scanning for archives")

    logGroup {
      // add all the folders from all backend archives
      controller.backend.getArchives.foreach {a => debug(s"adding loaded archive candidate ${a.root.toJava.toString}"); folders += a.root}

      // add all the level-2 subdirectories
      local.children.foreach { c =>

        if(c.isDirectory) {
          debug(s"scanning for archives in ${c.toJava.toString}")
          c.children.filter(_.isDirectory).foreach(d => {debug(s"adding folder candidate ${d.toJava.toString}"); folders += d})
        }
      }
    }

    // get all the archive instances
    folders.result().distinct.map(root => MathHubEntry(root))
  }

  /** tries to get some json form a given URL */
  private def get_json(url: URI) : Option[JSON] = {
    log(s"fetching $url")
    val attempt = Try(io.Source.fromURL(url.toString)("ISO-8859-1"))
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
  def installEntry(id: String, version: Option[String], recursive: Boolean = false, update: Boolean = true): Option[LMHHubEntry] = {
    installEntryInternal(id, version, recursive, update, Nil)._2
  }


  /** installs a list of new entries at once by cloning them from the server */
  def installEntries(entries: List[(String, Option[String])], recursive: Boolean = false, update: Boolean = true): Unit = {
    var visits: List[LMHHubEntry] = Nil
    entries.foreach({
      case (id, version) =>
        visits = installEntryInternal(id, version, recursive, update, visits)._1
    })
  }

  private def installEntryInternal(id: String, version: Option[String], recursive: Boolean, update: Boolean, visited: List[LMHHubEntry]): (List[LMHHubEntry], Option[LMHHubEntry]) = {
    // everything we have visited so far
    var visits: List[LMHHubEntry] = visited

    // if we visited this entry already, we do not want to re-install it
    // to prevent infinite recursion into this archive
    if(visited.exists(_.id.matches(id))){
      log(s"$id has already been installed and re-scanned for dependencies, skipping. ")
      return (visits, None)
    }

    // install the actual archive
    installActual(id, version, update) match {
      case Some(entry: MathHubEntry) =>
        // we have now visited ourself
        visits = entry :: visits

        // and if we are installing recursively, add the archives
        if (recursive) {
          entry match {
            case ae: MathHubArchiveEntry => {
              // Find the dependencies
              log(s"checking for dependencies of $id")

              // and install each of the sub-entries, and keeping track of all the archives we have already visited
              // failing silently
              ae.dependencies foreach {d =>
                logGroup {
                  log(s"installing dependency $d of $id")
                  visits = installEntryInternal(d, version, recursive = true, update = update, visited = visits)._1
                }
              }
            }
            case _ =>
              log(s"$id is not an archive, skipping dependency check")
          }
        }
        (entry :: visits, Some(entry))
      case None =>
        (visits, None)
    }
  }

  private def installActual(id : String, version: Option[String], update: Boolean) : Option[MathHubEntry] = {
    log(s"Attempting to install archive $id" + version.map("@" + _ ).getOrElse(""))

    // if the archive is already installed, we should not install it again
    // however we return it, so that we can scan dependencies again
    val entry = getEntry(id)
    if(entry.isDefined){
      val _entry = MathHubEntry(entry.get.root)
      log(s"$id already installed in ${_entry.root}" + _entry.version.map("@" + _ ).getOrElse(""))
      if(update) {
        installUpdateEntry(_entry, version)
      } else {
        log(s"$id has already been installed in ${entry.get.root}, re-scanning dependencies. ")
      }
      return Some(_entry)
    }
    // try to install via git
    val gitInstall = installGit(id, version)
    // there used to be code here that downloaded the zip
    gitInstall orElse {
      logError(s"installation has failed, please make sure that git is installed and try again. ")
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
      val cloneResult = git(local, "clone", rp, id)
      if (!cloneResult.success) {
        logError(s"git clone $rp failed")
        logError(s"Error: `${cloneResult.toString}`")
        if (lp.exists) {
          logError(s"Target directory $lp already exists, perhaps that was the source of the error, deleting $lp")
          lp.deleteDir
        }
        return None
      }
      log("git clone successful")
      // checkout specific version, fail silently with a simple log message
      if(version.isDefined){
        log(s"checking out ${version.get}")
        val vSuccess = git(lp, "checkout", "-f", version.get).success
        if (!vSuccess) {
          logError(s"checkout of $id failed, Local version may differ from requested version. ")
        }
      }
    }
    Some(MathHubEntry(lp))
  }

  /*
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
  }*/

  private def installUpdateEntry(entry: MathHubEntry, version: Option[String]): Unit = {
    val id = entry.id

    // fetch the archive
    Try(entry.fetch).toOption.getOrElse({log(s"failed to fetch $id, ignoring. ")})

    // if we have a given version, force checkout that version
    if(version.isDefined){
      log(s"checking out ${version.get}")
      val vSuccess = git(entry.root, "checkout", "-f", version.get).success
      if (!vSuccess) {
        logError(s"checkout of $id failed, Local version may differ from requested version. ")
      }
    }

    // then pull (if we are on a semantic version)
    log(s"updating ${entry.root}")
    Try(entry.pull).toOption.getOrElse({log(s"failed to pull $id, ignoring. ")})
  }


}

object MathHub {
  val defaultURL = URI("https://gl.mathhub.info/")
}
