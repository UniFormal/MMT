package info.kwarc.mmt.api.archives.lmh.MathHub

import info.kwarc.mmt.api.archives.lmh.LMHHubEntry
import info.kwarc.mmt.api.utils.{File, stringToList}

/** implements MathHub installation functionality */
trait MathHubInstaller {
  self: MathHub =>

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
      val success = git(local, "clone", rp, lp.toString).success
      if (!success) {
        if (lp.exists) {
          log("git failed, deleting " + lp)
          lp.deleteDir
        } else {
          log("git failed")
        }
        return None
      }

      log("git clone successful")

      // checkout specific version, fail silently with a simple log message
      if(version.isDefined){
        log(s"checking out ${version.get}")
        val vSuccess = git(lp, "checkout", "-f", version.get).success
        if (!vSuccess) {
          log("checkout failed, Local version may differ from requested version. ")
        }
      }
    }

    Some(new MathHubEntry(lp))
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
      Some(new MathHubEntry(lp))
    } catch {
      case _: Exception =>
        log(s"download failed, aborting")
        None
    } finally {
      zip.delete
    }
  }

  private def installActual(id : String, version: Option[String], enforceVersion: Boolean) : Option[MathHubEntry] = {
    // resolve the correct version of the archive, unless we enforce the current one
    val actualVersion = if(enforceVersion) version else versioning(id, version)

    log(s"Attempting to install archive $id (version=$version, actualVersion=$actualVersion)")

    // if the archive is already installed, we should not install it again
    // however we return it, so that we can scan dependencies again
    val entry = getEntry(id)
    if(entry.isDefined){
      log(s"$id has already been installed at ${entry.get.root}, re-scanning dependencies. ")
      return entry.map({e => new MathHubEntry(e.root) })
    }

    // first try to install via git
    val gitInstall = installGit(id, actualVersion)

    // if that has failed, try to download normally
    if(gitInstall.isEmpty) {
      installGet(id, actualVersion)

    // and if that has failed also, then return the repository
    } else {
      gitInstall
    }
  }

  def installEntry(id: String, version: Option[String], enforceVersion: Boolean = false, recursive: Boolean = false, visited: List[LMHHubEntry] = Nil): Option[LMHHubEntry] = {

    // if we visited this entry already, we do not want to re-install it
    // to prevent infinite recursion into this archive
    if(visited.exists(_.id.matches(id))){
      log(s"$id has already been installed and re-scanned for dependencies, skipping. ")
      return None
    }

    installActual(id, version, enforceVersion = enforceVersion) match {
      case Some(entry: MathHubEntry) =>

        // load the entry first
        entry.load()

        if(recursive) {
          // Find the dependencies
          // TODO: Fix legacy lmh using the wrong separator
          val depS = entry.archive.properties.getOrElse("dependencies", "")
          val deps = if (depS.contains(",")) stringToList(depS, ",").map(_.trim) else stringToList(depS)

          // and install each of the sub-entries, and keeping track of all the archives we have already visited
          // failing silently
          deps foreach {d =>
            logGroup {
              log(s"installing dependency ${deps.mkString("")} of $id")
              installEntry(d, version = None, enforceVersion = enforceVersion, recursive = true, visited = entry :: visited)
            }
          }
        }
        Some(entry)
      case None =>
        None
    }
  }
}