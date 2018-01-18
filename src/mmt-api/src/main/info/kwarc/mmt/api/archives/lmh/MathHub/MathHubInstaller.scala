package info.kwarc.mmt.api.archives.lmh.MathHub

import info.kwarc.mmt.api.utils.{File, stringToList}

/** implements MathHub installation functionality */
trait MathHubInstaller {
  self: MathHub =>

  private def installGit(id : String, version: Option[String]) : Option[MathHubEntry] = {
    log(s"trying to install $id (version $version) via git")

    val lp = local_(id)
    val rp = remote_(id)

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
        }
        return None
      }

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

    val lp = local_(id)
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

  private def installActual(id : String, version: Option[String]) : Option[MathHubEntry] = {
    val gitInstall = installGit(id, version)
    if(gitInstall.isEmpty) {
      installGet(id, version)
    } else {
      gitInstall
    }
  }

  def installEntry(id: String, version: Option[String], recursive: Boolean = false) : Option[MathHubEntry] = installActual(id, version) match {
    case Some(entry: MathHubEntry) =>
      if(recursive) {
        // Find the dependencies
        // TODO: Fix legacy lmh using the wrong separator
        val depS = entry.archive.properties.getOrElse("dependencies", "")
        val deps = if (depS.contains(",")) stringToList(depS, ",").map(_.trim) else stringToList(depS)
        // and install each of the sub-entries, failing silently
        deps foreach {d => installEntry(d, version = None, recursive = true)}
      }
      Some(entry)
    case None =>
      None
  }
}