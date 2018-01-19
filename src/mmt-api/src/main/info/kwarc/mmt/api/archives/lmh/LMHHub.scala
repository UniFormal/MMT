package info.kwarc.mmt.api.archives.lmh

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.frontend.{Controller, Logger, Report}
import info.kwarc.mmt.api.utils.File

import scala.util.matching.Regex

/** represents a hub of archives */
abstract class LMHHub extends Logger {
  def controller: Controller

  val logPrefix = "lmh"
  protected def report = controller.report

  /** find all locally installed repositories */
  protected def entries_ : List[LMHHubEntry]

  /** find all repositories given a specification */
  def entries(spec: String*): List[LMHHubEntry] = if(spec.nonEmpty) entries_.filter(e => spec.exists(e.matches)) else entries_

  /** find a list of remotely available entries (if any) */
  protected def available_(): List[String]

  /** resolves a list of available entries and returns pairs (id, version) */
  def available(spec: String*): List[(String, Option[String])] = {

    // this function will perform the match case insensitive
    // however the spec returned will be case sensitive
    // because of their case sensitive path on the file system

    // furthermore, we can not rely on available_() returning all existing entries
    // as some archives may be private (or the API may be broken)

    // we will need this, so we should cache the available instances
    lazy val all = available_()


    spec.flatMap({ s =>

      // split off a version separated by an '@', if given, e.g:
      // 'repo@version' => ('repo', Some('version'))
      // 'repo' => ('repo', None)
      val idx = s.lastIndexOf("@")
      val (pattern, version) = if (idx > 0) {
        (s.substring(0, idx - 1), Some(s.substring(idx + 1)))
      } else {
        (s, None)
      }


      // if we have a pattern that needs resolving, i.e. it is non-trivial
      // we should search for it
      if (LMHHub.isNonStatic(pattern)) {
        all.filter(i => LMHHub.matchesComponents(pattern.toLowerCase, i.toLowerCase)).map(r => (r, version))

      // if the pattern is static, it points to a single archive
      } else {
        all.find(_.toLowerCase == pattern.toLowerCase) match {
          // try to find the given archive in the known list of archives
          // so that we can get the case right
          case Some(p) => List((p, version))

          // if it is not known, we will take and try the pattern as is
          // (e.g allow private archives which aren't listed to be installed)
          case None => List((pattern, version))
        }
      }
    }).distinct.toList
  }

  /** gets a single locally installed archive */
  def getEntry(id : String) : Option[LMHHubEntry] = controller.backend.getArchive(id).flatMap(getEntry)
  def getEntry(archive: Archive) : Option[LMHHubEntry] = getEntry(archive.root)
  def getEntry(root: File) : Option[LMHHubEntry]

  /** creates a new archive */
  def createEntry(id: String): Option[LMHHubEntry]

  /** installs a new archive from the remote */
  def installEntry(id: String, version: Option[String], recursive: Boolean = false, visited: List[LMHHubEntry] = Nil) : Option[LMHHubEntry]

  /** get the default remote url for a repository with a given id */
  def remoteURL(id: String) : String
  /** get the default local path for a repository with the given id */
  def localPath(id: String) : File
}

/** represents a single archive inside of an [[LMHHub]] that is installed on disk */
abstract class LMHHubEntry extends Logger {

  /** the [[MathHub]] this [[LMHHubEntry]] belongs to */
  val hub : LMHHub
  protected def controller: Controller = hub.controller

  val logPrefix: String = "lmh"
  def report: Report = controller.report

  /** returns the [[Archive]] instance belonging to this local ArchiveHub entry */
  lazy val archive : Archive = {
    controller.backend.getArchive(root).getOrElse {
      controller.addArchive(root).head
    }
  }
  /** get the id of this archive */
  lazy val id: String = archive.id

  // Things to be implemented

  /** the local root of this archive entry */
  val root: File

  /** check if this archive matches a given spec */
  def matches(spec : String): Boolean = LMHHub.matchesComponents(spec, id)

  /** push the newest version of this archive to the remote */
  def push: Boolean

  /** pull the newest version of this archive from the remote */
  def pull: Boolean

  /** reset the remote url of this archive to a given one */
  def setRemote(remote : String) : Boolean

  /** fix the remote url of the archive */
  def fixRemote: Boolean = setRemote(hub.remoteURL(id))

  /** returns the version of an installed archive */
  def version: Option[String]
}

object LMHHub {
  final private val groupDepth = 2

  /** checks if a pattern is non-static, i.e. if it needs evaluation */
  def isNonStatic(pattern: String, separator: String = "/"): Boolean = {
    pattern.contains("*") || pattern.split("/").length != groupDepth
  }

  /** checks if a pattern matches an instance */
  def matchesComponents(pattern: String, instance: String, separator: String = "/") : Boolean = {
    val patternParts = pattern.split(separator).toList
    val instanceParts = instance.split(separator).toList

    if(patternParts.length > instanceParts.length) {
      return false
    }

    for ( (i, p) <- instanceParts.take(patternParts.length) zip patternParts) {
      if(!matches(p, i)){
        return false
      }
    }

    true
  }

  /** check if a single component of a pattern matches a single component of an instance */
  private def matches(pattern: String, instance: String): Boolean = {
    if(pattern == "") return true // an empty pattern always matches
    val regex = pattern.split("\\*").map(Regex.quote).mkString("(.*)") + (if(pattern.endsWith("*")) {"(.*)"} else "")
    regex.r.pattern.matcher(instance).matches()
  }
}