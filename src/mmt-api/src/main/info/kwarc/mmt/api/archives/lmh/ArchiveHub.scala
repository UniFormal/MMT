package info.kwarc.mmt.api.archives.lmh

import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.frontend.{Controller, Logger, Report}
import info.kwarc.mmt.api.utils.File

/** represents a hub of archives */
abstract class ArchiveHub extends Logger {
  def controller: Controller

  val logPrefix = "hub"
  protected def report = controller.report

  /** find all locally installed repositories */
  protected def entries_ : List[ArchiveHubEntry]

  /** find all repositories given a specification */
  def entries(spec: String*): List[ArchiveHubEntry] = if(spec.nonEmpty) entries_.filter(e => spec.exists(e.matches)) else entries_

  /** find all remotely available entries (if any) */
  def available: Option[List[String]] = None

  /** gets a single locally installed archive */
  def getEntry(id : String) : Option[ArchiveHubEntry] = controller.backend.getArchive(id).flatMap(getEntry)
  def getEntry(archive: Archive) : Option[ArchiveHubEntry] = controller.backend.getArchive(archive.root).flatMap(getEntry)
  def getEntry(root: File) : Option[ArchiveHubEntry]

  /** creates a new archive */
  def createEntry(id: String): Option[ArchiveHubEntry]

  /** installs a new archive from the remote */
  def installEntry(id: String, version: Option[String], recursive: Boolean = false) : Option[ArchiveHubEntry]

  /** pulls a given set of repositories from the remote */
  def pull(spec: String*): List[Boolean] = entries(spec :_*).map(_.pull)
  /** pushes a given set of repositories from the remote */
  def push(spec: String*): List[Boolean] = entries(spec :_*).map(_.push)

  /** get the default remote url for a repository with a given id */
  protected def remote_(id: String) : String
  /** get the default local path for a repository with the given id */
  protected def local_(id: String) : File

  /** reset the remotes of a given repository */
  def remote(spec: String*): List[Boolean] = entries(spec :_*).map(e => e.remote(remote_(e.id)))
}

/** represents a single archive inside of an [[ArchiveHub]] that is installed on disk */
abstract class ArchiveHubEntry extends Logger {

  /** the [[MathHub]] this [[ArchiveHubEntry]] belongs to */
  val hub : ArchiveHub
  protected def controller: Controller = hub.controller

  val logPrefix: String = "hub"
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
  def matches(spec : String): Boolean = id == spec // TODO: Support lmh-like regex

  /** push the newest version of this archive to the remote */
  def push: Boolean

  /** pull the newest version of this archive from the remote */
  def pull: Boolean

  /** reset the remote url of this archive to a given one */
  def remote(remote : String) : Boolean

  /** returns the version of an installed archive */
  def version: Option[String]
}