package info.kwarc.mmt.api.archives.lmh

import info.kwarc.mmt.api
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.frontend.{Controller, Logger, Report}
import info.kwarc.mmt.api.utils.{File, stringToList}

import scala.collection.mutable
import scala.util.matching.Regex

/** represents a hub of archives */
abstract class LMHHub extends Logger {
  def controller: Controller

  val logPrefix = "lmh"
  protected def report = controller.report

  /** find all locally installed entries */
  protected def entries_ : List[LMHHubEntry]

  /** find all repositories given a specification */
  def entries(spec: String*): List[LMHHubEntry] = if (spec.nonEmpty) entries_.filter(e => spec.exists(e.matches)) else entries_
  /** finds all archive entries available locally */
  def archiveEntries: List[LMHHubArchiveEntry] = entries_.collect({case a: LMHHubArchiveEntry => a})
  /** finds all group entries available locally */
  def groupEntries: List[LMHHubGroupEntry] = entries_.collect({case g: LMHHubGroupEntry => g})
  /** finds all directory entries available locally */
  def dirEntries: List[LMHHubDirectoryEntry] = entries_.collect({case d: LMHHubDirectoryEntry => d})

  /** checks if a group exists remotely */
  def hasGroup(name: String): Boolean

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
        (s.substring(0, idx), Some(s.substring(idx + 1)))
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
  def getEntry(id : String) : Option[LMHHubEntry] = {
    controller.backend.getArchive(id) match {
      case Some(a) => getEntry(a)
      case None => {
        val path = localPath(id)
        if(path.exists) {
          getEntry(path)
        } else {
          None
        }
      }
    }
  }
  def getEntry(archive: Archive) : Option[LMHHubArchiveEntry]
  def getEntry(root: File) : Option[LMHHubEntry]

  /** creates a new archive */
  def createEntry(id: String): Option[LMHHubEntry]

  /**
    * Install a new archive from the remote
    * @param id ID of archive to install
    * @param version Optional version to be installed
    * @param recursive If set to false, do not install archive dependencies
    * @return the newly installed entry
    */
  def installEntry(id: String, version: Option[String], recursive: Boolean = false) : Option[LMHHubEntry]

  /**
    * Same as installEntry, but optimised for multiple entries at once
    * @param entries
    * @param recursive
    */
  def installEntries(entries: List[(String, Option[String])], recursive: Boolean = false)

  /** get the default remote url for a repository with a given id */
  def remoteURL(id: String) : String
  /** get the default local path for a repository with the given id */
  def localPath(id: String) : File
}

/** represents a single git archive inside an [[LMHHub]] that is installed on disk */
trait LMHHubEntry extends Logger {
  /** the [[MathHub]] this [[LMHHubEntry]] belongs to */
  val hub : LMHHub
  protected def controller: Controller = hub.controller
  val logPrefix: String = "lmh"
  def report: Report = controller.report

  /** the name of the group of this entry */
  lazy val group: String = id.split("/").toList.headOption.getOrElse("")
  /** the name of this archive */
  lazy val name: String = id.split("/").toList.lastOption.getOrElse("")

  // Things to be implemented

  /** loads the LMHHubEntry or throw an error if it is invalid */
  def load(): Unit
  /** the id of this archive entry */
  val id: String
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
  /** returns the physical version (a.k.a commit hash) of an installed archive */
  def physicalVersion: Option[String]
  /** returns the logical version (a.k.a branch) of an installed archive */
  def logicalVersion: Option[String]
  /** gets the version of an installed archive, a.k.a. the branch of the git commit hash */
  def version: Option[String] = logicalVersion.map(Some(_)).getOrElse(physicalVersion)
}

/** Represents a simple LMHHub Directory entry */
trait LMHHubDirectoryEntry extends LMHHubEntry {
  def load(): Unit = {}

  lazy val id: String = (root / "..").canonical.name + "/" + root.canonical.name
}

/** represents a single archive inside an [[LMHHub]] that is installed on disk */
trait LMHHubArchiveEntry extends LMHHubDirectoryEntry {
  /** returns the [[Archive]] instance belonging to this local ArchiveHub entry */
  def archive : Archive = {
    load()
    controller.backend.getArchive(root).get
  }
  /** loads this archive into the controller (if not done already) */
  override def load() {
    controller.backend.getArchive(root).getOrElse {
      controller.addArchive(root)
    }

    if(controller.backend.getArchive(root).isEmpty) {
      throw NotLoadableArchiveEntry(root)
    }
  }

  /** get the id of this archive */
  override lazy val id: String = archive.id

  /** the list of dependencies of this archive */
  def dependencies: List[String] = {
    val string = archive.properties.getOrElse("dependencies", "").replace(",", " ")
    // check if we have a meta-inf repository, and if yes install it
    val deps = (if(hub.hasGroup(group)) List(group + "/meta-inf") else Nil) ::: stringToList(string)
    deps.distinct
  }

  // TODO: Change meta-inf property used here
  /** the list of tags associated with this archive */
  def tags: List[String] = {
    val mfTags = archive.properties.get("tags").map(stringToList(_, ",")).getOrElse(Nil)
    (List("group/"+group) ::: mfTags).map(_.toLowerCase)
  }
}

/** Error that is thrown when an archive on disk is not an actual archive */
case class NotLoadableArchiveEntry(root: File) extends api.Error("not a loadable archive at: "+root.toString)

/** represents a group archive inside an [[LMHHub]] that is installed on disk */
trait LMHHubGroupEntry extends LMHHubDirectoryEntry {
  private var groupManifest: mutable.Map[String, String] = null

  override def load(): Unit = {
    val manifest = {
      List("GROUP_MANIFEST.MF", "GROUP_MANIFEST.mf").map(root./)
        .find(_.exists).getOrElse(throw NotLoadableGroupEntry(root))
    }
    try {
      groupManifest = File.readProperties(manifest)
    } catch {
      case e: Exception => throw NotLoadableGroupEntry(root).setCausedBy(e)
    }
  }

  /** the group properties */
  def properties : mutable.Map[String, String] = {
    load()
    groupManifest
  }

  // TODO: Do we want to read the entry from the folder
  // override lazy val id: String = properties.getOrElse("id", (root / "..").name) + "/" + "meta-inf"
}

/** Error that is thrown when an archive on disk is not an actual archive */
case class NotLoadableGroupEntry(root: File) extends api.Error("not a loadable group at: "+root.toString)


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
    (instanceParts.take(patternParts.length) zip patternParts) foreach {case (i,p) =>
      if (!matches(p, i)) {
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
