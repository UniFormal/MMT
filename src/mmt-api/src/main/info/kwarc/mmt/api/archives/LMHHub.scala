package info.kwarc.mmt.api.archives

import info.kwarc.mmt.api
import info.kwarc.mmt.api.frontend.{Controller, Logger, Report}
import info.kwarc.mmt.api.utils.{File, stringToList}
import scala.collection.mutable
import scala.util.matching.Regex
import info.kwarc.mmt.api.utils.File.scala2Java

/** represents a hub of archives */
abstract class LMHHub extends Logger {
  def controller: Controller

  val logPrefix = "lmh"
  protected def report = controller.report

  /** find all locally installed entries */
  def installedEntries : List[LMHHubEntry]

  /** find all repositories given a specification */
  def entries(spec: String*): List[LMHHubEntry] = if (spec.nonEmpty) installedEntries.filter(e => spec.exists(e.matches)) else installedEntries
  /** finds all archive entries available locally */
  def archiveEntries: List[LMHHubArchiveEntry] = installedEntries.collect({case a: LMHHubArchiveEntry => a})
  /** finds all group entries available locally */
  def groupEntries: List[LMHHubGroupEntry] = installedEntries.collect({case g: LMHHubGroupEntry => g})
  /** finds all directory entries available locally */
  def dirEntries: List[LMHHubDirectoryEntry] = installedEntries.collect({case d: LMHHubDirectoryEntry => d})

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
  /** gets a single locally installed archive by an Archive instance */
  def getEntry(archive: Archive) : Option[LMHHubArchiveEntry]

  /** gets a single locally installed archive by a root folder */
  def getEntry(root: File) : Option[LMHHubEntry]

  def hasEntry(id: String):Boolean = getEntry(id).nonEmpty
  def hasEntry(archive: Archive): Boolean = getEntry(archive).nonEmpty
  def hasEntry(root: File): Boolean = getEntry(root).nonEmpty

  /** creates a new archive */
  def createEntry(id: String): Option[LMHHubEntry]

  /**
    * Install a new archive from the remote
    * @param id ID of archive to install
    * @param version Optional version to be installed
    * @param recursive If set to false, do not install archive dependencies
    * @param update If set to false, do not update already installed archives
    * @return the newly installed entry
    */
  def installEntry(id: String, version: Option[String], recursive: Boolean = false, update: Boolean = true) : Option[LMHHubEntry]

  /**
    * Same as installEntry, but optimised for multiple entries at once
    * @param entries
    * @param recursive
    */
  def installEntries(entries: List[(String, Option[String])], recursive: Boolean = false, update: Boolean = true): Unit

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

  /** the local root of this archive entry */
  val root: File

  /** loads the LMHHubEntry or throw an error if it is invalid */
  def load(): Unit

  /** the properties of this entry, if any */
  def properties: Map[String, String]

  /** reads the long description */
  def readLongDescription: Option[String] = {
    val filename = properties.getOrElse("description", "desc.html")
    List(root, root / "META-INF").map(_ / filename).find(_.exists).map(File.read)
  }


  /** the id of this archive entry */
  val id: String
  /** the name of the group of this entry */
  lazy val group: String = id.split("/").dropRight(1).mkString("/")
  /** the name of this archive */
  lazy val name: String = id.split("/").lastOption.getOrElse("")

  /** check if this archive matches a given spec */
  def matches(spec : String): Boolean = LMHHub.matchesComponents(spec, id)


  // version control things

  /** download information about archive versions from the remote */
  def fetch: Boolean
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

  def properties: Map[String, String] = Map()
  def statistics: Option[SimpleStatistics] = None

  // Read the properties from the manifest
  lazy val id: String = {
    properties.getOrElse("id", {
      val canon = s"${(root / "..").canonical.name}/${root.canonical.name}"
      log(s"Unable to read id from manifest, falling back to $canon")
      canon
    })
  }
}

/** represents a single archive inside an [[LMHHub]] that is installed on disk */
trait LMHHubArchiveEntry extends LMHHubDirectoryEntry {
  /** loads this archive into the controller (if not done already) */
  override def load(): Unit = {
    controller.backend.getArchive(root).getOrElse {
      controller.addArchive(root)
    }

    if(controller.backend.getArchive(root).isEmpty) {
      throw NotLoadableArchiveEntry(root)
    }
  }

  /** returns the [[Archive]] instance belonging to this local ArchiveHub entry */
  lazy val archive : Archive = {
    load()
    controller.backend.getArchive(root).get
  }

  /** reads the archive props */
  override def properties: Map[String, String] = archive.properties.toMap

  private def statsEnabled(): Boolean = properties.get("statistics").exists(v => v != "false" && v != "")

  /** reads archive statistics */
  override def statistics: Option[SimpleStatistics] = if(statsEnabled()) Some(archive.stats(controller)) else None

  /** the list of dependencies of this archive */
  def dependencies: List[String] = {
    // check if we have a meta-inf repository, and if yes install it
    val deps = (if(hub.hasGroup(group)) List(group + "/meta-inf") else Nil) ::: archive.dependencies
    deps.distinct
  }

  // TODO: Change meta-inf property used here
  /** the list of tags associated with this archive */
  def tags: List[String] = {
    val mfTags = properties.get("tags").map(stringToList(_, ",")).getOrElse(Nil)
    (List("group/"+group) ::: mfTags).map(_.toLowerCase)
  }
}

/** Error that is thrown when an archive on disk is not an actual archive */
case class NotLoadableArchiveEntry(root: File) extends api.Error("not a loadable archive at: "+root.toString)

/** represents a group archive inside an [[LMHHub]] that is installed on disk */
trait LMHHubGroupEntry extends LMHHubDirectoryEntry {
  private var groupManifest: Map[String, String] = null

  override def load(): Unit = {
    val manifest = {
      List("GROUP_MANIFEST.MF", "GROUP_MANIFEST.mf").map(root./)
        .find(_.exists).getOrElse(throw NotLoadableGroupEntry(root))
    }
    try {
      groupManifest = File.readProperties(manifest).toMap
    } catch {
      case e: Exception => throw NotLoadableGroupEntry(root).setCausedBy(e)
    }
  }

  /** finds all LMH Hub entries that are a member of this group */
  def members: List[LMHHubEntry] = {
    hub.installedEntries.filter(c => c.group == group && c.id != id)
  }

  /** collects statistics in this archive */
  override def statistics: Option[SimpleStatistics] = {
    Some(
      members
        .collect({case de: LMHHubDirectoryEntry => de.statistics})
        .collect({ case Some(s: SimpleStatistics) => s})
        .fold(SimpleStatistics.empty)(_ + _)
    )
  }

  /** the group properties */
  override def properties : Map[String, String] = {
    load()
    groupManifest
  }
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
