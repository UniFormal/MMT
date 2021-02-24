package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.{DPath, NamespaceMap, Path}
import info.kwarc.mmt.api.archives.LMHHubArchiveEntry
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IArchive, IArchiveRef, IDocument, IDocumentRef}

import scala.util.Try

trait ArchiveBuilder { this: Builder =>

  /** tries to find an archive with a given id */
  protected def tryArchive(id: String) : Option[LMHHubArchiveEntry] = {
    logDebug(s"trying $id as archive")
    val optEntry = mathHub.installedEntries.collectFirst({
      case e: LMHHubArchiveEntry if e.id == id => e
    })

    if(optEntry.isEmpty){
      logDebug(s"$id is not an archive")
    }

    optEntry
  }

  /** gets a reference to an archive */
  def getArchiveRef(id: String): Option[IArchiveRef] = getReferenceOf(classOf[IArchiveRef], id)

  /** builds a reference to an archive */
  protected def buildArchiveReference(entry: LMHHubArchiveEntry) : Option[IArchiveRef] = Some(
    IArchiveRef(
      getGroupRef(entry.group).map(Some(_)).getOrElse(return buildFailure(entry.id, "archive.parent")), /* parent */
      entry.id, /* id */
      entry.name, /* name */
      entry.archive.properties.getOrElse("title", entry.id), /* title */
      entry.archive.properties.getOrElse("teaser", "(no teaser provided)") /* teaser */
    )
  )

  /** gets an archive */
  def getArchive(id: String): Option[IArchive] = getObjectOf(classOf[IArchive], id)

  /** gets the narrative root of an archive */
  def getArchiveNarrativeRoot(id: String): Option[DPath] = {
    val entry = mathHub.installedEntries.collectFirst({
      case e: LMHHubArchiveEntry if e.id == id => e
    }).getOrElse(return None)
    getArchiveNarrativeRootPath(entry)
  }

  /** gets a dpath pointing to the narrative root of an archive */
  private def getArchiveNarrativeRootPath(entry: LMHHubArchiveEntry): Option[DPath] = {
    Path.fromURI(entry.archive.narrationBase, entry.archive.namespaceMap) match {
      case d: DPath => Some(d)
      case _ => None
    }
  }

  /** builds an archive object */
  protected def buildArchive(entry: LMHHubArchiveEntry): Option[IArchive] = {
    val ref = getArchiveRef(entry.id).getOrElse(return buildFailure(entry.id, "getArchiveRef(archive.id)"))

    val version = entry.version

    val tags = entry.tags.map(t => getTagRef("@" + t).getOrElse(return buildFailure(entry.id, s"getTagRef(archive.tag[@$t])")))

    // get the description file
    val description = entry.readLongDescription.getOrElse("No description provided")

    val responsible = entry.properties.getOrElse("responsible", "").split(",").map(_.trim).toList

    val narrativeRootPath = entry.archive.narrationBase.toString
    val narrativeRoot = getDocument(narrativeRootPath)
      .getOrElse({
        val pseudoPath = getArchiveNarrativeRootPath(entry) match {
          case Some(d) => d
          case None => return buildFailure(entry.id, s"Path.parseD(archive.narrativeRoot")
        }
        logDebug(s"Archive ${ref.id} has empty narrative root, using pseudo-document")
        buildPseudoDocument(pseudoPath, "This archive has no omdoc content")
      })

    // make the archive
    var archive = IArchive(
      ref.parent, ref.id, ref.name,
      getStats(entry.statistics),
      ref.title, ref.teaser,
      tags,
      version,

      description,
      responsible,

      narrativeRoot
    )

    // call all the pseudos on this archive!
    pseudos.foreach(pseudo => {
      archive = pseudo.extendArchive(this, archive)
    })

    // and return it!
    Some(archive)
  }
}
