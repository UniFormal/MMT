package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.archives.LMHHubArchiveEntry
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IArchive, IArchiveRef}

trait ArchiveBuilder { this: Builder =>

  /** tries to find an archive with a given id */
  protected def tryArchive(id: String) : Option[LMHHubArchiveEntry] = {
    logDebug(s"trying $id as archive")
    val optEntry = mathHub.entries_.collectFirst({
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

  /** builds an archive object */
  protected def buildArchive(entry: LMHHubArchiveEntry): Option[IArchive] = {
    val ref = getArchiveRef(entry.id).getOrElse(return buildFailure(entry.id, "getArchiveRef(archive.id)"))

    val version = entry.version

    val tags = entry.tags.map(t => getTagRef("@" + t).getOrElse(return buildFailure(entry.id, s"getTagRef(archive.tag[@$t])")))

    // get the description file
    val file = entry.root / entry.archive.properties.getOrElse("description", "desc.html")
    val description = if(entry.root <= file && file.exists()) {
      File.read(file)
    } else { "No description provided" }

    val responsible = entry.archive.properties.getOrElse("responsible", "").split(",").map(_.trim).toList

    val narrativeRoot = getDocument(entry.archive.narrationBase.toString)
      .getOrElse(return buildFailure(entry.id, "getDocument(archive.narrativeRoot)"))

    Some(IArchive(
      ref.parent, ref.id, ref.name,
      getStats(ref.id),
      ref.title, ref.teaser,
      tags,
      version,

      description,
      responsible,

      narrativeRoot
    ))
  }
}
