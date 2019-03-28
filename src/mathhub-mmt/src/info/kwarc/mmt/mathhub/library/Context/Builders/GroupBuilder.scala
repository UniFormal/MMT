package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.archives.{LMHHubArchiveEntry, LMHHubGroupEntry}
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IGroup, IGroupRef}

/** a builder for groups */
trait GroupBuilder { this: Builder =>

  /** tries to find a group with the given id */
  protected def tryGroup(id: String) : Option[LMHHubGroupEntry] = {
    logDebug(s"trying $id as group")

    val optEntry = mathHub.entries_.collectFirst({
      case e: LMHHubGroupEntry if e.group == id => e
    })

    if(optEntry.isEmpty){
      logDebug(s"$id is not a group")
    }

    optEntry
  }

  /** gets a reference to a group */
  def getGroupRef(id: String): Option[IGroupRef] = getReferenceOf(classOf[IGroupRef], id)

  /** builds a reference to a group */
  protected def buildGroupReference(entry: LMHHubGroupEntry) : Option[IGroupRef] = Some(
    IGroupRef(
      entry.group, /* id */
      entry.group, /* name */
      entry.properties.getOrElse("title", return buildFailure(entry.id, "group.title")), /* title */
      entry.properties.getOrElse("teaser", return buildFailure(entry.id, "group.teaser")) /* teaser */
    )
  )

  /** gets a group */
  def getGroup(id: String): Option[IGroup] = getObjectOf(classOf[IGroup], id)

  /** builds a group representation */
  protected def buildGroup(entry: LMHHubGroupEntry): Option[IGroup] = {
    val ref = getGroupRef(entry.group).getOrElse(return buildFailure(entry.group, "getGroupRef(group.id)"))

    // get the description file
    val description = entry.readLongDescription.getOrElse("No description provided")
    val responsible = entry.properties.getOrElse("responsible", "").split(",").map(_.trim).toList

    val archives = mathHub.entries_.collect({case archive: LMHHubArchiveEntry if archive.group == ref.id => archive.id })

    Some(IGroup(
      ref.id, ref.name,
      getStats(ref.id),
      ref.title, ref.teaser,

      description,
      responsible,
      archives.flatMap(getArchiveRef)
    ))
  }


}
