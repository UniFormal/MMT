package info.kwarc.mmt.mathhub

import info.kwarc.mmt.api
import info.kwarc.mmt.api.archives.lmh.{LMHHub, LMHHubArchiveEntry, LMHHubGroupEntry}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.{GeneralError, MPath, Path}

trait MathHubAPIImpl {
  protected var controller: Controller

  private def hubOrError: LMHHub = controller.getMathHub.getOrElse(throw api.GeneralError("LMH Missing from Controller"))

  private def groupInformation(p: LMHHubGroupEntry => Boolean): List[GroupInformation] =
    hubOrError.entries().collect({case g: LMHHubGroupEntry if p(g) => g})
      .map(g => GroupInformation(g, controller))

  private def archiveInformation(p: LMHHubArchiveEntry => Boolean): List[ArchiveInformation] =
    hubOrError.entries().collect({case a: LMHHubArchiveEntry if p(a) => a})
      .map(a => ArchiveInformation(a, controller))

  //
  // API Methods
  //

  protected def getGroups : List[IGroupItem] = groupInformation(_ => true).map(_.toIGroupItem)

  protected def getGroup(group: String) : IGroup  = {
    groupInformation(g => g.group == group)
      .headOption.getOrElse(throw GeneralError(s"Group $group does not exist. "))
      .toIGroup
  }

  protected def getArchive(group: String, name: String) : IArchive = {
    archiveInformation(a => a.group == group && a.name == name)
      .headOption.getOrElse(throw GeneralError(s"Archive $group/$name does not exist. "))
      .toIArchive
  }

  protected def getModule(group: String, archive: String, name: String): IModule = {
    // FIXME implement this
    throw GeneralError("Module does not exist")
  }

  protected def getDocument(group: String, archive: String, name: String): IDocument = {
    // FIXME implement this
    throw GeneralError("Document does not exist")
  }
}

case class GroupInformation(group: LMHHubGroupEntry, controller: Controller) {
  /** the title of this group */
  def title: String = group.properties.getOrElse("title", group.id)

  /** the teaser of this group */
  def teaser : String = group.properties.getOrElse("teaser", "")

  /** gets the description of the group */
  def description : String = {
    val file = group.root / group.properties.getOrElse("description", "desc.html")
    if(group.root <= file && file.exists()) {
      File.read(file)
    } else {
      "No Description available. "
    }
  }

  /** gets the email addresses of people responsible for this group */
  def responsible : List[String] = group.properties.getOrElse("responsible", "").split(",").map(_.trim).toList

  /** gets all archives contained in this Group */
  def archives: List[IArchiveItem] = group.hub.entries()
    .collect({case e: LMHHubArchiveEntry if e.group == group.group => e})
    .map(a => ArchiveInformation(a, controller)).map(_.toIArchiveItem)


  def toIGroupItem: IGroupItem = IGroupItem(
    group.group, title, teaser
  )
  def toIGroup: IGroup = {
    IGroup(
      group.group, title, teaser,
      description, responsible, archives
    )
  }
}

case class ArchiveInformation(archive: LMHHubArchiveEntry, controller: Controller) {
  /** the title of this group */
  def title: String = archive.archive.properties.getOrElse("title", archive.id)

  /** the teaser of this group */
  def teaser : String = archive.archive.properties.getOrElse("teaser", "")

  /** gets the description of the group */
  def description : String = {
    val file = archive.root / archive.archive.properties.getOrElse("description", "desc.html")
    if(archive.root <= file && file.exists()) {
      File.read(file)
    } else {
      "No Description available. "
    }
  }

  /** gets the email addresses of people responsible for this group */
  def responsible : List[String] = archive.archive.properties.getOrElse("responsible", "").split(",").map(_.trim).toList


  def toIArchiveItem: IArchiveItem = IArchiveItem(
    archive.name, archive.group, title, teaser
  )
  def toIArchive: IArchive = {
    IArchive(
      archive.name, archive.group, title, teaser,
      description, responsible, List() // TODO: Implement documents
    )
  }
}

case class ModuleInformation(module: MPath, archive: ArchiveInformation) {

  def toIModuleItem: IModuleItem = IModuleItem(module.toString, archive.archive.id)
  def toIModule: IModule = IModule(module.toString, archive.archive.id, "", "")
}