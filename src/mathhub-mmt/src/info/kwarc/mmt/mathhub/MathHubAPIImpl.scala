package info.kwarc.mmt.mathhub

import info.kwarc.mmt.api
import info.kwarc.mmt.api.archives.lmh.{LMHHub, LMHHubArchiveEntry, LMHHubGroupEntry}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.utils.File
import info.kwarc.mmt.api.{GeneralError, MPath}

trait MathHubAPIImpl {
  protected var controller: Controller


  //
  // API Methods
  //

  private def hubOrError: LMHHub = controller.getMathHub.getOrElse(throw api.GeneralError("LMH Missing from Controller"))

  protected def getGroups : List[IGroupItem] = {
    hubOrError.entries().collect({case g: LMHHubGroupEntry => g})
      .map(GroupInformation)
      .map(_.toIGroupItem)
  }

  protected def getGroup(group: String) : IGroup  = {
    hubOrError.entries().collect({case g: LMHHubGroupEntry if g.group == group => g})
      .map(GroupInformation)
      .headOption.getOrElse(throw GeneralError(s"Group $group does not exist. "))
      .toIGroup
  }

  protected def getArchive(group: String, name: String) : IArchive = {
    hubOrError.entries().collect({case a: LMHHubArchiveEntry if a.group == group && a.name == name => a})
      .map(ArchiveInformation)
      .headOption.getOrElse(throw GeneralError(s"Archive $group/$name does not exist. "))
      .toIArchive
  }

  protected def getModule(group: String, archive: String, name: String): IModule = {
    // FIXME implement this
    throw GeneralError("Module does not exist")
  }

  protected def getVariant(group: String, archive: String, name: String, variant: String): IVariant = {
    // FIXME implement this
    throw GeneralError("Variant does not exist")
  }
}

case class GroupInformation(group: LMHHubGroupEntry) {
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


  def toIGroupItem: IGroupItem = IGroupItem(
    group.group, title, teaser
  )
  def toIGroup: IGroup = {
    val archives: List[ArchiveInformation] = group.hub.entries()
      .collect({case e: LMHHubArchiveEntry if e.group == group.group => e})
      .map(ArchiveInformation)

    IGroup(
      group.group, title, teaser,
      description, responsible, archives.map(_.toIArchiveItem)
    )
  }
}

case class ArchiveInformation(archive: LMHHubArchiveEntry) {
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
    val modules : List[ModuleInformation] = Nil // TODO: Implement me

    IArchive(
      archive.name, archive.group, title, teaser,
      description, responsible, modules.map(_.toIModuleItem)
    )
  }
}

case class ModuleInformation(module: MPath, archive: LMHHubArchiveEntry) {
  def toIModuleItem: IModuleItem = IModuleItem(module.toString, archive.id)
  def toIModule: IModule = IModule(module.toString, archive.id, List())
}