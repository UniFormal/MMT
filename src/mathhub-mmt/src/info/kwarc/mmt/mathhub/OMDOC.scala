package info.kwarc.mmt.mathhub

/**
  * this file contains data structures used for the MathHub API
  * and should be changed only together with the corresponding client structures.
  */


import info.kwarc.mmt.api.utils.{JSON, JSONConverter, JSONObjectBuffer}

/**
  * Anything that can be serialized as OMDOC via the MMT-MathHub API
  */
abstract class OMDOC {
  def toJSON: JSON
}

object OMDOC {
  /** so that we can convert all the things into JSON */
  implicit def converter[T <: OMDOC]: JSONConverter[T] = new JSONConverter[T] {
    def toJSON(obj: T): JSON = obj.toJSON
    def fromJSONOption(j: JSON): Option[T] = None
  }
}


//
// GROUPS
//

case class IGroupItem(id: String, title: String, teaser: String) extends OMDOC{
  def toJSON: JSON = {
    val buffer = new JSONObjectBuffer

    buffer.add("id", id)
    buffer.add("title", title)
    buffer.add("teaser", teaser)

    buffer.result()
  }
}

case class IGroup(
                   id: String, title: String, teaser: String,
                   description: String, responsible: List[String], archives: List[IArchiveItem]
                 ) extends OMDOC {
  def toJSON: JSON = {
    val buffer = new JSONObjectBuffer

    buffer.add("id", id)
    buffer.add("title", title)
    buffer.add("teaser", teaser)

    buffer.add("description", description)
    buffer.add("responsible", responsible)
    buffer.add("archives", archives)

    buffer.result()
  }
}

//
// ARCHIVES
//

case class IArchiveItem(id: String, group: String, title: String, teaser: String) extends OMDOC {
  def toJSON: JSON = {
    val buffer = new JSONObjectBuffer

    buffer.add("id", id)
    buffer.add("group", group)
    buffer.add("title", title)
    buffer.add("teaser", teaser)

    buffer.result()
  }
}


case class IArchive(
                     id: String, group: String, title: String, teaser: String,
                     description: String, responsible: List[String], modules: List[IModuleItem]
                   ) extends OMDOC {
  def toJSON: JSON = {
    val buffer = new JSONObjectBuffer

    buffer.add("id", id)
    buffer.add("group", group)
    buffer.add("title", title)
    buffer.add("teaser", teaser)

    buffer.add("description", description)
    buffer.add("responsible", responsible)
    buffer.add("modules", modules)

    buffer.result()
  }
}

//
// MODULES
//


case class IModuleItem(name: String, archive: String) extends OMDOC {
  def toJSON: JSON = {
    val buffer = new JSONObjectBuffer

    buffer.add("name", name)
    buffer.add("archive", archive)

    buffer.result()
  }
}

case class IModule(
                    name: String, archive: String,
                    content: String
                  ) extends OMDOC {
  def toJSON: JSON = {
    val buffer = new JSONObjectBuffer

    buffer.add("name", name)
    buffer.add("archive", archive)

    buffer.add("content", content)

    buffer.result()
  }
}