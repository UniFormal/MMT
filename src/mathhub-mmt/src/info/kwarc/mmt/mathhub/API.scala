package info.kwarc.mmt.mathhub

/**
  * this file contains data structures used for the MathHub API
  * and should be changed only together with the corresponding client structures.
  */


import info.kwarc.mmt.api.utils.{JSON, JSONConverter, JSONObjectBuffer}

/**
  * Anything that can be serialized as OMDOC via the MMT-MathHub API
  */
abstract class API {
  def toJSON: JSON
}

object API {
  /** so that we can convert all the things into JSON */
  implicit def converter[T <: API]: JSONConverter[T] = new JSONConverter[T] {
    def toJSON(obj: T): JSON = obj.toJSON
    def fromJSONOption(j: JSON): Option[T] = None
  }
}


//
// GROUPS
//

case class IGroupItem(id: String, title: String, teaser: String) extends API{
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
                 ) extends API {
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

case class IArchiveItem(id: String, group: String, title: String, teaser: String) extends API {
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
                   ) extends API {
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


case class IModuleItem(name: String, archive: String) extends API {
  def toJSON: JSON = {
    val buffer = new JSONObjectBuffer

    buffer.add("name", name)
    buffer.add("archive", archive)

    buffer.result()
  }
}

case class IModule(
                    name: String, archive: String,
                    variants: List[IVariantItem]
                  ) extends API {
  def toJSON: JSON = {
    val buffer = new JSONObjectBuffer

    buffer.add("name", name)
    buffer.add("archive", archive)

    buffer.add("variants", variants)

    buffer.result()
  }
}

case class IVariantItem(name: String, module: String) extends API {
  def toJSON: JSON = {
    val buffer = new JSONObjectBuffer

    buffer.add("name", name)
    buffer.add("module", module)

    buffer.result()
  }
}

case class IVariant(
                     name: String, module: String,
                     presentation: String, source: String
                   ) extends API {
  def toJSON: JSON = {
    val buffer = new JSONObjectBuffer

    buffer.add("name", name)
    buffer.add("module", module)

    buffer.add("presentation", presentation)
    buffer.add("source", source)

    buffer.result()
  }
}