package info.kwarc.mmt.mathhub.library

/**
  * this file contains data structures used for the MathHub API
  * and should be changed only together with the corresponding client structures.
  *
  * the file structure is oriented to the client structures where possible
  */


import info.kwarc.mmt.api.utils._

/** anything returned by the API */
trait IResponse {
  /** serializes this response into a JSONObject Buffer */
  protected def toJSONBuffer: JSONObjectBuffer

  /** turns this response into a JSON object */
  def toJSON: JSON = toJSONBuffer.result()
}

/** any object that is referencable */
trait IReferencable extends IAPIObjectItem {
  val ref: Boolean = false
}

/** any concrete reference */
trait IReference extends IAPIObjectItem {
  val ref: Boolean = true
  val statistics: Option[List[IStatistic]] = None
}


//
// GROUP
//

trait IGroupItem extends IAPIObjectItem {
  val kind: String = "group"
  val parent: Option[IReference] = None

  /** a machine-readable ID of the group */
  val id: String

  /** the name of this group, same as id */
  val name: String

  /** human-readable title of the group */
  val title: IAPIObjectItem.HTML

  /** a short teaser description of the group */
  val teaser: IAPIObjectItem.HTML

  def toJSONBuffer: JSONObjectBuffer = {
    val buffer = new JSONObjectBuffer

    buffer.add("title", JSONString(title))
    buffer.add("teaser", JSONString(teaser))

    buffer
  }
}

/** a reference to a MathHub Archive */
case class IGroupRef(
                      override val id: String,
                      override val name: String,
                      override val title: IAPIObjectItem.HTML,
                      override val teaser: IAPIObjectItem.HTML
                    ) extends IGroupItem with IReference

/** a full description of a MathHub Group */
case class IGroup(
                   override val id: String,
                   override val name: String,
                   override val statistics: Option[List[IStatistic]],
                   override val title: IAPIObjectItem.HTML,
                   override val teaser: IAPIObjectItem.HTML,

                   description: IAPIObjectItem.HTML,
                   responsible: List[String],
                   archives: List[IArchiveRef]
                 ) extends IGroupItem with IReferencable {
  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("description", JSONString(description))
    buffer.add("responsible", JSONArray(responsible.map(JSONString): _*))
    buffer.add("archives", JSONArray(archives.map(_.toJSON):_*))

    buffer
  }
}


//
// TagGroup
//

trait ITagItem extends IAPIObjectItem {
  val kind: String = "tag"
  val parent: Option[IReference] = None

  /** a machine-readable ID of this tag, starts with an @ */
  val id: String

  /** the name of this tag */
  val name: String

  def toJSONBuffer: JSONObjectBuffer = JSONObjectBuffer()
}

/** a reference to a MathHub Archive */
case class ITagRef(
                      override val id: String,
                      override val name: String
                    ) extends ITagItem with IReference

/** a full description of a MathHub Group */
case class ITag(
                   override val id: String,
                   override val name: String,
                   override val statistics: Option[List[IStatistic]],
                   archives: List[IArchiveRef]
                 ) extends ITagItem with IReferencable {
  override def toJSONBuffer: JSONObjectBuffer = JSONObjectBuffer(
    "archives" -> JSONArray(archives.map(_.toJSON):_*)
  )
}

//
// Archive
//

trait IArchiveItem extends IAPIObjectItem {
  val kind: String = "archive"
  val parent: Some[IGroupRef]

  /** the id of the archive $parent.id/$name */
  val id: String

  /** the name of this archive */
  val name: String

  /** human-readable title of the group */
  val title: IAPIObjectItem.HTML

  /** a short teaser description of the group */
  val teaser: IAPIObjectItem.HTML

  def toJSONBuffer: JSONObjectBuffer = {
    val buffer = new JSONObjectBuffer

    buffer.add("title", JSONString(title))
    buffer.add("teaser", JSONString(teaser))

    buffer
  }
}

/** a reference to a MathHub Archive */
case class IArchiveRef(
                        override val parent: Some[IGroupRef],
                        override val id: String,
                        override val name: String,
                        override val title: IAPIObjectItem.HTML,
                        override val teaser: IAPIObjectItem.HTML
                      ) extends IArchiveItem with IReference with IDocumentParentRef

/** a full description of a MathHub Archive */
case class IArchive(
                        override val parent: Some[IGroupRef],
                        override val id: String,
                        override val name: String,
                        override val statistics: Option[List[IStatistic]],
                        override val title: IAPIObjectItem.HTML,
                        override val teaser: IAPIObjectItem.HTML,

                        tags: List[ITagRef],

                        description: IAPIObjectItem.HTML,
                        responsible: List[String],
                        narrativeRoot: IDocument
                      ) extends IArchiveItem with IReferencable {
  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("tags", JSONArray(tags.map(_.toJSON):_*))
    buffer.add("description", JSONString(description))
    buffer.add("responsible", JSONArray(responsible.map(JSONString):_*))
    buffer.add("narrativeRoot", narrativeRoot.toJSON)

    buffer
  }
}

//
// NARRATION
//

/** a narrative element inside an archive */
trait INarrativeElement extends IAPIObjectItem // TODO: URIS?

/** anything that can be the parent of a document */
trait IDocumentParentRef extends IReference


trait IDocumentItem extends IAPIObjectItem {
  val kind = "document"
  val parent: Some[IDocumentParentRef]

  /** the uri of this document */
  val id: String

  /** the name of this document */
  val name: String

  def toJSONBuffer: JSONObjectBuffer = {
    new JSONObjectBuffer
  }
}

/** a reference to a document */
case class IDocumentRef(
                         override val parent: Some[IDocumentParentRef],
                         override val id: IAPIObjectItem.URI,
                         override val name: String
                       ) extends IDocumentItem with IReference with IDocumentParentRef with INarrativeElement

/** a document of content */
case class IDocument(
                      override val parent: Some[IDocumentParentRef],
                      override val id: IAPIObjectItem.URI,
                      override val name: String,

                      tags: List[String],
                      sourceRef: Option[IFileReference],

                      override val statistics: Option[List[IStatistic]],

                      decls: List[INarrativeElement]
                    ) extends IDocumentItem with IReferencable with INarrativeElement {
  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("decls", JSONArray(decls.map(_.toJSON):_*))
    buffer.add("tags", JSONArray(tags.map(JSONString) :_*))
    buffer.addO("sourceRef", sourceRef.map(_.toJSON))

    buffer
  }

}

object IDocument {
  /** the list of known tags */
  val knownTags = List("ipynb-omdoc")
}

trait IOpaqueElementItem extends IAPIObjectItem {
  val kind: String = "opaque"
  val parent: Some[IDocumentParentRef]

  /** name of the module */
  val name: String

  /** the uri of this IModuleItem */
  val id: String

  def toJSONBuffer: JSONObjectBuffer = {
    new JSONObjectBuffer
  }
}

/** a reference to an opaque element */
case class IOpaqueElementRef(
                              override val parent: Some[IDocumentRef],
                              override val id: String,
                              override val name: String
                            ) extends IOpaqueElementItem with IReference

/** an opaque element */
case class IOpaqueElement(
                           override val parent: Some[IDocumentRef],
                           override val id: String,
                           override val name: String,

                           override val statistics: Option[List[IStatistic]],

                           contentFormat: String,
                           content: String
                         ) extends IOpaqueElementItem with IReferencable with INarrativeElement {

  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("contentFormat", JSONString(contentFormat))
    buffer.add("content", JSONString(content))

    buffer
  }

}

//
// CONTENT
//

trait IModuleItem extends IAPIObjectItem {
  /** there is no parent */
  val parent: Option[IReference] = None

  /** name of the module */
  val name: String

  /** the uri of this IModuleItem */
  val id: String

  def toJSONBuffer: JSONObjectBuffer = {
    val buffer = new JSONObjectBuffer

    buffer.add("name", JSONString(name))

    buffer
  }
}

/** a reference to a module */
trait IModuleRef extends IModuleItem with IReference with INarrativeElement

/** a reference to a theory */
case class ITheoryRef(
                       override val id: IAPIObjectItem.URI,
                       override val name: String
                     ) extends IModuleRef {
  val kind: String = "theory"
}


/** a reference to a view */
case class IViewRef(
                     override val id: IAPIObjectItem.URI,
                     override val name: String,
                     ) extends IModuleRef {
  val kind: String = "view"
}

/** an actual module, i.e. a theory or a view */
trait IModule extends IModuleItem with IReferencable {

  /** presentation of this module as HTML */
  val presentation: IAPIObjectItem.HTML

  /** source code of this module, if available */
  val source: Option[String]

  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("presentation", JSONString(presentation))
    buffer.add("source", source.map(JSONString).getOrElse(JSONNull))

    buffer
  }
}

/** a description of a theory */
case class ITheory(
                    override val id: IAPIObjectItem.URI,
                    override val name: String,

                    override val statistics: Option[List[IStatistic]],

                    override val presentation: IAPIObjectItem.HTML,
                    override val source: Option[String],

                    meta: Option[ITheoryRef]
                     ) extends IModule {
  val kind: String = "theory"

  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("meta", meta.map(_.toJSON).getOrElse(JSONNull))

    buffer
  }
}

/** a description of a view */
case class IView(
                  override val id: IAPIObjectItem.URI,
                  override val name: String,

                  override val statistics: Option[List[IStatistic]],

                  override val presentation: IAPIObjectItem.HTML,
                  override val source: Option[String],

                  domain: ITheoryRef,
                  codomain: ITheoryRef
                  ) extends IModule {
  val kind: String = "theory"

  override def toJSONBuffer: JSONObjectBuffer = super.toJSONBuffer(
    "domain" -> domain.toJSON,
    "codomain" -> codomain.toJSON
  )
}

//
// Other responses
//

case class IStatistic(key: String, value: Int) extends IResponse {
  override def toJSONBuffer: JSONObjectBuffer = JSONObjectBuffer(
    "key" -> JSONString(key),
    "value" -> JSONInt(value),
  )
}

/** a version information about MMT */
case class IMMTVersionInfo(
                            versionNumber: String,
                            buildDate: Option[String]
                          ) extends IResponse {
  override def toJSONBuffer: JSONObjectBuffer = JSONObjectBuffer(
    "versionNumber" -> JSONString(versionNumber),
    "buildDate" -> buildDate.map(JSONString).getOrElse(JSONNull)
  )
}


//
// Helper object
//

case class IFileReference(archive: IArchiveRef, path: String) extends IResponse {
  def toJSONBuffer: JSONObjectBuffer = {
    val buffer = new JSONObjectBuffer

    buffer.add("kind", JSONString("file"))
    buffer.add("ref", JSONBoolean(true))
    buffer.add("archive", archive.toJSON)
    buffer.add("path", JSONString(path))

    buffer
  }
}

/**
  * Any object exposed by the API
  */
trait IAPIObjectItem extends IResponse {

  /** the kind of object this represents */
  val kind: String

  /** weather this object is a reference or a full description */
  val ref: Boolean

  /** the id of this object */
  val id: String

  /** the name of this object */
  val name: String

  /** the parent of this object, if any */
  val parent: Option[IReference]

  /** statistics of this element (if any) */
  val statistics: Option[List[IStatistic]]

  /** serializes this object into a JSON Object */
  override final def toJSON: JSON = {
    val buffer = toJSONBuffer

    // add shared attributes
    buffer.add("kind", JSONString(kind))
    buffer.add("ref", JSONBoolean(ref))
    buffer.add("id", JSONString(id))
    buffer.add("name", JSONString(name))
    buffer.add("parent", parent.map(_.toJSON).getOrElse(JSONNull))
    buffer.addO("statistics", statistics.map(s => JSONArray(s.map(_.toJSON) :_*)))

    buffer.result()
  }
}

object IAPIObjectItem {
  type HTML = String
  type URI = String
}