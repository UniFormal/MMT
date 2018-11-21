package info.kwarc.mmt.mathhub.library

/**
  * this file contains data structures used for the MathHub API
  * and should be changed only together with the corresponding client structures.
  *
  * the file structure is oriented to the client structures where possible
  */


import info.kwarc.mmt.api.utils._

/** anything returned by the API */
sealed trait IResponse {
  /** serializes this response into a JSONObject Buffer */
  protected def toJSONBuffer: JSONObjectBuffer

  /** turns this response into a JSON object */
  def toJSON: JSON = toJSONBuffer.result()
}

/** any object that is referencable */
sealed trait IReferencable extends IAPIObjectItem {
  val ref: Boolean = false
}

/** any concrete reference */
sealed trait IReference extends IAPIObjectItem {
  val ref: Boolean = true
  val statistics: Option[List[IStatistic]] = None
}


//
// GROUP
//
/** an archive or a group reference */
sealed trait IHubReference extends IReference


sealed trait IGroupItem extends IAPIObjectItem {
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
                    ) extends IGroupItem with IReference with IHubReference

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

sealed trait ITagItem extends IAPIObjectItem {
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

sealed trait IArchiveItem extends IAPIObjectItem {
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
                      ) extends IArchiveItem with IReference with IDocumentParentRef with IHubReference

/** a full description of a MathHub Archive */
case class IArchive(
                        override val parent: Some[IGroupRef],
                        override val id: String,
                        override val name: String,
                        override val statistics: Option[List[IStatistic]],
                        override val title: IAPIObjectItem.HTML,
                        override val teaser: IAPIObjectItem.HTML,

                        tags: List[ITagRef],
                        version: Option[String],

                        description: IAPIObjectItem.HTML,
                        responsible: List[String],
                        narrativeRoot: IDocument
                      ) extends IArchiveItem with IReferencable {
  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("tags", JSONArray(tags.map(_.toJSON):_*))
    buffer.addO("version", version.map(JSONString))
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
sealed trait INarrativeElement extends IAPIObjectItem // TODO: URIS?

/** anything that can be the parent of a document */
sealed trait IDocumentParentRef extends IReference


sealed trait IDocumentItem extends IAPIObjectItem {
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
                      sourceRef: Option[ISourceReference],

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

sealed trait IOpaqueElementItem extends IAPIObjectItem {
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
// MODULES
//

sealed trait IModuleItem extends IAPIObjectItem {
  val kind: String = "module"

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
case class IModuleRef(
                       override val id: String,
                       override val name: String,

                       mod: String, // "module" | "view"
                     ) extends IModuleItem with IReference with INarrativeElement {
  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("mod", JSONString(mod))

    buffer
  }
}

/** an actual module, i.e. a theory or a view */
sealed trait IModule extends IModuleItem with IReferencable {
  val declarations: List[IDeclarationRef]
  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("declarations", JSONArray(declarations.map(_.toJSON) : _*))

    buffer
  }
}

/** a description of a theory */
case class ITheory(
                    override val id: IAPIObjectItem.URI,
                    override val name: String,

                    override val statistics: Option[List[IStatistic]],
                    override val declarations: List[IDeclarationRef],

                    meta: Option[IModuleRef]
                     ) extends IModule {
  override def toJSONBuffer: JSONObjectBuffer = super.toJSONBuffer(
    "mod" -> JSONObject(
      "kind" -> JSONString("theory"),
      "meta" -> meta.map(_.toJSON).getOrElse(JSONNull)
    )
  )
}

/** a description of a view */
case class IView(
                  override val id: IAPIObjectItem.URI,
                  override val name: String,

                  override val statistics: Option[List[IStatistic]],
                  override val declarations: List[IDeclarationRef], // TODO: Lower type

                  domain: IModuleRef,
                  codomain: IModuleRef
                  ) extends IModule {
  override def toJSONBuffer: JSONObjectBuffer = super.toJSONBuffer(
    "mod" -> JSONObject(
      "kind" -> JSONString("view"),
      "domain" -> domain.toJSON,
      "codomain" -> codomain.toJSON
    )
  )
}

//
// Declarations
//

sealed trait IDeclarationItem extends IAPIObjectItem {
  val kind: String = "declaration"

  /** there is no parent */
  val parent: Option[IModuleRef]

  /** name of the declaration */
  val name: String

  /** the uri of this IDeclarationItem */
  val id: String

  def toJSONBuffer: JSONObjectBuffer = JSONObjectBuffer(
    "name" -> JSONString(name)
  )
}

/** a reference to a module */
case class IDeclarationRef(
                       override val id: String,
                       override val name: String,
                       override val parent: Some[IModuleRef],

                       declaration: String, // "structure" | "rule" | "constant" | "nested"
                     ) extends IDeclarationItem with IReference with INarrativeElement {
  override def toJSONBuffer: JSONObjectBuffer = super.toJSONBuffer(
    "declaration" -> JSONString(declaration)
  )
}

/** an actual declaration, i.e. a structure, rule, ruleconstant or nested */
sealed trait IDeclaration extends IDeclarationItem with IReferencable {
  val declarations: List[IDeclarationRef]
  val components: List[IReference] // TODO: List of components
  override def toJSONBuffer: JSONObjectBuffer = super.toJSONBuffer(
    "declarations" -> JSONArray(declarations.map(_.toJSON) : _*),
    "components" -> JSONArray(declarations.map(_.toJSON) : _*)
  )
}

/** a description of a structure */
case class IStructure(
                      override val id: IAPIObjectItem.URI,
                      override val name: String,
                      override val parent: Some[IModuleRef],

                      override val statistics: Option[List[IStatistic]],
                      override val declarations: List[IDeclarationRef],
                      override val components: List[IReference], // TODO: List of components

                      isImplicit: Boolean,
                      isInclude: Boolean,
                     ) extends IDeclaration {
  override def toJSONBuffer: JSONObjectBuffer = super.toJSONBuffer(
    "mod" -> JSONObject(
      "kind" -> JSONString("structure"),

      "implicit" -> JSONBoolean(isImplicit),
      "include" -> JSONBoolean(isInclude),
    )
  )
}

/** a description of a constant */
case class IConstant(
                       override val id: IAPIObjectItem.URI,
                       override val name: String,
                       override val parent: Some[IModuleRef],

                       override val statistics: Option[List[IStatistic]],
                       override val declarations: List[IDeclarationRef],
                       override val components: List[IReference], // TODO: List of components

                       role: Option[String],
                       alias: List[String],
                     ) extends IDeclaration {
  override def toJSONBuffer: JSONObjectBuffer = super.toJSONBuffer(
    "mod" -> JSONObject(
      "kind" -> JSONString("constant"),

      "role" -> role.map(JSONString).getOrElse(JSONNull),
      "alias" -> JSONArray(alias.map(JSONString) : _*)
    )
  )
}

/** a description of a rule */
case class IRule(
                      override val id: IAPIObjectItem.URI,
                      override val name: String,
                      override val parent: Some[IModuleRef],

                      override val statistics: Option[List[IStatistic]],
                      override val declarations: List[IDeclarationRef],
                      override val components: List[IReference], // TODO: List of components
                    ) extends IDeclaration {
  override def toJSONBuffer: JSONObjectBuffer = super.toJSONBuffer(
    "mod" -> JSONObject(
      "kind" -> JSONString("rule"),
    )
  )
}

/** a description of a nested module */
case class INestedModule(
                  override val id: IAPIObjectItem.URI,
                  override val name: String,
                  override val parent: Some[IModuleRef],

                  override val statistics: Option[List[IStatistic]],
                  override val declarations: List[IDeclarationRef],
                  override val components: List[IReference], // TODO: List of components

                  mod: IModuleRef,
                ) extends IDeclaration {
  override def toJSONBuffer: JSONObjectBuffer = super.toJSONBuffer(
    "mod" -> JSONObject(
      "kind" -> JSONString("nested"),
      "mod" -> mod.toJSON
    )
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

case class ISourceReference(parent: IHubReference, version: Option[String], path: Option[String]) extends IResponse {
  def toJSONBuffer: JSONObjectBuffer = {
    val buffer = new JSONObjectBuffer

    buffer.add("kind", JSONString("source"))
    buffer.add("ref", JSONBoolean(true))
    buffer.add("parent", parent.toJSON)
    buffer.addO("version", version.map(JSONString))
    buffer.addO("path", path.map(JSONString))

    buffer
  }
}

/**
  * Any object exposed by the API
  */
sealed trait IAPIObjectItem extends IResponse {

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