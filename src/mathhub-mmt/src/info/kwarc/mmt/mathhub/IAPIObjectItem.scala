package info.kwarc.mmt.mathhub

/**
  * this file contains data structures used for the MathHub API
  * and should be changed only together with the corresponding client structures.
  *
  * the file structure is oriented to the client structures where possible
  */


import info.kwarc.mmt.api.utils.{JSON, JSONConverter, JSONObjectBuffer}

/** any object that is referencable */
trait IReferencable extends IAPIObjectItem {
  val ref: Boolean = false
}

/** any concrete reference */
trait IReference extends IAPIObjectItem {
  val ref: Boolean = true
}


//
// GROUP
//

trait IGroupItem extends IAPIObjectItem {
  val kind: String = "group"
  val parent: Option[IReference] = None

  /** a machine-readable ID of the group */
  val id: String

  /** human-readable title of the group */
  val title: IAPIObjectItem.HTML

  /** a short teaser description of the group */
  val teaser: IAPIObjectItem.HTML

  def toJSONBuffer: JSONObjectBuffer = {
    val buffer = new JSONObjectBuffer

    buffer.add("title", title)
    buffer.add("teaser", teaser)

    buffer
  }
}

/** a reference to a MathHub Archive */
case class IGroupRef(
                      override val id: String,
                      override val title: IAPIObjectItem.HTML,
                      override val teaser: IAPIObjectItem.HTML
                    ) extends IGroupItem with IReference

/** a full description of a MathHub Group */
case class IGroup(
                   override val id: String,
                   override val title: IAPIObjectItem.HTML,
                   override val teaser: IAPIObjectItem.HTML,

                   description: IAPIObjectItem.HTML,
                   responsible: List[String],
                   archives: List[IArchiveRef]
                 ) extends IGroupItem with IReferencable {
  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("description", description)
    buffer.add("responsible", responsible)
    buffer.add("archives", archives)

    buffer
  }
}

//
// Archive
//

trait IArchiveItem extends IAPIObjectItem {
  val kind: String = "archive"
  val parent: Some[IGroupRef]

  /** the id of the archive $parent.id/$name */
  val id: String

  /** human-readable title of the group */
  val title: IAPIObjectItem.HTML

  /** a short teaser description of the group */
  val teaser: IAPIObjectItem.HTML

  def toJSONBuffer: JSONObjectBuffer = {
    val buffer = new JSONObjectBuffer

    buffer.add("title", title)
    buffer.add("teaser", teaser)

    buffer
  }
}

/** a reference to a MathHub Archive */
case class IArchiveRef(
                        override val parent: Some[IGroupRef],
                        override val id: String,
                        override val title: IAPIObjectItem.HTML,
                        override val teaser: IAPIObjectItem.HTML
                      ) extends IArchiveItem with IReference with INarrativeParentRef

/** a full description of a MathHub Archive */
case class IArchive(
                        override val parent: Some[IGroupRef],
                        override val id: String,
                        override val title: IAPIObjectItem.HTML,
                        override val teaser: IAPIObjectItem.HTML,

                        description: IAPIObjectItem.HTML,
                        responsible: List[String],
                        narrativeRoot: List[INarrativeElement]
                      ) extends IArchiveItem with IReferencable {
  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("description", description)
    buffer.add("responsible", responsible)
    buffer.add("narrativeRoot", narrativeRoot)

    buffer
  }
}

//
// NARRATION
//

/** a narrative element inside an archive */
trait INarrativeElement extends IAPIObjectItem // TODO: URIS?

/** anything that can be the parent of a narrative element */
trait INarrativeParentRef extends IReference


trait IDocumentItem extends IAPIObjectItem {
  val kind = "document"
  val parent: Some[INarrativeParentRef]

  /** the uri of this document */
  val id: IAPIObjectItem.URI

  /** the name of this document */
  val name: String

  def toJSONBuffer: JSONObjectBuffer = {
    val buffer = new JSONObjectBuffer

    buffer.add("name", name)

    buffer
  }
}

/** a reference to a document */
case class IDocumentRef(
                       override val parent: Some[INarrativeParentRef],
                       override val id: IAPIObjectItem.URI,
                       override val name: String
                       ) extends IDocumentItem with IReference with INarrativeParentRef

/** a document of content */
case class IDocument(
                      override val parent: Some[INarrativeParentRef],
                      override val id: IAPIObjectItem.URI,
                      override val name: String,

                      decls: List[INarrativeElement]
                    ) extends IDocumentItem with IReferencable with INarrativeElement {
  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("decls", decls)

    buffer
  }

}

/** an opaque element */
case class IOpaqueElement(
                         override val parent: Some[INarrativeParentRef],

                         text: String
                         ) extends IReferencable with INarrativeElement {
  val kind: String = "opaque"
  val id: String = ""

  def toJSONBuffer: JSONObjectBuffer = {
    val buffer = new JSONObjectBuffer

    buffer.add("text", text)

    buffer
  }

}

//
// CONTENT
//

trait IModuleItem extends IAPIObjectItem {
  val parent: Some[INarrativeParentRef]

  /** name of the module */
  val name: String

  /** the uri of this IModuleItem */
  val id: IAPIObjectItem.URI

  def toJSONBuffer: JSONObjectBuffer = {
    val buffer = new JSONObjectBuffer

    buffer.add("name", name)

    buffer
  }
}

/** a reference to a module */
trait IModuleRef extends IModuleItem with IReference with INarrativeElement

/** a reference to a theory */
case class ITheoryRef(
                     override val parent: Some[INarrativeParentRef],
                     override val id: IAPIObjectItem.URI,
                     override val name: String
                     ) extends IModuleRef {
  val kind: String = "theory"
}


/** a reference to a view */
case class IViewRef(
                       override val parent: Some[INarrativeParentRef],
                       override val id: IAPIObjectItem.URI,
                       override val name: String
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

    buffer.add("presentation", presentation)
    buffer.add("source", source)

    buffer
  }
}

/** a description of a theory */
case class ITheory(
                       override val parent: Some[INarrativeParentRef],
                       override val id: IAPIObjectItem.URI,
                       override val name: String,

                       override val presentation: IAPIObjectItem.HTML,
                       override val source: Option[String],

                       meta: Option[ITheoryRef]
                     ) extends IModule {
  val kind: String = "theory"

  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("meta", meta)

    buffer
  }
}

/** a description of a view */
case class IView(
                    override val parent: Some[INarrativeParentRef],
                    override val id: IAPIObjectItem.URI,
                    override val name: String,

                    override val presentation: IAPIObjectItem.HTML,
                    override val source: Option[String],

                    domain: ITheoryRef,
                    codomain: ITheoryRef
                  ) extends IModule {
  val kind: String = "theory"

  override def toJSONBuffer: JSONObjectBuffer = {
    val buffer = super.toJSONBuffer

    buffer.add("domain", domain)
    buffer.add("codomain", codomain)

    buffer
  }
}


//
// Helper object
//

/**
  * Any object exposed by the API
  */
trait IAPIObjectItem {

  /** the kind of object this represents */
  val kind: String

  /** weather this object is a reference or a full description */
  val ref: Boolean

  /** the id of this object, if any */
  val id: String

  /** the parent of this object, if any */
  val parent: Option[IReference]



  /** serialiazes this object into a JSONObject Buffer */
  protected def toJSONBuffer: JSONObjectBuffer

  /** serializes this object into a JSON Object */
  final def toJSON: JSON = {
    val buffer = toJSONBuffer

    // add shared attributes
    buffer.add("kind", kind)
    buffer.add("ref", ref)
    buffer.add("id", id)
    buffer.add("parent", parent)

    buffer.result()
  }
}

object IAPIObjectItem {
  type HTML = String
  type URI = String

  /** so that we can convert all the things into JSON */
  implicit def converter[T <: IAPIObjectItem]: JSONConverter[T] = new JSONConverter[T] {
    def toJSON(obj: T): JSON = obj.toJSON
    def fromJSONOption(j: JSON): Option[T] = None
  }
}