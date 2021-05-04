package info.kwarc.mmt.mathhub.library.Context.Builders.Special

import info.kwarc.mmt.api.archives.MathHub
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.mathhub.library.{IDocumentParentRef, IDocumentRef, IHubReference, INarrativeElement, IOpaqueElement, IOpaqueElementRef, IReferencable, IReference, ISourceReference}

/**
  * Represents a "virtual" tree inserted directly into the root of archives.
  *
  * Nodes are identified by a (archive, path) pair, where archive is the ID of the current archive
  * and path is a list of strings.
  *
  * A path may not contain a '/' or ','.
  *
  * Each node is either an "inner node" or a "leaf node".
  *
  * An inner node may have children, a leaf node may not.
  * Paths to child nodes consist of the path to the parent node and the name of the child node.
  *
  * Leaf nodes are represented as an IDocument; that is the contain a list of narrative elements.
  */
abstract class VirtualTree(
                            protected val controller: Controller,
                            protected val mathhub: MathHub) {
  // internal identifier used in virtual URLs
  val key: String

  // used in the interface
  val displayName: String

  /**
    * Checks if this VirtualTree is applicable to the provided archive.
    * Applicable means that it is displayed on the archive page.
    */
  def applicable(archive: String): Boolean

  /** Check if a given path exists */
  def exists(archive: String, path: List[String]): Boolean = {
    if (!applicable(archive)) return false
    path.indices.drop(1).foreach(n => {
      // check that the children at this point contains the next child!
      if (!children(archive, path.take(n)).contains(path(n))){
        return false
      }
    })
    true
  }

  /** Return the children of an tree node */
  def children(archive: String, path: List[String]): List[String]

  /** get the displayName of a tree node */
  def displayName(archive: String, path: List[String]): String = path.lastOption.getOrElse(displayName)

  /** get the sourceReference of a node */
  def sourceRef(archive: IHubReference, path: List[String]): Option[ISourceReference]

  /** checks if a node is an inner tree node (without content) or a leaf node (with content) */
  def hasContent(archive: String, path: List[String]): Boolean

  /** gets a list of content nodes for a provided node */
  def content(archive: String, path: List[String]): List[String]

  /** checks if a given content node exists */
  def existsContent(archive: String, path: List[String], name: String): Boolean = {
    exists(archive, path) && hasContent(archive, path) && content(archive, path).contains(name)
  }

  /** gets a pointer to the provided content node, that will be inserted into the parent document */
  def contentPointer(archive: String, path: List[String], name: String, parent: IDocumentRef, id: String) : INarrativeElement

  /** gets a reference to the content object, that will be returned when a specific request to a content uri is made */
  def contentRef(archive: String, path: List[String], name: String, parent: IDocumentRef, id: String) : IReference

  /** gets an actual content object, that will be returned when a specific request to a content uri is made */
  def contentObj(archive: String, path: List[String], name: String, parent: IDocumentRef, id: String): IReferencable
}

abstract class OpaqueTree(
                           controller: Controller,
                           mathhub: MathHub) extends VirtualTree(controller, mathhub) {

  /** gets a pointer to the provided content node, that will be inserted into the parent document */
  def contentPointer(archive: String, path: List[String], name: String, parent: IDocumentRef, id: String) : INarrativeElement = {
    val (format, content) = opaqueContent(archive, path, name)

    IOpaqueElement(
      parent = Some(parent),
      name = opaqueName(archive, path, name),
      id = id,

      statistics = None,
      contentFormat = format,
      content = content,
    )
  }

  /** gets a reference to the content object, that will be returned when a specific request to a content uri is made */
  def contentRef(archive: String, path: List[String], name: String, parent: IDocumentRef, id: String) : IReference = {
    IOpaqueElementRef(
      parent = Some(parent),
      name = opaqueName(archive, path, name),
      id = id,
    )
  }

  /** gets an actual content object, that will be returned when a specific request to a content uri is made */
  def contentObj(archive: String, path: List[String], name: String, parent: IDocumentRef, id: String): IReferencable = {
    contentPointer(archive, path, name, parent, id).asInstanceOf[IOpaqueElement]
  }

  /** return a list of opaque element names in the given leaf node */
  def content(archive: String, path: List[String]): List[String]

  /** for the given leaf node, return the content */
  def opaqueName(archive: String, path: List[String], name: String): String = name

  /** for the given leaf node, return a pair of (format, content), format is typically html */
  def opaqueContent(archive: String, path: List[String], name: String): (String, String)
}
