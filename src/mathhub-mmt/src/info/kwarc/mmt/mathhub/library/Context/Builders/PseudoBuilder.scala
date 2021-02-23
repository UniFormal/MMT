package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.archives.LMHHubArchiveEntry
import info.kwarc.mmt.mathhub.library.Context.Builders.Special.VirtualTree
import info.kwarc.mmt.mathhub.library.{IArchive, IDocument, IDocumentParentRef, IDocumentRef, INarrativeElement, IOpaqueElement, IOpaqueElementRef}

/**
  * Builds a pseudo tree living under an archive.
  * Inner nodes are reprensented as documents, leaf nodes as opaque elements.
  *
  * @param tree
  */
class PseudoBuilder(val tree: VirtualTree) {
  private val docuri = PseudoBuilderURL("pseudo-document://" + tree.key + "/")
  private val opuri = PseudoBuilderURL("pseudo-opaque://" + tree.key + "/")

  /**
    * Add a reference to the root document induces by the OpaqueTree (if applicable).
    * @return
    */
  def extendArchive(builder: Builder, archive: IArchive): IArchive = {
    if(!tree.applicable(archive.id)) return archive

    // add a reference to the root document!
    val ref = builder.getDocumentRef(docuri(archive.id, Nil)).getOrElse(return archive)
    val declarations = archive.narrativeRoot.declarations ::: (ref :: Nil)

    // and return the archive
    archive.copy(narrativeRoot = archive.narrativeRoot.copy(declarations = declarations))
  }

  /** builds a document reference for the provided uri */
  def buildDocumentRef(builder: Builder, uri: String): Option[IDocumentRef] = {
    val (archive, path) = docuri(uri).getOrElse(return None)
    if (!tree.applicable(archive)) return None
    if (!tree.exists(archive, path)) return None

    // find the path
    val parentURI = if(path.nonEmpty) {
      docuri(archive, path.dropRight(1))
    } else {
      findNarrativeRoot(builder, archive).getOrElse(return None)
    }

    val parent = builder.getDocumentRef(parentURI).getOrElse(return None)

    // return the document!
    Some(IDocumentRef(
      parent = Some(parent),
      id = uri,
      name = tree.getName(archive, path),
    ))
  }

  /** finds the narrative root for a specific archive */
  private def findNarrativeRoot(builder: Builder, archive: String): Option[String] = {
    builder.mathHub.installedEntries.foreach({
      case ae: LMHHubArchiveEntry if ae.id == archive =>
        return Some(ae.archive.narrationBase.toString)
      case _ =>
    })
    None
  }

  /** builds a document for the provided uri */
  def buildDocument(builder: Builder, uri: String): Option[IDocument] = {
    val ref = buildDocumentRef(builder, uri).getOrElse(return None)
    val (archive, path) = docuri(uri).get // when none, we already returned above

    // fetch the children of "real" inner nodes
    // these point to their respective "document" URIs
    val children: List[INarrativeElement]  = if (!tree.isLeaf(archive, path)) {
      tree.children(archive, path).map(child => {
        val childPath = path ::: (child :: Nil)
        val childURI = docuri(archive, childPath)
        builder.getDocumentRef(childURI).getOrElse(return None)
      })

    // "fake" inner nodes contain only the opaque element
    } else {
      List(builder.getOpaqueElement(opuri(archive, path)).getOrElse(return None))
    }

    Some(IDocument(
      parent = ref.parent,
      id = ref.id,
      name = ref.name,

      tags = Nil,
      sourceRef = None,
      statistics = None,
      declarations = children,
    ))
  }

  /** builds an opaque element reference for the provided uri */
  def buildOpaqueRef(builder: Builder, uri: String): Option[IOpaqueElementRef] = {
    val (archive, path) = opuri(uri).getOrElse(return None)
    if (!tree.applicable(archive)) return None
    if (!tree.exists(archive, path)) return None
    if (!tree.isLeaf(archive, path)) return None

    // the parent is the fake inner node
    val parent = builder.getDocumentRef(docuri(archive, path)).getOrElse(return None)

    Some(IOpaqueElementRef(
      parent = Some(parent),
      id = uri,
      name = tree.getName(archive, path),
    ))
  }

  def buildOpaque(builder: Builder, uri: String): Option[IOpaqueElement] = {
    val ref = buildOpaqueRef(builder, uri).getOrElse(return None)
    val (archive, path) = opuri(uri).get // when none, already returned above

    val (format, content) = tree.getLeafContent(archive, path)

    return Some(IOpaqueElement(
      parent = ref.parent,
      id = ref.id,
      name = ref.name,

      statistics = None,
      contentFormat = format,
      content = content,
    ))
  }

}

/** Mapping between URI and (archive, path) pair */
case class PseudoBuilderURL(prefix: String) {
  def apply(id: String): Option[(String, List[String])] = {
    if(!id.startsWith(prefix)) { return None }

    id.substring(prefix.length).split("/").toList match {
      case gid::aid::path => Some(gid + "/" + aid, path)
      case _ => None
    }
  }
  def apply(archive: String, path: List[String]): String = {
    prefix + (archive :: path).mkString("/")
  }
}