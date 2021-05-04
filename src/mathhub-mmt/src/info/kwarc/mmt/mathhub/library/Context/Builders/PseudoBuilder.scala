package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.archives.{LMHHubArchiveEntry, content}
import info.kwarc.mmt.mathhub.library.Context.Builders.Special.VirtualTree
import info.kwarc.mmt.mathhub.library.{IArchive, IDocument, IDocumentParentRef, IDocumentRef, INarrativeElement, IOpaqueElement, IOpaqueElementRef, IReferencable, IReference}

/**
  * Builds elements found within a virtual tree.
  */
class PseudoBuilder(val tree: VirtualTree) {
  private val docuri = PseudoTreeURL("pseudo-tree://" + tree.key + "/")
  private val opuri = PseudoLeafURL("pseudo-leaf://" + tree.key + "/")

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

  /** builds a document reference for the provided tree node */
  def buildTreeRef(builder: Builder, uri: String): Option[IDocumentRef] = {
    val (archive, path) = docuri(uri).getOrElse(return None)
    if (!tree.exists(archive, path)) return None

    // find the parent
    val parent = if(path.nonEmpty) {
      // simple case: move up one folder
      builder.getDocumentRef(docuri(archive, path.dropRight(1))).getOrElse(return None)
    } else {
      // get the dpath of the archive narrative root
      val dpath = builder.getArchiveNarrativeRoot(archive).getOrElse(return None)
      // find the document or use a pseudo reference!
      builder.getDocumentRef(dpath.toString) match {
        case Some(dref) => dref
        case None => builder.makeDPathReference(dpath).getOrElse(return None)
      }
    }

    // return the document!
    Some(IDocumentRef(
      parent = Some(parent),
      id = uri,
      name = tree.displayName(archive, path),
    ))
  }

  /** builds a document for the provided uri */
  def buildTreeDoc(builder: Builder, uri: String): Option[IDocument] = {
    val ref = buildTreeRef(builder, uri).getOrElse(return None)
    val (archive, path) = docuri(uri).get // when none, we already returned above

    // we have an inner node and should just build it's children!
    val children: List[INarrativeElement]  = if (!tree.hasContent(archive, path)) {
      tree.children(archive, path).map(child => {
        val childPath = path ::: (child :: Nil)
        val childURI = docuri(archive, childPath)
        builder.getDocumentRef(childURI).getOrElse(return None)
      })

    // we have a leaf node and should generate the actual inner nodes!
    } else {
      val contentNodes = tree.content(archive, path)
      contentNodes.map(name => {
        val uri = opuri(archive, path, name)
        buildContentPtr(builder, uri).getOrElse(return None)
      })
    }

    val sourceRef = builder.getArchiveRef(archive).flatMap(tree.sourceRef(_, path))

    Some(IDocument(
      parent = ref.parent,
      id = ref.id,
      name = ref.name,

      tags = Nil,
      sourceRef = sourceRef,
      statistics = None,
      declarations = children,
    ))
  }

  /** builds a narrative element to be included inside a tree leaf document */
  private def buildContentPtr(builder: Builder, uri: String): Option[INarrativeElement] = {
    val (archive, path, name) = opuri(uri).getOrElse(return None)
    if (!tree.existsContent(archive, path, name)) return None

    val parent = builder.getDocumentRef(docuri(archive, path)).getOrElse(return None)

    Some(tree.contentPointer(
      archive = archive,
      path = path,
      name = name,
      parent = parent,
      id = uri
    ))
  }


  /** builds a reference to a content object inside a leaf */
  def buildContentRef(builder: Builder, uri: String): Option[IReference] = {
    val (archive, path, name) = opuri(uri).getOrElse(return None)
    if (!tree.existsContent(archive, path, name)) return None

    val parent = builder.getDocumentRef(docuri(archive, path)).getOrElse(return None)

    Some(tree.contentRef(
      archive = archive,
      path = path,
      name = name,
      parent = parent,
      id = uri,
    ))
  }

  /** builds a content object (found within a leaf) */
  def buildContentObj(builder: Builder, uri: String): Option[IReferencable] = {
    val (archive, path, name) = opuri(uri).getOrElse(return None)
    if (!tree.existsContent(archive, path, name)) return None

    // parent is the containing document!
    val parent = builder.getDocumentRef(docuri(archive, path)).getOrElse(return None)

    Some(tree.contentObj(
      archive = archive,
      path = path,
      name = name,
      parent = parent,
      id = uri
    ))
  }

}

/** Mapping between URI and (archive, path) pair */
case class PseudoTreeURL(prefix: String) {
  def apply(id: String): Option[(String, List[String])] = {
    if(!id.startsWith(prefix)) { return None }

    id.substring(prefix.length).split("/", -1).toList match {
      case gid::aid::path => Some(gid + "/" + aid, path)
      case _ => None
    }
  }
  def apply(archive: String, path: List[String]): String = {
    prefix + (archive :: path).mkString("/")
  }
}

/** Mapping between URI and (archive, path) pair */
case class PseudoLeafURL(prefix: String) {
  private val url = PseudoTreeURL(prefix)
  def apply(id: String): Option[(String, List[String], String)] = {
    // trim off part after a single comma!
    val (splitID, name) = id.split(",", -1).toList match {
      case before :: after :: Nil => (before, after)
      case _ => return None
    }

    // apply the normal URL mapping!
    url(splitID) match {
      case Some((archive, path)) => Some((archive, path, name))
      case None => None
    }
  }
  def apply(archive: String, path: List[String], name: String): String = {
    url.apply(archive, path) + "," + name
  }
}