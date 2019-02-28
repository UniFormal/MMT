package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.{DPath, LocalName, Path, archives}
import info.kwarc.mmt.api.archives.{ImporterAnnotator, LMHHubArchiveEntry}
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.opaque.OpaqueElement
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.mathhub.library.{IDocument, IDocumentParentRef, IDocumentRef, ISourceReference}

trait DocumentBuilder { this: Builder =>
  /** gets a reference to a document */
  def getDocumentRef(id: String): Option[IDocumentRef] = getReferenceOf(classOf[IDocumentRef], id)

  /** builds a reference to a document */
  protected def buildDocumentReference(document: Document) : Option[IDocumentRef] = makeDocumentRef(document.path)

  /** optimisation to quickly build a reference to a parent without loading more content */
  protected def getDocumentReference(dRef: DRef): Option[IDocumentRef] = getReferenceOrElse(classOf[IDocumentRef], dRef.path.toPath) {
    makeDocumentRef(dRef.target)
  }

  private def makeDocumentRef(path: DPath): Option[IDocumentRef] = {
    // if we are the narrationBase of an archive, that is our parent
    val parent = mathHub.entries_.find({
      case ae: LMHHubArchiveEntry => ae.archive.narrationBase.toString == path.toPath
      case _ => false
    }) match {
      case Some(ae) => getArchiveRef(ae.id)
      // if not, the parent is simply a document
      case None => getDocumentRef(path.^.toPath)
    }

    Some(
      IDocumentRef(
        parent.map(Some(_))
          .getOrElse(return buildFailure(path.toPath, "getRef(document.parent)")),/* parent */
        path.toPath, /* id */
        path.name.last.toPath /* name */
      )
    )
  }

  /** gets a document */
  def getDocument(id: String): Option[IDocument] = getObjectOf(classOf[IDocument], id)

  /** builds a document representation */
  protected def buildDocument(document: Document): Option[IDocument] = {
    val ref = getDocumentRef(document.path.toPath)
      .getOrElse(return buildFailure(document.path.toPath, "getDocumentRef(document.id)"))

    // (archive / archives.source / path).setExtension("mmt")

    // try to resolve the path to the document
    val source =
      SourceRef.get(document).map(_.container).flatMap(controller.backend.resolveLogical).map { case (archive, path) => ISourceReference(
        getArchiveRef(archive.id).getOrElse(return buildFailure(document.path.toPath, s"getArchiveRef(document.source)")),
        getArchive(archive.id).getOrElse(return buildFailure(document.path.toPath, s"getArchiveRef(document.version)")).version,
        Some(archive.root.relativize(archive / archives.source / path).toString)
      )}

    // TODO: More tags here
    val tags = ImporterAnnotator.get(document).toList.filter(IDocument.knownTags.contains)

    // find all the declarations
    val decls = document.getDeclarations.flatMap({

      // IOpaqueElement
      case o: OpaqueElement => getOpaqueElement(o.path.toPath).map(Some(_))
        .getOrElse(return buildFailure(document.path.toPath, s"getOpaqueElement(document.decls[${o.path.toPath}])"))

      // IDocument
      case d: Document => getDocument(d.path.toPath).map(Some(_))
        .getOrElse(return buildFailure(document.path.toPath, s"getDocument(document.decls[${d.path.toPath}])"))

      // IModuleRef
      case m: MRef => getModuleReference(m)

      // IDocumentRef
      case d: DRef => getDocumentReference(d)

      case d =>
        logDebug(s"buildDocument: ignoring unknown child ${d.path.toPath} of ${document.path.toPath} (not OpaqueElement, Document or DRef, MRef)")
        None
    })

    Some(IDocument(
      ref.parent, ref.id, ref.name,

      tags,
      source,

      getStats(ref.id),
      decls
    ))
  }
}
