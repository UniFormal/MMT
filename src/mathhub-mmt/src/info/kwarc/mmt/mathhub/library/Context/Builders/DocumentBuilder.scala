package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.archives
import info.kwarc.mmt.api.archives.ImporterAnnotator
import info.kwarc.mmt.api.archives.lmh.LMHHubArchiveEntry
import info.kwarc.mmt.api.documents.{Document, NRef}
import info.kwarc.mmt.api.metadata.HasMetaData
import info.kwarc.mmt.api.opaque.OpaqueElement
import info.kwarc.mmt.api.parser.SourceRef
import info.kwarc.mmt.api.utils.{File, mmt}
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IDocument, IDocumentRef, ISourceReference, IModuleRef}

trait DocumentBuilder { this: Builder =>
  /** gets a reference to a document */
  def getDocumentRef(id: String): Option[IDocumentRef] = getReferenceOf(classOf[IDocumentRef], id)

  /** builds a reference to a document */
  protected def buildDocumentReference(document: Document) : Option[IDocumentRef] = {

    log(s"parent of ${document} is ${document.parent.toPath}")

    // if we are the narrationBase of an archive, that is our parent
    val parent = mathHub.entries_.find({
      case ae: LMHHubArchiveEntry => ae.archive.narrationBase.toString == document.path.toPath
      case _ => false
    }) match {
      case Some(ae) => getArchiveRef(ae.id)
      // if not, the parent is simply a document
      case None => getDocumentRef(document.path.^.toPath) // TODO: is parent working properly?
    }


    Some(
      IDocumentRef(
        parent.map(Some(_))
          .getOrElse(return buildFailure(document.path.toPath, "getRef(document.parent)")),/* parent */
        document.path.toPath, /* id */
        document.name.last.toPath /* name */
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
    // TODO: Implement other types and section references
    val decls = document.getDeclarations.flatMap({

      // IOpaqueElement
      case o: OpaqueElement => getOpaqueElement(o.path.toPath).map(Some(_))
        .getOrElse(return buildFailure(document.path.toPath, s"getOpaqueElement(document.decls[${o.path.toPath}])"))

      // IDocument
      case d: Document => getDocument(d.path.toPath).map(Some(_))
        .getOrElse(return buildFailure(document.path.toPath, s"getDocument(document.decls[${d.path.toPath}])"))

      // Reference, currently support IDocumentRef and IModuleRef
      case n: NRef => {
        val path = n.target.toPath
        getReference(path) match {
          case Some(mr: IModuleRef) => Some(mr)
          case Some(dr: IDocumentRef) => Some(dr)

          // TODO: Support other types once they have been implemented
          case _ => {
            log(s"buildDocument: ignoring unknown reference child ${path} of ${document.path.toPath} (not Module, Document)")
            None
          }
        }
      }
      // TODO: Implement URIs
      case d => {
        log(s"buildDocument: ignoring unknown child ${d.path.toPath} of ${document.path.toPath} (not OpaqueElement, Document or MRef)")
        None
      }
    })

    Some(IDocument(
      ref.parent, ref.id, ref.name,

      tags, // TODO: Implement tags
      source,

      getStats(ref.id),
      decls
    ))
  }

  /** gets a string representing the source code of a specific object */
  protected def getSourceOf(md: HasMetaData): Option[String] = {
    // get the source ref string
    val sourceRef = md.metadata.getLinks(mmt.mmtbase ? "metadata" ? "sourceRef")
      .headOption.map(SourceRef.fromURI).getOrElse(return None)

    // try to resolve the physical path to the source in the source Dimension
    val (archive, path) = controller.backend.resolveLogical(sourceRef.container).getOrElse(return None)
    val sourcePath = path.foldLeft[File](archive.root / archive.resolveDimension(archives.source))((p, c) => p / c)
    if(!sourcePath.isFile){ return None }

    // take the appropriate lines from the source file
    val sourceLines = File.read(sourcePath).split("\n")
      .slice(sourceRef.region.start.line, sourceRef.region.end.line + 1)

    if(sourceLines.length > 1){
      // we have > 1 source line
      val firstLine = sourceLines.headOption.map(_.drop(sourceRef.region.start.column))
      val middleLines = sourceLines.drop(1).dropRight(1).toList
      val lastLine = sourceLines.lastOption.map(l => l.dropRight(l.length - sourceRef.region.end.column))

      Some((firstLine :: middleLines :: (lastLine :: Nil)).mkString("\n"))
    } else {
      // we have exactly one line
      Some(sourceLines.head.slice(sourceRef.region.start.column, sourceRef.region.end.column + 1))
    }
  }
}
