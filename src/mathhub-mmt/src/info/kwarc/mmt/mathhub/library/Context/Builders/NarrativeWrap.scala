package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IDocumentParentRef, INarrativeElement}

trait NarrativeWrap { this: Builder =>
  /** gets a reference to a narrative parent */
  def getNarrativeParentRef(id: String): Option[IDocumentParentRef] = {
    getArchiveRef(id).map(Some(_)).getOrElse(getDocumentRef(id)) // ArchiveRef | DocumentRef
  }

  /** gets a narrative element */
  def getNarrativeElement(id: String): Option[INarrativeElement] = {
    // IOpaqueElement | IDocument | IDocumentRef | IModuleRef

    // TODO: This always sends a full document and not a reference
    getOpaqueElement(id).map(Some(_))
      .getOrElse(getDocument(id)).map(Some(_))
      .getOrElse(getModuleRef(id))
  }

}
