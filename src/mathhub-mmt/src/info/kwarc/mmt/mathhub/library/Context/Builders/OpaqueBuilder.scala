package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.{DPath, Path}
import info.kwarc.mmt.api.opaque.OpaqueElement
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IDocumentRef, IOpaqueElement, IOpaqueElementRef}

trait OpaqueBuilder { this: Builder =>

  /** gets a reference to an opaque element */
  def getOpaqueElementRef(id: String): Option[IOpaqueElementRef] = getReferenceOf(classOf[IOpaqueElementRef], id)

  /** builds a reference to an opaque element */
  protected def buildOpaqueReference(opaque: OpaqueElement) : Option[IOpaqueElementRef] = Some(
    IOpaqueElementRef(
      getDocumentRef(opaque.path.^.toPath).map(Some(_)) // TODO: is parent working properly?
        .getOrElse(return buildFailure(opaque.path.toPath, "getDocumentRef(opaque.parent)")),/* parent */
      opaque.path.toPath, /* id */
      opaque.name.last.toPath /* name */
    )
  )

  /** gets an opaque element */
  def getOpaqueElement(id: String): Option[IOpaqueElement] = getObjectOf(classOf[IOpaqueElement], id)

  /** builds an opaque representation */
  protected def buildOpaque(opaque: OpaqueElement): Option[IOpaqueElement] = {
    val ref = getOpaqueElementRef(opaque.path.toPath)
      .getOrElse(return buildFailure(opaque.path.toPath, "getOpaqueElementRef(opaque.id)"))

    Some(IOpaqueElement(
      ref.parent, ref.id, ref.name,
      getStats(ref.id),

      opaque.format,
      opaque.raw.toString
    ))
  }
  /** builds a pseudo opqaue-element with the given content */
  protected def pseudoOpaqueElement(parent: IDocumentRef, parentPath: DPath, text: String) : IOpaqueElement = {
    val path = parentPath ? "pseudo"

    IOpaqueElement(
      Some(parent), path.toPath, "pseudo",
      None,

      "text",
      text
    )
  }
}
