package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.symbols.Structure
import info.kwarc.mmt.mathhub.library.{IDeclarationRef, IStructure}

trait StructureBuilder { this: Builder =>

  /** gets a reference to a structure */
  def getStructureRef(id: String): Option[IDeclarationRef] = getReferenceOf(classOf[IDeclarationRef], id)

  /** builds a reference to a structure */
  protected def buildStructureReference(structure: Structure) : Option[IDeclarationRef] = {
    val parent = getModuleRef(structure.parent.toPath)
      .getOrElse(return buildFailure(structure.parent.toPath, "getModuleRef(structure.parent)"))

    Some(
      IDeclarationRef(
        structure.path.toPath, /* id */
        structure.name.toPath, /* name */
        Some(parent), /* parent */
        "structure" /* declaration */
      )
    )
  }

  /** gets a structure */
  def getStructure(id: String): Option[IStructure] = getObjectOf(classOf[IStructure], id)

  /** builds a structure representation */
  protected def buildStructure(structure: Structure) : Option[IStructure] = {
    val ref = getStructureRef(structure.path.toPath)
      .getOrElse(return buildFailure(structure.path.toPath, "getStructureRef(structure.id)"))

    Some(IStructure(
      ref.id, ref.name, ref.parent,
      getStats(ref.id),

      getDeclarations(structure),
      getComponents(structure),

      structure.isImplicit,
      structure.isInclude
    ))
  }
}