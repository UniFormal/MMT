package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.symbols.NestedModule
import info.kwarc.mmt.mathhub.library
import info.kwarc.mmt.mathhub.library.{IDeclarationRef, INestedModule}

trait NestedModuleBuilder { this: Builder =>

  /** gets a reference to a nested module */
  def getNestedModuleRef(id: String): Option[IDeclarationRef] = getReferenceOf(classOf[IDeclarationRef], id)

  /** builds a reference to a nested module */
  protected def buildNestedModuleReference(nested: NestedModule) : Option[IDeclarationRef] = {
    val parent = getModuleRef(nested.parent.toPath)
      .getOrElse(return buildFailure(nested.parent.toPath, "getModuleRef(nested.parent)"))

    Some(
      IDeclarationRef(
        nested.path.toPath, /* id */
        nested.name.toPath, /* name */
        Some(parent), /* parent */
        "rule" /* declaration */
      )
    )
  }

  /** gets a nested module */
  def getNestedModule(id: String): Option[INestedModule] = getObjectOf(classOf[INestedModule], id)

  /** builds a rule representation */
  protected def buildNestedModule(nested: NestedModule) : Option[INestedModule] = {
    val ref = getNestedModuleRef(nested.path.toPath)
      .getOrElse(return buildFailure(nested.path.toPath, "getNestedModuleRef(nested.id)"))

    val mod = getModuleRef(nested.module.path.toPath)
      .getOrElse(return buildFailure(nested.path.toPath, "getModuleRef(nested.mod)"))

    Some(INestedModule(
      ref.id, ref.name, ref.parent,
      getStats(ref.id),

      getDeclarations(nested),
      getComponents(nested), // TODO: List of components

      mod
    ))
  }
}