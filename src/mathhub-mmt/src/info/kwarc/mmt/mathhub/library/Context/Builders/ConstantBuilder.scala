package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.symbols.Constant
import info.kwarc.mmt.mathhub.library.{IConstant, IDeclarationRef}

trait ConstantBuilder { this: Builder =>

  /** gets a reference to a constant */
  def getConstantRef(id: String): Option[IDeclarationRef] = getReferenceOf(classOf[IDeclarationRef], id)

  /** builds a reference to a constant */
  protected def buildConstantReference(constant: Constant) : Option[IDeclarationRef] = {
    val parent = getModuleRef(constant.parent.toPath)
      .getOrElse(return buildFailure(constant.parent.toPath, "getModuleRef(constant.parent)"))

    Some(
      IDeclarationRef(
        constant.path.toPath, /* id */
        constant.name.toPath, /* name */
        Some(parent), /* parent */
        "constant" /* declaration */
      )
    )
  }

  /** gets a structure */
  def getConstant(id: String): Option[IConstant] = getObjectOf(classOf[IConstant], id)

  /** builds a constant representation */
  protected def buildConstant(constant: Constant) : Option[IConstant] = {
    val ref = getConstantRef(constant.path.toPath)
      .getOrElse(return buildFailure(constant.path.toPath, "getConstantRef(constant.id)"))

    Some(IConstant(
      ref.id, ref.name, ref.parent,
      getStats(ref.id),

      getDeclarations(constant),
      getComponents(constant),

      constant.rl,
      constant.alias.map(_.toPath)
    ))
  }
}