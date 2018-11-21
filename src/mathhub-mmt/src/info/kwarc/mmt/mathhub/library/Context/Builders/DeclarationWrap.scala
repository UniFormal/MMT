package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.utils
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IDeclarationRef, IDeclaration}

trait DeclarationWrap { this: MathHubAPIContext =>
  /** gets a reference to a module */
  def getDeclarationRef(id: String): Option[IDeclarationRef] = utils.firstDefined(
    { _ => getStructureRef(id)},
    { _ => getConstantRef(id)},
    { _ => getRuleRef(id)},
    { _ => getNestedModuleRef(id)},
  )


  /** gets a module */
  def getDeclaration(id: String): Option[IDeclaration] = utils.firstDefined(
    { _ => getStructure(id)},
    { _ => getConstant(id)},
    { _ => getRule(id)},
    { _ => getNestedModule(id)},
  )
}