package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.documents.MRef
import info.kwarc.mmt.api.{ContentElement, utils}
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IDeclarationRef, IModule, IModuleRef}

trait ModuleWrap { this: MathHubAPIContext =>
  /** gets a reference to a module */
  def getModuleRef(id: String): Option[IModuleRef] = utils.firstDefined(
    { _ => getViewRef(id) },
    { _ => getTheoryRef(id) }
  )

  /** gets a reference to a module with as few references as possible */
  def getModuleReference(mref: MRef): Option[IModuleRef] = getReferenceOrElse(classOf[IModuleRef], mref.path.toPath) {
    Some(IModuleRef(mref.target.toPath, mref.target.name.toPath))
  }

  /** gets a module */
  def getModule(id: String): Option[IModule] = utils.firstDefined(
    { _ => getView(id) },
    { _ => getTheory(id) }
  )

  /** gets the declarations within a content element */
  protected def getDeclarations(element: ContentElement): List[IDeclarationRef] = {
    element.getDeclarations.collect({case d: Declaration => d.path.toPath})
      .flatMap(getDeclarationRef)
  }
}
