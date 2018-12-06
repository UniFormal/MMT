package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.Obj
import info.kwarc.mmt.api.presentation.{HTMLPresenter, MathMLPresenter}
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.{AbstractObjectContainer, AbstractTermContainer, DeclarationComponent, utils}
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IComponent, IDeclaration, IDeclarationRef}

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

  /** gets a list of components */
  protected def getComponents(declaration: Declaration): List[IComponent] = {
    declaration.getComponents.flatMap({
      case DeclarationComponent(key, oc: AbstractObjectContainer) if oc.isDefined =>
          Some(IComponent(key.toString, "object", oc.get.map(getPresentationOf).get))
      case DeclarationComponent(key, nc: NotationContainer) if nc.isDefined =>
        Some(IComponent(key.toString, "notation", nc.toString))
      case t =>
        logDebug(s"ignoring unknown or empty DeclarationComponent ${t.key} of $declaration")
        None
    })
  }


  private def getPresentationOf(obj: Obj): String = {
    // TODO: Customize the object presenter
    val presenter = controller.extman.get(classOf[MathMLPresenter]).head // .head safe because present by default
    presenter.asString(obj, None)
  }
}