package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.Obj
import info.kwarc.mmt.api.presentation.{HTMLPresenter, MathMLPresenter}
import info.kwarc.mmt.api.symbols.Declaration
import info.kwarc.mmt.api.{AbstractObjectContainer, DeclarationComponent, utils}
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
      case DeclarationComponent(key, oc: AbstractObjectContainer) =>
          Some(IComponent(key.toString, "object", oc.get.map(getPresentationOf).getOrElse("")))
      case DeclarationComponent(key, nc: NotationContainer) =>
          Some(IComponent(key.toString, "notation", nc.toString))
      case t =>
        log(s"ignoring unknown DeclarationComponent $t")
        None
    })
  }


  private def getPresentationOf(obj: Obj): String = {
    // TODO: Customize the object presenter
    val presenter = controller.extman.get(classOf[MathMLPresenter]).head
    presenter.asString(obj, None)
  }
}