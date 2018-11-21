package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.modules.View
import info.kwarc.mmt.mathhub.library.{IView, IModuleRef}

trait ViewBuilder { this: Builder =>

  /** gets a reference to a view */
  def getViewRef(id: String): Option[IModuleRef] = getReferenceOf(classOf[IModuleRef], id)

  /** builds a reference to a view */
  protected def buildViewReference(view: View) : Option[IModuleRef] = Some(
    IModuleRef(
      view.path.toPath, /* id */
      view.name.toPath, /* name */
      "view" /* kind */
    )
  )

  /** gets a theory */
  def getView(id: String): Option[IView] = getObjectOf(classOf[IView], id)

  /** builds a view representation */
  protected def buildView(view: View): Option[IView] = {
    val ref = getViewRef(view.path.toPath)
      .getOrElse(return buildFailure(view.path.toPath, "getViewRef(view.id)"))

    val domain = getTheoryRef(view.from.toMPath.toPath)
      .getOrElse(return buildFailure(view.path.toPath, "getTheoryRef(view.domain)"))

    val codomain = getTheoryRef(view.to.toMPath.toPath)
      .getOrElse(return buildFailure(view.path.toPath, "getTheoryRef(view.codomain)"))

    Some(IView(
      ref.id, ref.name,
      getStats(ref.id),

      getDeclarations(view),

      domain,
      codomain
    ))
  }
}
