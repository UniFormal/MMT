package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.modules.View
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IView, IViewRef}

trait ViewBuilder { this: Builder =>

  /** gets a reference to a view */
  def getViewRef(id: String): Option[IViewRef] = getReferenceOf(classOf[IViewRef], id)

  /** builds a reference to a view */
  protected def buildViewReference(view: View) : Option[IViewRef] = Some(
    IViewRef(
      view.path.toPath, /* id */
      view.name.toPath /* name */
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

    val presentation: String = getPresentationOf(view)
    val source: Option[String] = getSourceOf(view)

    Some(IView(
      ref.id, ref.name,
      getStats(ref.id),

      presentation,
      source,

      domain,
      codomain
    ))
  }
}
