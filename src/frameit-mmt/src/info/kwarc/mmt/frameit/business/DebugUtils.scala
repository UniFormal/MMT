package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Module, Theory}
import info.kwarc.mmt.api.presentation.{MMTSyntaxPresenter, RenderingHandler}

object DebugUtils {
  def syntaxPresentRecursively(module: Module)(implicit ctrl: Controller, presenter: MMTSyntaxPresenter, renderingHandler: RenderingHandler): Unit = {
    val includes = module match {
      case t: Theory => t.getAllIncludesWithoutMeta
      case x => x.getAllIncludes
    }

    includes.map(_.from).map(ctrl.getAs(classOf[Module], _)).foreach(syntaxPresentRecursively(_))
    presenter(module)
  }
}
