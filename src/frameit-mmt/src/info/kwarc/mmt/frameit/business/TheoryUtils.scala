package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{Module, Theory}
import info.kwarc.mmt.api.presentation.{MMTSyntaxPresenter, RenderingHandler}
import info.kwarc.mmt.api.symbols.{FinalConstant, PlainInclude}

object TheoryUtils {
  def getAllFinalConstantsRecursively(module: Module)(implicit ctrl: Controller): List[FinalConstant] = {
    module.getDeclarations.collect {
      case c: FinalConstant => List(c)
      case PlainInclude(from, to) if to == module.path => getAllFinalConstantsRecursively(ctrl.getAs(classOf[Module], from))
    }.flatten
  }
}
