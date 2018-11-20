package info.kwarc.mmt.mathhub.library.Context.Builders

import info.kwarc.mmt.api.StructuralElement
import info.kwarc.mmt.api.presentation.{HTMLExporter, StringBuilder}
import info.kwarc.mmt.mathhub.library.Context.MathHubAPIContext
import info.kwarc.mmt.mathhub.library.{IModule, IModuleRef}

trait ModuleWrap { this: MathHubAPIContext =>
  /** gets a reference to a module */
  def getModuleRef(id: String): Option[IModuleRef] = {
    getViewRef(id).map(Some(_)).getOrElse(getTheoryRef(id)) // View | Theory
  }


  /** gets a module */
  def getModule(id: String): Option[IModule] = {
    // View | Theory
    getView(id).map(Some(_))
      .getOrElse(getTheory(id))
  }

  /** presents any element for the MathHub API */
  @deprecated("no longer needed")
  protected def getPresentationOf(se: StructuralElement): String = {
    val exporter = controller.extman.get(classOf[HTMLExporter]).head // TODO: Build a custom presenter
    val sb = new StringBuilder
    exporter(se, standalone = false)(sb)
    sb.get
  }
}
