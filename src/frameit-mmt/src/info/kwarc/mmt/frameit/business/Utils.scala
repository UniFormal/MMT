package info.kwarc.mmt.frameit.business

import info.kwarc.mmt.api.{LocalName, Path, StructuralElement}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.Module
import info.kwarc.mmt.api.objects.OMMOD
import info.kwarc.mmt.api.symbols.NestedModule

object Utils {
  /**
    * mixture of [[Controller.getAs]] and [[Controller.getO]]
    * @todo integrate into official [[Controller]] API?
    */
  def getAsO[E <: StructuralElement](cls : Class[E], path: Path)(implicit lookup: Lookup): Option[E] = lookup.getO(path) match {
    case Some(e : E@unchecked) if cls.isInstance(e) => Some(e)
    case _ => None
  }


  /**
    * Adds a module to the controller, taking additional care if it is a nested module.
    *
    * In case of a nested module (as determined by ''module.path.name'' comprised of multiple
    * steps), a [[NestedModule]] declaration is added to the containing module (via ctrl).
    *
    * In any case, the module is added (via ctrl).
    *
    * @return List of all paths that need to be deleted from the [[Controller]] if you want to delete
    *         everything this method added
    */
  def addModule(module: Module)(implicit ctrl: Controller): List[Path] = {
    // todo: support nested modules with nesting level >= 3
    module.path.name.steps match {
      case prefix :+ targetModuleName if prefix.nonEmpty =>
        val parentModule = ctrl.getModule(module.path.doc ? LocalName(prefix))
        val nestedModuleDecl = new NestedModule(
          home = parentModule.toTerm,
          name = LocalName(targetModuleName),
          mod = module
        )
        ctrl.add(nestedModuleDecl)
        ctrl.add(module)

        List(module.path, nestedModuleDecl.path)

      case _ =>
        ctrl.add(module)
        ctrl.endAdd(module)
        List(module.path)
    }
  }

  // todo: support nested modules with nesting level >= 3
  // todo: call endAdd recursively on module and all upper modules
  def endAddModule(module: Module)(implicit ctrl: Controller): Unit = {
    module.path.name.steps match {
      case prefix :+ _ if prefix.nonEmpty =>
        val parentModule = ctrl.getModule(module.path.doc ? LocalName(prefix))
        ctrl.endAdd(module)
        ctrl.endAdd(parentModule)

      case _ =>
        ctrl.endAdd(module)
    }
  }
}
