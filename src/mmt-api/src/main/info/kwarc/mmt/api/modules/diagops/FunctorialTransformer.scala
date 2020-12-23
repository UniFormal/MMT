package info.kwarc.mmt.api.modules.diagops

/**
  * Traits for "anonymous" functorial operators, i.e., operators that are not bound
  * to an MMT symbol.
  * Anonymous operators are useful, e.g., for [[ParametricLinearOperator]]s that create them
  * on-the-fly at runtime based on the parameters they receive.
  * The main trait for linear operators is [[LinearModuleTransformer]].
  *
  * @see Operators.scala for named diagram operators.
  */

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Module}
import info.kwarc.mmt.api.objects.Term

trait ModulePathTransformer {
  protected def applyModuleName(name: LocalName): LocalName

  // diagram X := FORCE_BIND (HOM ourDiagram) /algebra
  // FORCE_BIND: DiagramOperator -> Namespace -> DiagramOperator
  // FORCE_BIND(op, n) := new DiagramOperator {
  //   def applyModulePath(mpath: MPath) = n ? op.applyModulePath(mpath)
  //   // otherwise, delegate to op
  // }

  final def applyModulePath(mpath: MPath): MPath = {
    mpath.doc ? applyModuleName(LocalName(mpath.name.head)) / mpath.name.tail
    /*if (mpath == mpath.mainModule) {
      mpath.doc ? applyModuleName(mpath.name)
    } else {
      val newMPath = applyModulePath(mpath.mainModule)
      newMPath.doc ? LocalName(newMPath.name.steps ::: mpath.name.drop(1))
    }*/
  }
}

/**
  * Transforms modules to modules in a diagram.
  *
  * Base class of whole trait hierarchy of [[LinearModuleTransformer]]s and [[LinearConnectorTransformer]]s.
  *
  * In contrast to the somewhat parallel class hierarchy of [[FunctorialOperator]]s, [[LinearOperator]]s, and
  * [[LinearConnector]]s, this trait hierarchy only concerns itself with the functional behavior of
  * operators/connectors and *not* with parsing input diagram expressions (which are [[Term]]s) and constructing
  * output diagram expressions.
  * Especially, all traits in this hierarchy are *not* associated with any MMT symbol.
  * In contrast, [[FunctorialOperator]] implements [[SyntaxDrivenRule]].
  */
trait FunctorialTransformer extends FunctorialOperatorState with ModulePathTransformer {
  /**
    * Transforms a module.
    *
    * Invariants:
    *
    *  - pre-condition: m is known to `interp.ctrl`, the `Controller`.
    *
    *  - post-condition: if `Some(module)` is returned,
    *
    *    - (a) it has been added to `state.processedModules` and
    *    - (b) it has been passed to [[DiagramInterpreter.addToplevelResult()]].
    *
    *  - it must be efficient to call this function multiple times on the same module; the
    *    computation should only happen once.
    *    We recommend overriding methods have
    *    `state.processedModules.get(m.path).foreach(return _)` as their first line.
    *
    * @return The transformed module if the transformer was applicable on the input module.
    *         In case of errors, these should be signalled via [[DiagramInterpreter.errorCont]].
    *         In case the transformer was inapplicable, [[None]] should be returned.
    */
  def applyModule(m: Module)(implicit state: DiagramState, interp: DiagramInterpreter): Option[Module]

  /**
    * Transforms a diagram module-by-module via [[applyModule()]]
    * @param modulePaths The modules in the diagram.
    * @return The paths of the transformed modules (i.e. modules for which [[applyModule()]] returned
    *         `Some(_)`).
    */
  final def applyDiagram(modulePaths: List[MPath])(implicit interp: DiagramInterpreter): List[MPath] = {
    val modules: Map[MPath, Module] = modulePaths.map(p => (p, interp.ctrl.getModule(p))).toMap
    implicit val state: DiagramState = initDiagramState(modules, interp)

    def sanityCheck(inModulePath: MPath, outModule: Module): Unit = {
      if (!state.processedElements.get(inModulePath).contains(outModule)) {
        throw ImplementationError(s"$getClass applied on input module ${outModule} (via " +
          s"applyModule()) returned module that it did not correctly put into `state.processedElements`.")
      }

      if (!interp.hasToplevelResult(outModule.path)) {
        throw ImplementationError(s"$getClass applied on input module ${inModulePath} (via " +
          s"applyModule()) returned module that it did not register as top-level result")
      }
    }

    modulePaths
      .flatMap(m => applyModule(interp.get(m)) match {
        case Some(outModule) =>
          sanityCheck(m, outModule)
          Some(outModule.path)
        case _ => None
      })
  }
}