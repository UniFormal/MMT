package info.kwarc.mmt.api.modules.diagrams

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
import info.kwarc.mmt.api.modules.Module

trait ModulePathTransformer {
  protected def applyModuleName(name: LocalName): LocalName

  // diagram X := FORCE_BIND (HOM ourDiagram) /algebra
  // FORCE_BIND: DiagramOperator -> Namespace -> DiagramOperator
  // FORCE_BIND(op, n) := new DiagramOperator {
  //   def applyModulePath(mpath: MPath) = n ? op.applyModulePath(mpath)
  //   // otherwise, delegate to op
  // }

  /**
    * For [[LinearModuleTransformer]]s: only call this function IF YOU KNOW that the operator transforms mpath
    *   e.g. if mpath stems from operator domain and is left untouched, DO NOT CALL THIS FUNCTION
    */
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

trait DiagramTransformer extends DiagramTransformerState {
  def beginDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Boolean
  def applyDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Option[Diagram]

  // overwrite if necessary
  def endDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Unit = {}
}

/**
  * Transforms modules to modules in a diagram.
  */
trait ModuleTransformer extends DiagramTransformer with ModuleTransformerState with ModulePathTransformer {
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
}