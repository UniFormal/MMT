package info.kwarc.mmt.api.modules.diagops

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Module, ModuleOrLink}
import info.kwarc.mmt.api.symbols._

/**
  * Linearly transforms [[Module modules]] to modules in a diagram.
  *
  * Even more, we (recursively) linearly transforms [[ModuleOrLink]]s to [[ModuleOrLink]]s.
  * For instance, if a [[Module]] (as passed to `applyModule()`) contains nested modules,
  * we recurse into them. Or if it contains links (e.g. structures), we also recurse into them.
  *
  * To have a handier name, we call [[ModuleOrLink]]s "containers" in function/parameter names and comments.
  *
  * It is left to implementations on which containers exactly they are applicable on, and if so,
  * how the type of output container relates to the type of the input container:
  *
  *  - the subclass [[LinearModuleTransformer]] transforms theories to theories, and links to links
  *  - the subclass [[LinearConnectorTransformer]] transforms theories to views, and links not at all
  *
  *
  * Implementation notes for this class and subclasses:
  *
  *   - if a method takes a `LinearState`, it shouldn't take a DiagramState on top
  *   - methods working on containers should take a `LinearState` as a normal parameter
  *     (not an implicit parameter)
  *   - methods working on declarations should take a `LinearState` as an implicit parameter
  *     By contrast to the point above, here, confusing states is less of a problem.
  *     And also, it makes [[OperatorDSL]] much nicer to work with.
  */
trait LinearTransformer extends FunctorialTransformer with LinearOperatorState {
  type Container = ModuleOrLink

  /**
    * Creates a new output container as a first means to map `inContainer`; called by [[applyContainer()]].
    *
    * If the transformer is applicable on `inContainer`, it creates a new output container
    * and return it. Transformers may choose on their own what kind of output container to create.
    * E.g. [[LinearModuleTransformer]] creates theories for theories, and views for views.
    *
    * You may override this method to do additional action.
    *
    * Post-conditions:
    *
    *   - if `Some(outContainer)` is returned, you must have
    *
    *     - [[DiagramInterpreter.add()]] on `outContainer`
    *     - added `(inContainer, outContainer)` to `state.diagramState.processedElements`
    *   - see also postconditions of [[applyContainer()]]
    *
    * @see [[endContainer()]]
    */
  protected def beginContainer(inContainer: Container, state: LinearState)(implicit interp: DiagramInterpreter): Option[Container]

  /**
    * Finalizes a container.
    *
    * By default, this method uses `DiagramState.processedElements` to lookup to which container
    * `inContainer` has been mapped to, and calls [[DiagramInterpreter.endAdd()]] on the latter.
    *
    * Pre-condition: [[beginContainer()]] must have returned true on `inContainer` before.
    *
    * Post-conditions:
    *
    *   - the container returned by [[beginContainer()]] must have been finalized via
    *     [[DiagramInterpreter.endAdd()]]
    *     (You can access that container via `state.diagramState.processedElements`,
    *      see post-conditions of [[beginContainer()]].)
    *   - see also postconditions of [[applyContainer()]]
    *
    * You may override this method. Be sure to call `super.endContainer()` last in your overridden
    * method (to have [[DiagramInterpreter.endAdd()]] at the very end). Or know what you're doing.
    *
    * @see [[beginContainer()]]
    */
  protected def endContainer(inContainer: Container, state: LinearState)(implicit interp: DiagramInterpreter): Unit = {
    // be careful with accessing in processedElements
    // in applyContainerBegin, e.g. for structures we didn't add anything to processedElements
    state.diagramState.processedElements.get(inContainer.path).foreach {
      case ce: ContainerElement[_] => interp.endAdd(ce)
    }
  }

  /**
    * Transforms a [[Declaration]] from `container`.
    *
    * Pre-condition:
    *
    *   - `decl` must have been added to `state.processedDeclarations`
    *     (This makes the design [[SystematicRenamingUtils]] much easier.)
    *
    * Post-condition:
    *
    *   - The transformation results must be added via [[DiagramInterpreter.add()]] *and*,
    *     results that are containers must also be finalized via [[DiagramInterpreter.endAdd()]].
    *
    * You may override this method to handle more declaration cases specific to your transformer.
    * When the transformer is inapplicable on `decl`, an error should be signalled via
    * [[DiagramInterpreter.errorCont]].
    *
    * @param decl The declaration from `container`, which is to be transformed
    * @param container The input container in which `decl` lives.
    *                  This is *not* the output container into which the transformation result should go!
    * @param state The linear state of the input `container`.
    *
    * @todo should we pass also the `outContainer`?
    */
  protected def applyDeclaration(decl: Declaration, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = decl match {
    case c: Constant => applyConstant(c, container)
    case Include(includeData) => applyIncludeData(includeData, container)
    case s: Structure => applyStructure(s, container) // must come after Include case
    case _ =>
      interp.errorCont(InvalidElement(decl, s"Transformer `$getClass` not applicable on this kind " +
        s"of declaration."))
  }

  /**
    * See [[applyDeclaration()]]; the same notes apply.
    */
  protected def applyConstant(c: Constant, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit

  /**
    * See [[applyDeclaration()]]; the same notes apply.
    */
  protected def applyIncludeData(include: IncludeData, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit

  /**
    * Adds declarations from a [[Structure]] occuring in `container` to the [[LinearState]] of
    * `container`.
    *
    * This ensures implementing transforms can treat theories with declarations
    * and theories with structure (inducing analogous declarations) in the same way.
    *
    * @see [[applyDeclaration()]]; the same notes apply.
    */
  protected def applyStructure(s: Structure, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = {
    applyContainer(s)(state.diagramState, interp).foreach(_ => {
      // todo: should actually register all (also unmapped) declarations from
      //   structure's domain theory, no?

      val inducedDeclarationPaths = s.getDeclarations.map(_.path).flatMap(p => {
        if (p.name.tail.nonEmpty) {
          Some(s.path / p.name.tail)
        } else {
          None
        }
      })
      val inducedDeclarations = inducedDeclarationPaths.map(interp.ctrl.getAs(classOf[Declaration], _))
      inducedDeclarations.foreach(state.registerDeclaration)
    })
  }

  final def applyModule(inModule: Module)(implicit state: DiagramState, interp: DiagramInterpreter): Option[Module] = applyContainer(inModule).map {
    case m: Module => m
    case _ => throw ImplementationError(s"Transformer $getClass transformed module into non-module.")
  }

  /**
    * Transforms a container (i.e. a [[ModuleOrLink]]).
    *
    * Invariants:
    *
    *  - pre-condition: `inContainer` is known to `interp.ctrl`, the `Controller`.
    *
    *  - post-conditions: if `Some(outContainer)` is returned, you must have
    *
    *    - (a) added `(inContainer, outContainer)` `state.processedElements`
    *    - (b) called [[DiagramInterpreter.add()]] and [[DiagramInterpreter.endAdd()]]
    *          on `outContainer`
    *    - (c) if `inContainer` fulfills [[DiagramInterpreter.hasToplevelResult()]],
    *          `outContainer` must be a [[Module]] and you must have called
    *          [[DiagramInterpreter.addToplevelResult()]] on it.
    *
    *  - it must be efficient to call this function multiple times on the same container; the
    *    computation should only happen once.
    *
    * @return The transformed container if the transformer was applicable on the input container.
    *         In case of errors, these should be signalled via [[DiagramInterpreter.errorCont]].
    *         In case the transformer was inapplicable, [[None]] should be returned.
    *
    *
    * @return true if element was processed (or already had been processed), false otherwise.
    */
  final protected def applyContainer(inContainer: Container)(implicit state: DiagramState, interp: DiagramInterpreter): Option[Container] = {
    state.processedElements.get(inContainer.path).foreach(c => return Some(c.asInstanceOf[Container]))

    val inLinearState = state.initAndRegisterNewLinearState(inContainer)

    beginContainer(inContainer, inLinearState).map(outContainer => {
      inLinearState.outContainer = outContainer
      inContainer.getDeclarations.foreach(decl => {
        inLinearState.registerDeclaration(decl)
        applyDeclaration(decl, inContainer)(inLinearState, interp)
      })

      endContainer(inContainer, inLinearState)

      outContainer
    })
  }
}

trait RelativeBaseTransformer {
  val operatorDomain: MPath
  val operatorCodomain: MPath
}
