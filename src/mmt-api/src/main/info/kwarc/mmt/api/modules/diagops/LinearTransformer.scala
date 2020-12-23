package info.kwarc.mmt.api.modules.diagops

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Module, ModuleOrLink, Theory, View}
import info.kwarc.mmt.api.objects.{OMCOMP, OMIDENT, OMMOD, Term}
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
    * @see [[endContainer()]]
    */
  protected def beginContainer(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Option[Container]

  /**
    * Finalizes a container.
    *
    * By default, this method uses `DiagramState.processedElements` to lookup to which container
    * `inContainer` has been mapped to, and calls [[DiagramInterpreter.endAdd()]] on the latter.
    *
    * Pre-condition: [[beginContainer()]] must have returned true on `inContainer` before.
    *
    * You may override this method. Be sure to call `super.endContainer()` last in your overridden
    * method; or know what you're doing.
    *
    * @see [[beginContainer()]]
    */
  protected def endContainer(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Unit = {
    // be careful with accessing in processedElements
    // in applyContainerBegin, e.g. for structures we didn't add anything to processedElements
    diagState.processedElements.get(inContainer.path).foreach {
      case ce: ContainerElement[_] => interp.endAdd(ce)
    }
  }

  /**
    * Post-condition: all added declaration added to ''interp''
    * (i.e. called [[DiagramInterpreter.add()]] *and* [[DiagramInterpreter.endAdd()]]).
    */
  protected def applyDeclaration(container: Container, containerState: LinearState, decl: Declaration)(implicit interp: DiagramInterpreter, state: DiagramState): Unit

  /**
    * Post-condition: all added declaration added to ''interp''
    * (i.e. called [[DiagramInterpreter.add()]] *and* [[DiagramInterpreter.endAdd()]]).
    */
  protected def applyIncludeData(container: Container, containerState: LinearState, include: IncludeData)(implicit interp: DiagramInterpreter, state: DiagramState): Unit

  final def applyModule(inModule: Module)(implicit interp: DiagramInterpreter, state: DiagramState): Option[Module] = applyContainer(inModule).map {
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
    *  - post-condition: if `Some(outContainer)` is returned,
    *
    *    - (a) `outContainer` has been added to `state.processedElements` and
    *    - (b) `outContainer` has been passed to [[DiagramInterpreter.add()]].
    *    - (c) if `inContainer` fulfills [[DiagramInterpreter.hasToplevelResult()]]
    *          `outContainer` must be a [[Module]] and be added via
    *          [[DiagramInterpreter.addToplevelResult()]].
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
  final protected def applyContainer(inContainer: Container)(implicit interp: DiagramInterpreter, state: DiagramState): Option[Container] = {
    state.processedElements.get(inContainer.path).foreach(c => return Some(c.asInstanceOf[Container]))

    val inLinearState = state.initAndRegisterNewLinearState(inContainer)

    beginContainer(inContainer, inLinearState).map(outContainer => {
      inContainer.getDeclarations.foreach(decl => {
        inLinearState.registerDeclaration(decl)
        decl match {
          case Include(includeData) => applyIncludeData(inContainer, inLinearState, includeData)
          case s: Structure => applyStructure(inContainer, inLinearState, s)
          case decl: Declaration => applyDeclaration(inContainer, inLinearState, decl)
        }
      })

      endContainer(inContainer, inLinearState)

      outContainer
    })
  }

  /**
    * Adds declarations from a [[Structure]] occuring in `container` to the [[LinearState]] of
    * `container`.
    *
    * This ensures implementing transforms can treat theories with declarations
    * and theories with structure (inducing analogous declarations) in the same way.
    */
  protected def applyStructure(container: Container, containerState: LinearState, s: Structure)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = {
    applyContainer(s).foreach(_ => {
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
      inducedDeclarations.foreach(containerState.registerDeclaration)
    })
  }

  // DSL
  object NotApplicable {
    def apply[T](c: Declaration, msg: String = "")(implicit interp: DiagramInterpreter, state: LinearState): List[T] = {
      state.registerSkippedDeclaration(c)
      interp.errorCont(InvalidElement(
        c,
        s"${LinearTransformer.this.getClass.getSimpleName} not applicable" +
          (if (msg.nonEmpty) ": " + msg else "")
      ))

      Nil
    }

    object Module {
      def apply(m: Module, msg: String)(implicit interp: DiagramInterpreter): Unit = {
        interp.errorCont(InvalidElement(
          m,
          s"${LinearTransformer.this.getClass.getSimpleName} not applicable" +
            (if (msg.nonEmpty) ": " + msg else "")
        ))
      }
    }
  }
}

trait RelativeBaseTransformer {
  val operatorDomain: MPath
  val operatorCodomain: MPath
}
