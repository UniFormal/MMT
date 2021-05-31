package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.{Module, ModuleOrLink}
import info.kwarc.mmt.api.symbols._

import scala.collection.mutable

/**
  * Performs an action on [[ModuleOrLink]]s declaration-by-declaration ("linearly").
  *
  * It offers methods `beginContainer`, `endContainer`, `applyDeclaration`, `applyConstant`, among others.
  * What action is performed in those methods, is left up to the implementation.
  *
  * To have a handier name, we call [[ModuleOrLink]]s "containers" in this trait.
  *
  * @see [[ModuleTransformer]] for a subtrait that in `beginContainer` creates
  *      new module for each passed module.
  * @see [[LinearFunctor]] for a subtrait that in `beginContainer` for
  *      [[info.kwarc.mmt.api.modules.Theory theories]] creates new theories,
  *      for [[info.kwarc.mmt.api.modules.View views]] createss new views,
  *      and maps declarations in both declaration-by-declaration.
  * @see [[LinearConnector]] for a subtrait that in `beginContainer` for
  *      theories creates views, for views does no action at all,
  *      and maps declarations in theories to view assignments for the created view.
  */
trait LinearTransformer extends DiagramTransformer {
  type Container = ModuleOrLink

  protected val seenDeclarations: mutable.Map[ContentPath, mutable.ListBuffer[GlobalName]] = mutable.HashMap()
  def registerSeenDeclaration(d: Declaration): Unit = {
    seenDeclarations.getOrElseUpdate(d.path.module, mutable.ListBuffer()) += d.path
  }

  protected val skippedDeclarations: mutable.Map[ContentPath, mutable.Set[GlobalName]] = mutable.Map()
  def registerSkippedDeclarations(d: Declaration): Unit = {
    skippedDeclarations.getOrElseUpdate(d.path.module, mutable.Set()) += d.path
  }

  // override in child classes if you add more state
  def inheritState(into: MPath, from: MPath): Unit = {
    seenDeclarations.getOrElseUpdate(into, mutable.ListBuffer()) ++= seenDeclarations.getOrElse(from, mutable.ListBuffer())
    skippedDeclarations.getOrElseUpdate(into, mutable.Set()) ++= skippedDeclarations.getOrElse(from, mutable.Set())
  }

  // todo: rename this to a better name
  protected val startedContainers: mutable.ListBuffer[Path] = mutable.ListBuffer()

  /**
    * Hook before the declarations of a container are gone linearly through; called by
    * [[applyContainer()]].
    *
    * @return `true` if the transformer is applicable on the container, `false` otherwise.
    *         If `false` is returned, the declarations within the container aren't processed.
    *
    * @see [[endContainer()]]
    */
  def beginContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Boolean

  /**
    * Hook after all declarations of a container have been gone through; called by
    * [[applyContainer()]].
    *
    * Only called if [[beginContainer()]] returned `true` on `inContainer`.
    *
    * @see [[beginContainer()]]
    */
  def endContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Unit

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
  def applyDeclaration(decl: Declaration, container: Container)(implicit interp: DiagramInterpreter): Unit = decl match {
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
  def applyConstant(c: Constant, container: Container)(implicit interp: DiagramInterpreter): Unit

  /**
    * See [[applyDeclaration()]]; the same notes apply.
    */
  def applyIncludeData(include: IncludeData, container: Container)(implicit interp: DiagramInterpreter): Unit

  /**
    * Adds the elaborated constants of a [[Structure]] (which occurs in `container`) to the [[LinearState]] of
    * `container`.
    *
    * This ensures implementing transformers can treat theories with declarations
    * and theories with structure (inducing analogous declarations) in the same way.
    *
    * @see [[applyDeclaration()]]; the same notes apply.
    */
  def applyStructure(s: Structure, container: Container)(implicit interp: DiagramInterpreter): Unit = {
    if (applyContainer(s)) {
      // todo: should actually register all (also unmapped) declarations from
      //   structure's domain theory, no?

      // As an example, assume `container` is a theory `T`, `s` is a structure `s`,
      // and that `s` maps a constant `c := E` from a theory `R`.
      // Then the elaborated constant for that assignment has the path
      //   `T.path ? (s/[?R]/c)`.
      //
      // (By contrast, the path `T.path / s ? ([?R]/c)` would refer to the assignment within the structure,
      //  not the elaborated constant.)
      val elaboratedConstantPaths = s.getDeclarations.map(_.path.name).map(assignmentName =>
        s.path.module ? (s.path.name / assignmentName)
      )

      elaboratedConstantPaths
        .map(interp.ctrl.getAs(classOf[Declaration], _))
        .foreach(registerSeenDeclaration)
    }
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
  def applyContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Boolean = {
    if (startedContainers.contains(inContainer.path)) {
      return true
    }

    startedContainers += inContainer.path

    if (beginContainer(inContainer)) {
      inContainer.getDeclarations.foreach(decl => {
        registerSeenDeclaration(decl)
        applyDeclaration(decl, inContainer)
      })

      endContainer(inContainer)

      true
    } else {
      false
    }
  }

  object NotApplicable {
    def apply[T](decl: Declaration, msg: String = "")(implicit interp: DiagramInterpreter): List[T] = {
      registerSkippedDeclarations(decl)
      interp.errorCont(InvalidElement(
        decl,
        s"`${LinearTransformer.this.getClass.getSimpleName}` not applicable" + (if (msg.nonEmpty) ": " + msg else "")
      ))

      Nil
    }
  }
}

/**
  * Linearly transforms [[Module]]s to modules in a diagram.
  *
  * It does so by going through modules declaration-by-declaration (using the [[LinearTransformer]]
  * trait) and recursing into nested modules or structures when needed.
  *
  * It is left to implementations on which modules exactly they are applicable on, and if so,
  * how the type of output container relates to the type of the input container:
  *
  *  - the subtrait [[LinearFunctor]] transforms theories to theories, and views to views
  *  - the subtrait [[LinearConnector]] transforms theories to views, and views not at all
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
trait LinearModuleTransformer extends LinearTransformer with BaseTransformer {
  /**
    * invariant: value v for a key k always has same type as k
    * e.g. modules are mapped to modules, structures to structures
    */
  protected val transformedContainers: mutable.Map[Container, Container] = mutable.Map()

  final def applyModule(inModule: Module)(implicit interp: DiagramInterpreter): Option[Module] = {
    if (operatorDomain.hasImplicitFrom(inModule.path)(interp.ctrl.library)) {
      Some(inModule)
    } else if (applyContainer(inModule)) {
      Some(transformedContainers(inModule).asInstanceOf[Module])
    } else {
      None
    }
  }

  final override def beginDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Boolean = {
    if (diag.mt.exists(operatorDomain.subsumes(_)(interp.ctrl.library))) {
      true
    } else {
      interp.errorCont(InvalidObject(diag.toTerm, s"Transformer ${getClass.getSimpleName} not applicable on " +
        s"diagram because operator domain `$operatorDomain` does not subsume meta diagram of given diagram."))
      false
    }
  }

  final override def applyDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Option[Diagram] = {
    if (beginDiagram(diag)) {
      val newModules = diag.modules
        .map(interp.ctrl.getModule)
        .flatMap(applyModule)
        .map(_.path)

      endDiagram(diag)

      Some(Diagram(newModules, Some(operatorCodomain)))
    } else {
      None
    }
  }

  // We restate the declaration of beginContainer here even though it is already included in
  // our parent trait LinearTransformer for the sake of adding more documentation, pre-, and
  // post-conditions.
  /**
    * Creates a new output container as a first means to map `inContainer`; called by [[applyContainer()]].
    *
    * If the transformer is applicable on `inContainer`, it creates a new output container,
    * performs the post-conditions below, and returns true.
    * Transformers may choose on their own what kind of output container to create.
    * E.g. [[LinearFunctor]] creates theories for theories, and views for views,
    * and [[LinearConnector]] creates views for theories and is inapplicable on view.s
    *
    * Post-conditions:
    *
    *   - if `true` is returned, you must have
    *
    *     - [[DiagramInterpreter.add()]] on `outContainer`
    *     - added `(inContainer, outContainer)` to `state.diagramState.processedElements`
    *     - set `state.outContainer = outContainer`
    *   - see also postconditions of [[applyContainer()]]
    *
    * @see [[endContainer()]]
    */
  def beginContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Boolean

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
  override def endContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Unit = {
    transformedContainers.get(inContainer).foreach(interp.endAdd)
  }
}

/**
  * Base trait of transformers that have some notion of an `operatorDomain` and `operatorCodomain`.
  *
  * As a first approximation, transformers could have a mere [[MPath]] as their domain and codomain.
  * But this trait directly generalizes this to whole [[Diagram]]s as domain and codomain.
  *
  * So far it is only used in [[LinearFunctor]] and [[LinearConnector]].
  * There, diagrams are transformed module-by-module and declaration-by-declaration, and upon
  * includes the following happens: for an `include T = t`, where `T` stems from `operatorDomain`,
  * we transform it into an `include applyMetaModule(T) = applyMetaModule(t)` (very roughly).
  */
trait RelativeBaseTransformer {
  def operatorDomain: Diagram
  def operatorCodomain: Diagram
/*
  /**
    * Translates occurrences (e.g. includes) of theories and views from [[operatorDomain]]
    * to something over [[operatorCodomain]].
    *
    *  - For [[LinearFunctorialTransformer]], this is a functor.
    *  - For [[LinearConnectorTransformer]] (between functors [[LinearConnectorTransformer.in]] and
    *    [[LinearConnectorTransformer.out]]), this is a natural transformation between `in` and `out`.
    *
    *
    * @todo ask Florian whether this method is good, ideally we should only be forced to give the functor
    *       on "individual morphisms" and have it homomorphically extended to OMCOMPs and so on; how to do
    *       this?
    *
    * @todo this is very hacky, please completely rethink conceptually
    */
  def applyMetaModule(t: Term)(implicit lookup: Lookup): Term = (operatorDomain.modules, operatorCodomain.modules) match {
    case (List(domTheory), List(codTheory)) => t match {
      case OMMOD(`domTheory`) => OMMOD(codTheory)
      case OMIDENT(OMMOD(`domTheory`)) => OMIDENT(OMMOD(codTheory))
      case OMMOD(m) if lookup.hasImplicit(m, domTheory) =>
        if (lookup.hasImplicit(m, codTheory)) {
          OMMOD(m) // a very general theory, e.g. LF
        } else {
          OMMOD(codTheory)
        }

      // todo: this default case probably does more harm than good
      //       above some cases are probably missing, too
      case t => t
    }

    case _ => throw ImplementationError(s"Implementor ${getClass.getSimpleName} of RelativeBaseTransformer " +
      "did not override applyMetaModule, but yet had operatorDomain and/or operatorCodomain that were more " +
      "than just singletons (for which the default implementation would have provided a fallback).")
  }*/
}
