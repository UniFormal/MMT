package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.modules.{Module, ModuleOrLink}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._

import scala.collection.mutable

trait DiagramOperator {
  def beginDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Boolean
  def applyDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Option[Diagram]

  // overwrite if necessary
  def endDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Unit = {}
}

trait BasedOperator {
  val dom: Diagram
  val cod: Diagram

  /**
    * @see [[applyModulePath]]
    */
  protected def applyModuleName(name: LocalName): LocalName

  /**
    * should be fast
    * pre-condition: applyModule on the module described by path returned true previously
    */
  def applyModulePath(mpath: MPath): MPath = {
    mpath.doc ? applyModuleName(LocalName(mpath.name.head)) / mpath.name.tail
  }
}

trait Functor extends BasedOperator {
  /**
    * The functor from [[operatorDomain]] to [[operatorCodomain]], or rather,
    * its base case on individual modules.
    *
    * The actual functor is uniquely established by homomorphic extension to
    * [[OMMOD]], [[OMIDENT]], and [[OMCOMP]] terms in [[applyDomain()]].
    *
    * pre-condition: there is an implicit morphism from [[operatorDomain]] to path.
    *
    * should be pretty conservative, i.e., not throw any match errors
    * if in doubt, just return ''m'' (be the "identity")
    */
  def applyDomainModule(m: MPath): MPath

  /**
    * The functor from [[operatorDomain]] to [[operatorCodomain]] uniquely described
    * by [[applyDomainModule()]].
    * @param t Any module expression over [[operatorDomain]], e.g. a composition of
    *          [[OMMOD]], [[OMIDENT]], and [[OMCOMP]] terms
    */
  final def applyDomain(t: Term): Term = t match {
    case OMMOD(p) => OMMOD(applyDomainModule(p))
    case OMIDENT(t) => OMIDENT(applyDomain(t))
    case OMCOMP(mors) => OMCOMP(mors.map(applyDomain))
  }
}


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
trait LinearOperator extends DiagramOperator {
  type Container = ModuleOrLink

  def ::(first: LinearOperator): ZippingOperator = {
    // seemingly switched order, but this makes `op1 :: op2` really result in List(op1, op2)
    new ZippingOperator(List(first, this))
  }
  /*
  def renamedTo(): LinearOperator = a new instance, overriding applyModuleName, delegating to this
                                    everywhere else

  sketch of a use case:

  // diagram X := FORCE_BIND (HOM ourDiagram) /algebra
  // FORCE_BIND: DiagramOperator -> Namespace -> DiagramOperator
  // FORCE_BIND(op, n) := new DiagramOperator {
  //   def applyModulePath(mpath: MPath) = n ? op.applyModulePath(mpath)
  //   // otherwise, delegate to op
  // }
   */

  protected val seenDeclarations: mutable.Map[ContentPath, mutable.ListBuffer[GlobalName]] = mutable.HashMap()
  def registerSeenDeclaration(d: Declaration): Unit = {
    seenDeclarations.getOrElseUpdate(d.path.module, mutable.ListBuffer()) += d.path
  }

  protected val skippedDeclarations: mutable.Map[ContentPath, mutable.Set[GlobalName]] = mutable.Map()
  def registerSkippedDeclarations(d: Declaration): Unit = {
    skippedDeclarations.getOrElseUpdate(d.path.module, mutable.Set()) += d.path
  }

  // override in child classes if you add more state, should be idempotent and not overwrite any previous state (in case such exists)
  def initState(container: Container): Unit = {
    seenDeclarations.getOrElseUpdate(container.path, mutable.ListBuffer())
    skippedDeclarations.getOrElseUpdate(container.path, mutable.Set())
  }

  // override in child classes if you add more state
  def inheritState(into: ContentPath, from: ContentPath): Unit = {
    seenDeclarations(into) ++= seenDeclarations.getOrElse(from, mutable.ListBuffer())
    skippedDeclarations(into) ++= skippedDeclarations.getOrElse(from, mutable.Set())
  }

  // todo: rename this to a better name
  protected val startedContainers: mutable.ListBuffer[ContentPath] = mutable.ListBuffer()

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
    // TODO(NR@anyone): can we unify cases for plain includes and structures?
    case s @ Include(includeData) => applyIncludeData(includeData, s.asInstanceOf[Structure], container)
    case s: Structure => applyStructure(s, container) // must come after Include case
    case _ =>
      interp.errorCont(InvalidElement(decl, s"Linear operator `$getClass` not applicable on this kind " +
        s"of declaration."))
  }


  /**
    * See [[applyDeclaration()]]; the same notes apply.
    */
  def applyConstant(c: Constant, container: Container)(implicit interp: DiagramInterpreter): Unit

  /**
    * See [[applyDeclaration()]]; the same notes apply.
    *
    * @param structure The [[Structure]] containing [[include]].
    */
  def applyIncludeData(include: IncludeData, structure: Structure, container: Container)(implicit interp: DiagramInterpreter): Unit

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
    initState(inContainer)

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
        s"`${LinearOperator.this.getClass.getSimpleName}` not applicable" + (if (msg.nonEmpty) ": " + msg else "")
      ))

      Nil
    }
  }
}

/**
  * Linearly transforms [[Module]]s to modules in a diagram.
  *
  * It does so by going through modules declaration-by-declaration (using the [[LinearOperator]]
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
trait LinearModuleOperator extends LinearOperator with BasedOperator {
  def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration]

  /**
    * invariant: value v for a key k always has same type as k
    * e.g. modules are mapped to modules, structures to structures
    */
  protected val transformedContainers: mutable.Map[Container, Container] = mutable.Map()

  final def applyModule(inModule: Module)(implicit interp: DiagramInterpreter): Option[Module] = {
    if (dom.hasImplicitFrom(inModule.path)(interp.ctrl.library)) {
      Some(inModule)
    } else if (applyContainer(inModule)) {
      Some(transformedContainers(inModule).asInstanceOf[Module])
    } else {
      None
    }
  }

  final override def beginDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Boolean = {
    if (diag.mt.forall(dom.subsumes(_)(interp.ctrl.library))) {
      true
    } else {
      interp.errorCont(InvalidObject(diag.toTerm, s"Linear operator ${getClass.getSimpleName} not applicable on " +
        s"diagram because operator domain `$dom` does not subsume meta diagram of given diagram."))
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

      Some(Diagram(newModules, Some(cod)))
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
    *     - added `(inContainer, outContainer)` to `transformedContainers`
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

  override def applyConstant(c: Constant, container: Container)(implicit interp: DiagramInterpreter): Unit = {
    val outContainerPath = transformedContainers.get(container).map(_.path)

    translateConstant(c).foreach(decl => {
      // sanity check that all returned declarations have correct homes
      outContainerPath.foreach(outContainerPath => {
        if (decl.path.module != outContainerPath) {
          throw ImplementationError(s"Linear operator ${this.getClass.getSimpleName} translated constant to container " +
            s"${decl.path.module}` which is different than the intended one `${outContainerPath}`.")
        }
      })
      decl.setOrigin(GeneratedFrom(c.path, this))

      interp.add(decl)
      decl match {
        case ce: ContainerElement[_] => interp.endAdd(ce)
        case _ => /* do nothing */
      }
    })
  }
}

trait SystematicRenamer {
  def apply(c: Constant): OMID = OMS(apply(c.path))
  def apply(name: LocalName): LocalName

  /**
    * Only renames symbols already processed in `state`.
    */
  def apply(path: GlobalName): GlobalName
  def applyAlways(path: GlobalName): GlobalName

  // def apply(t: Term): Term, deprecated, use ApplyMorphs on a connector instead
}


/**
  * Linearly transforms things
  * while hiding much of the complexity of their contents.
  *
  * Implementors only need to give a ''applyConstantSimple'' method.
  */
trait SimpleLinearOperator extends LinearOperator {
  /**
    * Maps a constant to a list of assignments in the connecting morphism.
    *
    * @return A list of assignments (simpleName, assignmentTerm), which is used by [[applyConstant]]
    *         to build a [[FinalConstant]] with the right name, empty type container, and a definiens container
    *         containing assignmentTerm.
    */
  protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter): List[Constant]

  final override def applyConstant(c: Constant, container: Container)(implicit interp: DiagramInterpreter): Unit = {
    val curSkippedDeclarations = skippedDeclarations(c.path.module).toSet.asInstanceOf[Set[ContentPath]]

    def expand(t: Term): Term =
      interp.ctrl.library.ExpandDefinitions(t, curSkippedDeclarations)

    val expandedTp = c.tp.map(expand).getOrElse({NotApplicable(c, "no type component"); return})
    val expandedDf = c.df.map(expand)

    val outConstants = applyConstantSimple(c, expandedTp, expandedDf)
    outConstants.foreach(interp.add)
  }
}

trait SimpleLinearFunctor extends LinearFunctor with SimpleLinearOperator
trait SimpleLinearConnector extends LinearConnector with SimpleLinearOperator