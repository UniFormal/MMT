package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.{ContainerElement, ContentPath, GeneratedFrom, GlobalName, ImplementationError, InvalidElement, InvalidObject, LocalName, MPath}
import info.kwarc.mmt.api.modules.{Module, ModuleOrLink}
import info.kwarc.mmt.api.objects.{OMCOMP, OMID, OMIDENT, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, Include, IncludeData, Structure}

import scala.collection.mutable

/**
  * Anonymous [[UnaryOperator]]s that map [[Module]]s to [[Module]]s.
  *
  * @see [[LinearFunctor]] and [[LinearConnector]] for the most common special cases.
  */

/**
  * A diagram operator that maps modules (over some domain diagram `dom`) to modules (over some codomain diagram `cod`).
  *
  * @see [[Diagram]] for what "diagrams over other diagrams" means
  * @see [[LinearModuleOperator]] for a [[ModuleOperator]] that works declaration-by-declaration on modules.
  */
trait ModuleOperator extends UnaryOperator {
  val dom: Diagram
  val cod: Diagram

  /**
    * Maps modules contained in `dom` to modules contained in `cod`.
    *
    * @see [[applyDomain]] for the functor's action on possibly complex module expressions.
    *
    * pre-condition: there is an implicit morphism from [[dom]] to `m`. (todo: what does this mean for views?)
    *
    * should be pretty conservative, i.e., not throw any match errors
    * if in doubt, just return ''m'' (be the "identity")
    */
  def applyDomainModule(m: MPath): MPath

  /**
    * Maps module expressions over `dom` to module expressions over `cod`.
    * The atomic case for [[MPath module paths]] is given by [[applyDomainModule]].
    *
    * (E.g., for [[Functor]]s the map on complex cases described by this method is determined by functoriality
    *  from the atomic case alone.)
    *
    * @param t Any module expression over [[dom]], e.g., a composition of
    *          [[OMMOD]], [[OMIDENT]], and [[OMCOMP]] terms.
    */
  def applyDomain(t: Term): Term

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

  def applyModuleExpression(m: Term): Term
}

/**
  * A functor on MMT theories and views.
  *
  * Concretely, it is a functor between the category of MMT theories and views over the diagram [[ModuleOperator.dom]]
  * to the analogous category of theories and views over [[ModuleOperator.cod]].
  *
  * The functor's action on [[MPath module paths]] is given by `applyDomainModule()` and its action on module
  * expressions, i.e., terms containing [[OMMOD]], [[OMIDENT]], [[OMCOMP]], etc., is given by `applyDomain()`.
  *
  * @see [[LinearFunctor]] for a functor that works declaration-by-declaration on theories and views.
  */
trait Functor extends ModuleOperator {
  /** Extends a function on [[MPath module paths]] to module expressions in a functorial way. */
  private def functorialify(f: MPath => MPath): Term => Term = {
    def tr(t: Term): Term = t match {
      case OMMOD(m) => OMMOD(f(m))
      case OMIDENT(m) => OMIDENT(tr(m))
      case OMCOMP(mors) => OMCOMP(mors.map(tr))
    }
    tr
  }

  /**
    * Maps module expressions by the functor.
    * The atomic case for [[MPath module paths]] is given by [[applyDomainModule]] and the complex cases are
    * determined by functoriality.
    *
    * @param t Any module expression over [[dom]], e.g., a composition of
    *          [[OMMOD]], [[OMIDENT]], and [[OMCOMP]] terms.
    */
  final override def applyDomain(t: Term): Term = functorialify(applyDomainModule)(t)
  final override def applyModuleExpression(m: Term): Term = functorialify(applyModulePath)(m)
}


/**
  * A diagram operator that acts linearly on diagrams:
  * module-by-module (in dependency order) and declaration-by-declaration.
  * No assumptions are made on what action is done (if any at all) on traversing through modules or declarations.
  *
  * More precisely, we even go through [[ModuleOrLink]]s declaration-by-declaration.
  * (The applicability on [[info.kwarc.mmt.api.modules.Link Link]]s is necessary, e.g., when recursing into
  *  [[Structure]]s contained in [[info.kwarc.mmt.api.modules.Theory theories]].)
  * For brevity, we refer to [[ModuleOrLink]]s simply as *containers* in this trait and all subtraits.
  *
  * Implementors in particular need to implement methods `beginContainer`, `endContainer` (for pre-/postprocessing),
  * and `applyDeclaration`, `applyConstant`.
  *
  * @see [[LinearModuleOperator]] for a subtrait that upon every input module creates (some kind of) output module
  * @see [[LinearFunctor]] for a [[LinearModuleOperator]] that maps theories to theories and views to views
  * @see [[LinearConnector]] for [[LinearModuleOperator]] that maps theories to views and views not at all
  * @see [[ZippingOperator]] for a subtrait that bundles multiple [[LinearOperator]]s in parallel
  */
trait LinearOperator extends UnaryOperator {
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

  /**
    * Hook before the declarations of a container are gone linearly through; called by
    * [[applyContainer]].
    *
    * @return `true` if the transformer is applicable on the container, `false` otherwise.
    *         If `false` is returned, the declarations within the container aren't processed.
    *
    * @see [[endContainer]]
    */
  def beginContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Boolean

  /**
    * Hook after all declarations of a container have been gone through; called by
    * [[applyContainer]].
    *
    * Only called if [[beginContainer]] returned `true` on `inContainer`.
    *
    * @see [[beginContainer]]
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
    *   - The transformation results must be added via [[DiagramInterpreter.add]] *and*,
    *     results that are containers must also be finalized via [[DiagramInterpreter.endAdd]].
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
    * See [[applyDeclaration]]; the same notes apply.
    */
  def applyConstant(c: Constant, container: Container)(implicit interp: DiagramInterpreter): Unit

  /**
    * See [[applyDeclaration]]; the same notes apply.
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
    * @see [[applyDeclaration]]; the same notes apply.
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
    * A cache for [[applyContainer]] to save which containers the operator was applicable on and has already been
    * applied to.
    *
    * @see [[applyContainer]]
    */
  protected val applicableContainers: mutable.Map[ContentPath, Boolean] = mutable.Map()

  /**
    * Acts on a container (i.e. a [[ModuleOrLink]]).
    *
    * This method must be an efficient idempotence: whenever called a second time for the same input container, it must
    * (i) return the same output container as before and (ii) do so efficiently (e.g., by caching).
    *
    * Pre-condition: `inContainer` is known to `interp.ctrl`, the `Controller`.
    * Post-conditions: if `Some(outContainer)` is returned, you must have
    *    - called [[DiagramInterpreter.add]] and [[DiagramInterpreter.endAdd]]
    *      on `outContainer`
    *    - if `inContainer` fulfills [[DiagramInterpreter.hasToplevelResult]],
    *      `outContainer` must be a [[Module]] and you must have called
    *      [[DiagramInterpreter.addToplevelResult]] on it.
    *
    * @return True if the operator was applicable on the container and processed it (or had processed it already in
    *         the past), false if not.
    *         Possible errors should be logged via [[DiagramInterpreter.errorCont]].
    */
  def applyContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Boolean = {
    applicableContainers.get(inContainer.path).foreach(return _)

    initState(inContainer)

    if (beginContainer(inContainer)) {
      inContainer.getDeclarations.foreach(decl => {
        registerSeenDeclaration(decl)
        applyDeclaration(decl, inContainer)
      })

      endContainer(inContainer)

      applicableContainers += inContainer.path -> true
      true
    } else {
      applicableContainers += inContainer.path -> false
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
  * A diagram operator that maps linearly on diagrams: modules to modules and declarations to declarations.
  *
  * It is left to implementors to choose the precise kind of output module for input modules:
  *
  * - see [[LinearFunctor]] for a subtrait that maps theories to theories and views to views
  * - see [[LinearConnector]] for a subtrait that maps theories to views and views not at all
  *
  * Implementors must annotate all output modules and output declarations with metadata using
  * [[info.kwarc.mmt.api.StructuralElement.setOrigin]] and [[GeneratedFrom `GeneratedFrom(..., this)`]].
  */
trait LinearModuleOperator extends LinearOperator with ModuleOperator {
  /**
    * Maps a constant to a list of output [[Declaration]]s.
    *
    * Implementations are advised to be as structure-preserving of `c`'s data as possible:
    * try translating the type, definiens, alias, role, notation, and the metadata, and whatever else the [[Constant]]
    * interface offers.
    *
    * @return A list of output declarations, possibly Nil. If the operator is inapplicable on `c`,
    *         `Nil` is returned and an [[InvalidElement]] reported to
    *         [[DiagramInterpreter.errorCont `interp.errorCont`]]. You can use the [[NotApplicable]] convenience
    *         object for that.
    */
  def translateConstant(c: Constant)(implicit interp: DiagramInterpreter): List[Declaration]

  /**
    * Saves the containers that have been mapped so far.
    *
    * Invariant: [[Module]]s are only mapped to other modules (possibly of different kind, e.g.,
    * theories to views) and [[Structure]]s are only mapped to [[Structure]]s.
    */
  protected val mappedContainers: mutable.Map[Container, Container] = mutable.Map()

  /**
    * Linearly maps a module.
    *
    * This method must be an efficient idempotence: whenever called a second time for the same input module, it must
    * (i) return the same output module as before and (ii) do so efficiently (e.g., by caching).
    *
    * @return In case of success, some output module is returned. In case of failure (e.g., because the operator
    *         was partial on that specific module), errors are logged to [[DiagramInterpreter.errorCont]] and None is
    *         returned.
    */
  final def applyModule(module: Module)(implicit interp: DiagramInterpreter): Option[Module] = {
    // force elaboration on input module (among other things, this makes sure the implicit graph
    // related to inModule gets constructed)
    interp.ctrl.simplifier(module)

    if (dom.hasImplicitFrom(module.path)(interp.ctrl.library)) {
      interp.ctrl.getAsO(classOf[Module], applyDomainModule(module.path))
    } else if (applyContainer(module)) {
      Some(mappedContainers(module).asInstanceOf[Module])
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
  // TODO (Florian's intuition): applyDiagram creates module stubs, translateConstant fills those modules (and returns nothing)
  final override def applyDiagram(diag: Diagram)(implicit interp: DiagramInterpreter): Option[Diagram] = {
    if (beginDiagram(diag)) {
      // We naively call `applyModule` on all modules.
      // This is fine even when implementors recurse upon includes in `applyModule` (and transitively called methods,
      // e.g. `applyContainer`) because `applyModule` is an efficient idempotence by its method contract.
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
    * Creates a new output container as a first means to map `container`; called by [[applyContainer]].
    *
    * If the operator is applicable on `container`, it creates a new output container,
    * performs the post-conditions below, and returns true.
    * Operators may choose on their own what kind of output container to create: e.g. [[LinearFunctor]]s create
    * theories for theories and views for views, while [[LinearConnector]]s create views for theories and are
    * inapplicable on views.
    *
    * Post-conditions:
    *
    *   - if `true` is returned, you must have
    *     - added an entry to `(inContainer, outContainer)` (for some container `outContainer` of your choice)
    *       to `mappedContainers`
    *     - called [[DiagramInterpreter.add]] on `outContainer`
    *   - see also postconditions of [[applyContainer]]
    *
    * @see [[endContainer]]
    */
  def beginContainer(container: Container)(implicit interp: DiagramInterpreter): Boolean

  /**
    * Finalizes a container.
    *
    * By default, this method uses [[LinearModuleOperator.mappedContainers]] to look up which container `inContainer`
    * has been mapped to and calls [[DiagramInterpreter.endAdd]] on it.
    *
    * Pre-condition: [[beginContainer]] must have returned true on `inContainer` before.
    *
    * Post-conditions:
    *
    *   - the container returned by [[beginContainer]] must have been finalized via
    *     [[DiagramInterpreter.endAdd]]
    *   - see also postconditions of [[applyContainer]]
    *
    * You may override this method. Be sure to call `super.endContainer()` last in your overridden
    * method (to have [[DiagramInterpreter.endAdd]] at the very end). Or know what you're doing.
    *
    * @see [[beginContainer]]
    */
  override def endContainer(inContainer: Container)(implicit interp: DiagramInterpreter): Unit = {
    mappedContainers.get(inContainer).foreach(interp.endAdd)
  }

  override def applyConstant(c: Constant, container: Container)(implicit interp: DiagramInterpreter): Unit = {
    val outContainerPath = mappedContainers(container).path

    translateConstant(c).foreach(decl => {
      // sanity check that all returned declarations have correct homes
      if (decl.path.module != outContainerPath) {
        throw ImplementationError(s"Linear operator ${this.getClass.getSimpleName} translated constant to container " +
          s"${decl.path.module}` which is different than the intended one `$outContainerPath`.")
      }
      decl.setOrigin(GeneratedFrom(c.path, this, None))

      interp.add(decl)
      // call endAdd where applicable (esp. important when decl is a structure or include)
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

  // when connectors are total, we can equivalently use ApplyMorphs instead
  // but when connectors are partial (e.g., for CompTransOperator in general), this gets much more tricky
  // hence this function exists
  def apply(t: Term): Term
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
