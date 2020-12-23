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

/**
  * Linearly transforms theories to theories, and views to views.
  *
  * Implementors must implement
  *
  *  - `applyDeclaration()`
  *
  * and may override
  *
  *  - `beginTheory()`
  *  - `beginView()`
  *  - `beginStructure()`
  */
trait LinearModuleTransformer extends LinearTransformer with RelativeBaseTransformer {

  /**
    * Creates a new output theory that serves to contain the to-be-mapped declarations; called by
    * [[beginModule()]].
    *
    * You may override this method to do additional action.
    *
    * @example Some transformers need to add includes at the beginning of every theory. They should
    *          override the method as follows:
    *          {{{
    *          override protected def beginTheory(...): Option[Theory] = {
    *            super.beginTheory(...).map(thy => {
    *              // add inclusion to thy (via interp.ctrl)

    *              thy
    *            })
    *          }
    *          }}}
    */
  protected def beginTheory(thy: Theory, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Option[Theory] = {
    val outPath = applyModulePath(thy.path)
    val newMeta = thy.meta.map {
      case mt if interp.ctrl.globalLookup.hasImplicit(mt, operatorDomain) =>
        operatorCodomain
      case mt =>
        if (applyModule(interp.ctrl.getModule(mt)).isEmpty) {
          interp.errorCont(InvalidElement(thy, s"Theory had meta theory `$mt` for which there " +
            s"was no implicit morphism into `$operatorDomain`. Recursing into meta theory as usual " +
            s"failed, too; reasons are probably logged above."))
          return None
        }
        applyModulePath(mt)
    }

    Some(Theory.empty(outPath.doc, outPath.name, newMeta))
  }

  /**
    * Creates a new output view that serves to contain the to-be-mapped assignments; called by
    * [[beginModule()]].
    *
    * You may override this method to do additional action.
    *
    * @example Some transformers need to add includes at the beginning of every view. They should
    *          override the method as follows:
    *          {{{
    *          override protected def beginView(...): Option[View] = {
    *            super.beginView(...).map(view => {
    *              // add inclusion to view (via interp.ctrl)

    *              view
    *            })
    *          }
    *          }}}
    */
  protected def beginView(view: View, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Option[View] = {
    if (applyModule(interp.ctrl.getModule(view.from.toMPath)).isEmpty) {
      return None
    }
    if (applyModule(interp.ctrl.getModule(view.to.toMPath)).isEmpty) {
      return None
    }

    containerState.inherit(diagState.getLinearState(view.to.toMPath))

    val outPath = applyModulePath(view.path)
    Some(View(
      outPath.doc, outPath.name,
      OMMOD(applyModulePath(view.from.toMPath)), OMMOD(applyModulePath(view.to.toMPath)),
      view.isImplicit
    ))
  }

  /**
    * Creates a new output structure that serves to contain the to-be-mapped assignments; called by
    * [[beginModule()]].
    *
    * You may override this method to do additional action.
    *
    * @see [[beginTheory()]], [[beginView()]]
    */
  protected def beginStructure(s: Structure, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Option[Structure] = s.tp.flatMap {
    case OMMOD(structureDomain) =>
      applyContainer(interp.ctrl.getModule(structureDomain)).getOrElse(return None)

      // inherit linear state from module where structure is declared
      containerState.inherit(diagState.getLinearState(s.home.toMPath))

      // TODO: s.dfC is thrown away
      Some(new Structure(
        home = OMMOD(applyModulePath(s.path.module)),
        name = s.name,
        TermContainer.asAnalyzed(OMMOD(applyModulePath(structureDomain))), TermContainer.empty(),
        s.isImplicit, s.isTotal
      ))
    case _ => None
  }

  /**
    * Creates a new output module that serves to contain the to-be-mapped assignments; called by
    * [[beginContainer()]].
    *
    * @see [[beginTheory()]], [[beginView()]], [[beginStructure()]].
    */
  private def beginModule(inModule: Module, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Option[Module] = {
    if (!diagState.seenModules.contains(inModule.path)) {
      NotApplicable.Module(inModule, "unbound module not in input diagram")
      return None
    }

    (inModule match {
      case thy: Theory => beginTheory(thy, containerState)
      case view: View => beginView(view, containerState)
    }).map(outModule => {
      diagState.seenModules += inModule.path

      if (diagState.inputToplevelModules.contains(inModule.path)) {
        interp.addToplevelResult(outModule)
      }

      outModule
    })
  }

  /**
    * See superclass documentation, or [[beginContainer()]].
    */
  final override protected def beginContainer(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Option[Container] = {
    val outContainer = inContainer match {
      case m: Module => beginModule(m, containerState)
      case s: Structure => beginStructure(s, containerState)
    }

    outContainer.map(outContainer => {
      interp.add(outContainer)
      diagState.processedElements.put(inContainer.path, outContainer)

      outContainer
    })
  }

  /**
    *
    * {{{
    *   include ?opDom [= E]  |-> include ?opCod [= E']
    *   include ?S [= E]      |-> include ?S [= E']         if there is an implicit morphism ?S -> ?opDom (case probably wrong)
    *   include ?S [= E]      |-> include ?op(S) [= E']     if ?S is in input diagram
    * }}}
    *
    * and E via
    * {{{
    *   OMIDENT(?opDom)       |-> OMIDENT(?opCod)
    *   OMIDENT(?S)           |-> OMIDENT(?S)              if there is an implicit morphisim ?S -> ?opDom (case probably wrong)
    *   OMIDENT(?S)           |-> OMIDENT(?op(S))          if ?T is in input diagram
    *   ?v                    |-> ?op(v)                   if ?v is in input diagram
    * }}}
    */
  final override protected def applyIncludeData(container: Container, containerState: LinearState, include: IncludeData)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = {
    val ctrl = interp.ctrl

    if (include.args.nonEmpty) ???

    val newFrom: MPath = include.from match {
      case `operatorDomain` => operatorCodomain.toMPath

      // classic case for include preserving behavior of linear operators
      case from if state.seenModules.contains(from) =>
        applyModule(ctrl.getModule(from))

        if (container.isInstanceOf[Theory]) {
          state.getLinearState(container.path).inherit(state.getLinearState(from))
        }

        applyModulePath(from)

      case from if ctrl.globalLookup.hasImplicit(OMMOD(from), OMMOD(operatorDomain)) =>
        // e.g. for a view v: ?S -> ?T and S, T both having meta theory ?meta,
        //      the view will feature an "include ?meta = OMIDENT(OMMOD(?meta))"
        //      but in general it might be something else
        //
        // todo: what to do here? add to context? just retain and hope there's an implicit morphism from from to operatorCodomain, too?
        from

      case _ =>
        interp.errorCont(InvalidElement(container, "Cannot handle include (or structure) of " +
          s"`${include.from}`: unbound in input diagram"))
        return
    }

    val newDf: Option[Term] = include.df.map {
      case OMIDENT(`operatorDomain`) => OMIDENT(OMMOD(operatorCodomain))
      case OMIDENT(OMMOD(thy)) if state.seenModules.contains(thy) => OMIDENT(OMMOD(applyModulePath(thy)))
      case OMIDENT(thy) if ctrl.globalLookup.hasImplicit(thy, OMMOD(operatorDomain)) =>
        // e.g. for a view v: ?S -> ?T and S, T both having meta theory ?meta,
        //      the view will feature an "include ?meta = OMIDENT(OMMOD(?meta))"
        //      but in general it might be something else
        //
        // todo: what to do here? add to context? just retain and hope there's an implicit morphism from from to operatorCodomain, too?
        OMIDENT(thy)

      case OMMOD(dfPath) if state.seenModules.contains(dfPath) =>
        // ???: error: morphism provided as definiens to include wasn't contained in diagram
        applyModule(ctrl.getModule(dfPath))

        if (container.isInstanceOf[View]) {
          state.getLinearState(container.path).inherit(state.getLinearState(dfPath))
        }

        OMMOD(applyModulePath(dfPath))
      case _ =>  ???
    }

    val s = Structure(
      home = OMMOD(applyModulePath(container.modulePath)),
      name = LocalName(newFrom),
      from = OMMOD(newFrom),
      df = newDf,
      isImplicit = if (container.isInstanceOf[Theory]) true else false,
      isTotal = include.total
    )
    interp.add(s)
    interp.endAdd(s)
  }
}

class IdentityLinearTransformer(private val domain: MPath) extends LinearModuleTransformer with DefaultLinearStateOperator {
  override val operatorDomain: MPath = domain
  override val operatorCodomain: MPath = domain

  override protected def applyModuleName(name: LocalName): LocalName = name

  final override protected def applyDeclaration(container: Container, containerState: LinearState, decl: Declaration)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = {}
}

/**
  * Linearly transforms theories to connecting views, and views not at all.
  *
  * invariants so far:
  *
  * - in and out have same domain/codomain
  * - applyDeclaration outputs declarations valid in a view (esp. for output FinalConstants that means they
  *   have a definiens)
  */
trait LinearConnectorTransformer extends LinearTransformer with RelativeBaseTransformer {
  val in: LinearModuleTransformer
  val out: LinearModuleTransformer

  // declare next two fields lazy, otherwise default initialization order entails in being null
  // see https://docs.scala-lang.org/tutorials/FAQ/initialization-order.html
  final override lazy val operatorDomain: MPath = in.operatorDomain
  final override lazy val operatorCodomain: MPath = in.operatorCodomain

  // doing this just in the Scala object would throw hard-to-debug "Exception at Initialization" errors
  private var hasRunSanityCheck = false
  private def sanityCheckOnce()(implicit interp: DiagramInterpreter): Unit = {
    if (hasRunSanityCheck) {
      return
    }
    hasRunSanityCheck = true
    sanityCheck()
  }

  /**
    * Runs a sanity check for whether [[in]] and [[out]] are actually "connectible" operators.
    *
    * The sanity check is only run once in the entire lifetime of ''this''.
    *
    * Subclasses may override and extend this method. Call ''super.sanityCheck()'' in those cases.
    */
  protected def sanityCheck()(implicit interp: DiagramInterpreter): Unit = {
    if (in.operatorDomain != out.operatorDomain) {
      // todo:
      // throw ImplementationError(s"Can only connect between two LinearModuleTransformers with same domain, got ${in.operatorDomain} and ${out.operatorDomain} for in and out, respectively.")
    }
  }

  /**
    * Creates a new output view that serves to contain the to-be-created assignments; called by
    * [[beginContainer()]].
    *
    * You may override this method to do additional action.
    *
    * @example Some transformers need to add includes. They should
    *          override the method as follows:
    *          {{{
    *          override protected def beginTheory(...): Option[View] = {
    *            super.beginTheory(...).map(view => {
    *              // add inclusion to view (via interp.ctrl)

    *              view
    *            })
    *          }
    *          }}}
    */
  protected def beginTheory(thy: Theory, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: LinearDiagramState): Option[View] = {
    val outPath = applyModulePath(thy.path)

    Some(View(
      outPath.doc, outPath.name,
      from = OMMOD(in.applyModulePath(thy.path)),
      to = OMMOD(out.applyModulePath(thy.path)),
      isImplicit = false
    ))
  }

  final override protected def beginContainer(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: LinearDiagramState): Option[Container] = {
    sanityCheckOnce()
    inContainer match {
      // only applicable on theories and their contents
      case _: View => None

      // we accept structures, but don't create a special out container for them
      // but to conform to the method signature, we must return Some(-) to keep processing
      case _: Structure => Some(inContainer)

      case inTheory: Theory =>
        beginTheory(inTheory, containerState).map(outView => {
          interp.addToplevelResult(outView)
          diagState.processedElements.put(inTheory.path, outView)

          outView
        })
    }
  }

  /**
    *
    * {{{
    *   include ?opDom   |-> include ?opCod = OMIDENT(?opDom)
    *   include ?S       |-> <nothing>                            if there is an implicit morphism ?S -> ?opDom
    *   include ?S       |-> include in(?S) = conn(?S)            if ?S is in input diagram
    *   include ?S = ?v  |-> include in(?S) = out(?v) . conn(?S)  if ?S, ?v are both in input diagram
    * }}}
    *
    * (In the last line, one path from the square of the commutativity of the natural transformation conn(-)
    *  is chosen. The other path could have been chosen as well.)
    *
    * We can handle the last two cases in a unified way as follows:
    * read ''include ?T'' as ''include ?T = OMIDENT(?T)'' and have cases
    *
    * {{{
    *   include ?S = ?v           |-> include in(?S) = out(?v) . conn(?S)
    *   include ?S = OMIDENT(?S)  |-> include in(?S) = OMIDENT(out(?S)) . conn(?S)
    * }}}
    *
    * Example:
    * Let S, T be theories, v: S -> T a view and suppose T contains an ''include ?S = ?v''. Then,
    * {{{
    *   S       in(S) -----conn(S)----> out(S)
    *   | v      | in(v)                  | out(v)
    *   v        v                        v
    *   T       in(T) -----conn(T)----> out(T)
    * }}}
    *
    * Here, in(T) contains an ''include in(S) = in(v)'' and out(T) contains analogously ''include out(S) = out(v)''.
    * Now, conn(T) must contain ''include in(S) = out(v) . conn(S)'' (or, alternatively,
    * ''include in(S) = conn(T) . in(v)'', but the latter but be somewhat self-referential in conn(T), so unsure
    * whether it works.)
    */
  final override protected def applyIncludeData(container: Container, containerState: LinearState, include: IncludeData)(implicit interp: DiagramInterpreter, state: LinearDiagramState): Unit = {
    val ctrl = interp.ctrl // shorthand

    if (include.args.nonEmpty) {
      // unsure what to do
      ???
    }

    val newFrom: MPath = include.from match {
      case p if p == in.operatorDomain =>
        in.operatorCodomain //, OMIDENT(OMMOD(in.operatorCodomain)))

      case from if state.seenModules.contains(from) =>
        applyModule(ctrl.getModule(from))
        state.getLinearState(container.path).inherit(state.getLinearState(from))

        in.applyModulePath(from) //, OMMOD(applyModulePath(include.from)))

      case _ =>
        interp.errorCont(InvalidElement(container, "Cannot handle include (or structure) of " +
          s"`${include.from}`: unbound in input diagram"))
        return
    }

    val newDf: Term = include.df.getOrElse(OMIDENT(OMMOD(include.from))) match {
      case OMMOD(v) if state.seenModules.contains(v) =>
        // even though we, as a connector, don't act on views, for consistency, we call applyModule nonetheless
        applyModule(ctrl.getModule(v))
        // todo: in which order does OMCOMP take its arguments? (Document this, too!)
        OMCOMP(OMMOD(out.applyModulePath(v)), OMMOD(applyModulePath(include.from)))

      case OMIDENT(OMMOD(thy)) if state.seenModules.contains(thy) =>
        OMMOD(applyModulePath(include.from))

      case OMIDENT(OMMOD(p)) if p == in.operatorDomain =>
        OMMOD(in.operatorCodomain)

      case _ => ???
    }

    val outputInclude = Include.assignment(
      home = OMMOD(applyModulePath(container.path.toMPath)),
      from = newFrom,
      df = Some(newDf)
    )
    interp.add(outputInclude)
    interp.endAdd(outputInclude)
  }
}


