package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.{Module, Theory, View}
import info.kwarc.mmt.api.objects.{OMIDENT, OMMOD, Term}
import info.kwarc.mmt.api.symbols.{Constant, IncludeData, Structure, TermContainer}
import info.kwarc.mmt.api.{InvalidElement, LocalName, MPath}

/**
  * Linearly transforms theories to theories, and views to views.
  * A functor on diagrams, which acts include-preservingly and declaration-by-declaration in theories
  * and views.
  *
  * Implementors must implement
  *
  *  - `applyConstant()` (inherited as [[LinearTransformer.applyConstant()]])
  *
  * and may override, among other methods, in particular
  *
  *  - `beginTheory()`
  *  - `beginView()`
  *  - `beginStructure()`
  */
trait LinearFunctorialTransformer extends LinearModuleTransformer with RelativeBaseTransformer {

  /**
    * Creates a new output theory that serves to contain the to-be-mapped declarations; called by
    * [[beginModule()]].
    *
    * You may override this method to do additional action.
    *
    * @return The output theory. If `Some(outTheory)` is returned, you must have called
    *         [[DiagramInterpreter.add()]] on `outTheory`.
    *
    * @example Some transformers need to add includes at the beginning of every theory, and
    *          correspondingly an include assignment at the beginning of every view.
    *
    *          The transformers can override [[beginTheory()]] and [[beginView()]] as follows:
    *          {{{
    *            val additionalTheory: MPath
    *
    *            override protected def beginTheory(thy: Theory, state: LinearState)(implicit interp: DiagramInterpreter): Option[Theory] = {
    *              super.beginTheory(thy, state).map(outTheory => {
    *                val include = PlainInclude(additionalTheory, outTheory.path)
    *                interp.add(include)
    *                interp.endAdd(include)  // don't forget endAdd!
    *                outTheory
    *              })
    *            }
    *
    *            override protected def beginView(view: View, state: LinearState)(implicit interp: DiagramInterpreter): Option[View] = {
    *              super.beginView(view, state).map(outView => {
    *                val include = Include.assignment(
    *                  outView.toTerm,
    *                  OMMOD(additionalTheory),
    *                  Some(OMIDENT(OMMOD(additionalTheory)))
    *                )
    *                interp.add(include)
    *                interp.endAdd(include)  // don't forget endAdd!
    *
    *                outView
    *              })
    *          }}}
    */
  protected def beginTheory(thy: Theory, state: LinearState)(implicit interp: DiagramInterpreter): Option[Theory] = {
    val outPath = applyModulePath(thy.path)
    val newMeta = thy.meta.map {
      case mt if operatorDomain.hasImplicitFrom(mt)(interp.ctrl.library) =>
        applyMetaModule(OMMOD(mt))(interp.ctrl.globalLookup).toMPath
      case mt =>
        if (applyModule(interp.ctrl.getModule(mt))(state.diagramState, interp).isEmpty) {
          interp.errorCont(InvalidElement(thy, s"Theory had meta theory `$mt` for which there " +
            s"was no implicit morphism into `$operatorDomain`. Recursing into meta theory as usual " +
            s"failed, too; reasons are probably logged above. Keeping meta theory as-is."))
          mt
        } else {
          applyModulePath(mt)
        }
    }

    val outTheory = Theory.empty(outPath.doc, outPath.name, newMeta)
    interp.add(outTheory)

    Some(outTheory)
  }

  /**
    * Creates a new output view that serves to contain the to-be-mapped assignments; called by
    * [[beginModule()]].
    *
    * @return The output view. If `Some(outView)` is returned, you must have called
    *         [[DiagramInterpreter.add()]] on `outView`.
    *
    * You may override this method to do additional action.
    *
    * @see [[beginTheory()]] for an example
    */
  protected def beginView(view: View, state: LinearState)(implicit interp: DiagramInterpreter): Option[View] = {
    implicit val diagramState: DiagramState = state.diagramState

    if (applyModule(interp.ctrl.getModule(view.from.toMPath)).isEmpty) {
      return None
    }
    if (applyModule(interp.ctrl.getModule(view.to.toMPath)).isEmpty) {
      return None
    }

    state.inherit(diagramState.getLinearState(view.to.toMPath))

    val outPath = applyModulePath(view.path)
    val outView = View(
      outPath.doc, outPath.name,
      OMMOD(applyModulePath(view.from.toMPath)), OMMOD(applyModulePath(view.to.toMPath)),
      view.isImplicit
    )
    interp.add(outView)

    Some(outView)
  }

  /**
    * Creates a new output structure that serves to contain the to-be-mapped assignments; called by
    * [[beginModule()]].
    *
    * @return The output structure. If `Some(outStructure)` is returned, you must have called
    *         [[DiagramInterpreter.add()]] on `outStructure`.
    *
    * You may override this method to do additional action.
    *
    * @see [[beginTheory()]], [[beginView()]]
    */
  protected def beginStructure(s: Structure, state: LinearState)(implicit interp: DiagramInterpreter): Option[Structure] = s.tp.flatMap {
    case OMMOD(structureDomain) =>
      val newStructureDomain = applyModule(interp.ctrl.getModule(structureDomain))(state.diagramState, interp).getOrElse(
        return None
      )

      // inherit linear state from module where structure is declared
      state.inherit(state.diagramState.getLinearState(s.home.toMPath))

      // TODO: s.dfC is thrown away/ignored
      val outStructure = new Structure(
        home = OMMOD(applyModulePath(s.path.module)),
        name = s.name,
        tpC = TermContainer.asAnalyzed(newStructureDomain.toTerm),
        dfC = TermContainer.empty(),
        s.isImplicit, s.isTotal
      )
      interp.add(outStructure)
      Some(outStructure)
    case _ => None
  }

  /**
    * Creates a new output module that serves to contain the to-be-mapped assignments; called by
    * [[beginContainer()]].
    *
    * @see [[beginTheory()]], [[beginView()]], [[beginStructure()]].
    */
  private def beginModule(inModule: Module, state: LinearState)(implicit interp: DiagramInterpreter): Option[Module] = {
    val diagramState = state.diagramState

    if (!diagramState.seenModules.contains(inModule.path)) {
      interp.errorCont(InvalidElement(
        inModule,
        "unbound module not in input diagram"
      ))
      return None
    }

    (inModule match {
      case thy: Theory => beginTheory(thy, state)
      case view: View => beginView(view, state)
    }).map(outModule => {
      diagramState.seenModules += inModule.path

      diagramState.processedElements.put(inModule.path, outModule)
      if (diagramState.inputDiagram.modules.contains(inModule.path)) {
        interp.addToplevelResult(outModule)
      }

      outModule
    })
  }

  /**
    * See superclass documentation, or [[beginContainer()]].
    */
  final override def beginContainer(inContainer: Container, state: LinearState)(implicit interp: DiagramInterpreter): Boolean = {
    val outContainer = inContainer match {
      case m: Module => beginModule(m, state)
      case s: Structure => beginStructure(s, state)
    }

    outContainer match {
      case Some(outContainer) =>
        state.diagramState.processedElements.put(inContainer.path, outContainer)
        state.outContainer = outContainer
        true

      case _ => false
    }
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
  override def applyIncludeData(include: IncludeData, container: Container)(implicit state: LinearState, interp: DiagramInterpreter): Unit = {
    val ctrl = interp.ctrl
    implicit val library: Lookup = ctrl.library
    implicit val diagramState: DiagramState = state.diagramState

    if (include.args.nonEmpty) ???

    val newFrom: MPath = include.from match {
      case p if operatorDomain.hasImplicitFrom(p) => applyMetaModule(OMMOD(p)).toMPath

      // classic case for include preserving behavior of linear operators
      case from if diagramState.seenModules.contains(from) =>
        applyModule(ctrl.getModule(from))

        if (container.isInstanceOf[Theory]) {
          state.inherit(diagramState.getLinearState(from))
        }

        applyModulePath(from)

      case from =>
        interp.errorCont(InvalidElement(container, s"Origin ('from') `$from` of include or structure unbound " +
          "in input diagram, leaving as-is"))
        from
    }

    val newDf: Option[Term] = include.df.map {
      case df @ OMIDENT(OMMOD(p)) if operatorDomain.hasImplicitFrom(p) => applyMetaModule(df)
      case OMIDENT(OMMOD(thy)) if diagramState.seenModules.contains(thy) => OMIDENT(OMMOD(applyModulePath(thy)))
      case df @ OMIDENT(OMMOD(thy)) if operatorDomain.hasImplicitFrom(thy) =>
        // e.g. for a view v: ?S -> ?T and S, T both having meta theory ?meta,
        //      the view will feature an "include ?meta = OMIDENT(OMMOD(?meta))"
        //      but in general it might be something else
        //
        // todo: what to do here? add to context? just retain and hope there's an implicit morphism from from to operatorCodomain, too?
        applyMetaModule(df)

      case OMMOD(dfPath) if diagramState.seenModules.contains(dfPath) =>
        // ???: error: morphism provided as definiens to include wasn't contained in diagram
        applyModule(ctrl.getModule(dfPath))

        if (container.isInstanceOf[View]) {
          state.inherit(diagramState.getLinearState(dfPath))
        }

        OMMOD(applyModulePath(dfPath))
      case df =>
        interp.errorCont(InvalidElement(container, s"Definiens `$df` of include or structure unbound in input " +
          s"diagram, leaving as-is"))
        df
    }

    val s = Structure(
      home = OMMOD(applyModulePath(container.modulePath)),
      name = LocalName(newFrom),
      from = OMMOD(newFrom),
      df = newDf,
      isImplicit = if (container.isInstanceOf[Theory]) true else false,
      isTotal = include.total
    )

    // TODO hack to prevent: "add error: a declaration for the name [...] already exists [...]"
    //      when refactoring the whole framework, we should fix this anyway in the course of doing so
    if (ctrl.getO(s.path).isEmpty) {
      interp.add(s)
      interp.endAdd(s)
    }
  }
}

object LinearFunctorialTransformer {
  /**
    * No-op identity [[LinearTransformer transformer]] on some diagram.
    *
    * Its purpose is to serve for the `in` or `out` field of [[LinearConnectorTransformer]]s.
    */
  def identity(domain: Diagram): LinearFunctorialTransformer = new LinearFunctorialTransformer with DefaultLinearStateOperator {
    override val operatorDomain: Diagram = domain
    override val operatorCodomain: Diagram = domain
    override def applyMetaModule(m: Term)(implicit lookup: Lookup): Term = m

    override def applyModuleName(name: LocalName): LocalName = name

    override def applyConstant(c: Constant, container: Container)(implicit state: SkippedDeclsExtendedLinearState, interp: DiagramInterpreter): Unit = {}
  }

  def identity(domainTheory: MPath): LinearFunctorialTransformer = identity(Diagram(List(domainTheory), None))
}
