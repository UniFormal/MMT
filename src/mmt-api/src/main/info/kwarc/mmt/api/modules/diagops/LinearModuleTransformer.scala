package info.kwarc.mmt.api.modules.diagops

import info.kwarc.mmt.api.{InvalidElement, LocalName, MPath}
import info.kwarc.mmt.api.modules.{DiagramInterpreter, Module, Theory, View}
import info.kwarc.mmt.api.objects.{OMIDENT, OMMOD, Term}
import info.kwarc.mmt.api.symbols.{Declaration, IncludeData, Structure, TermContainer}

/**
  * Linearly transforms theories to theories, and views to views.
  * A functor on diagrams, which acts include-preservingly and declaration-by-declaration in theories
  * and views.
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