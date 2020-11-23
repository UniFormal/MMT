package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{OMIDENT, OMMOD, Term}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, Include, IncludeData, Structure, TermContainer}
import info.kwarc.mmt.api.{ComplexStep, ContainerElement, GeneralError, ImplementationError, LocalName, MPath}

trait ModulePathTransformer {
  protected def applyModuleName(name: LocalName): LocalName

  final def applyModulePath(mpath: MPath): MPath = {
    if (mpath == mpath.mainModule) {
      mpath.doc ? applyModuleName(mpath.name)
    } else {
      val newMPath = applyModulePath(mpath.mainModule)
      newMPath.doc ? LocalName(newMPath.name.steps ::: mpath.name.drop(1))
    }
  }
}

/**
  * Transforms modules to modules in a diagram.
  *
  * An abstraction over [[FunctorialOperator]] leaving out details for parsing the input
  * diagram expression and constructing the output diagram expression.
  */
trait FunctorialTransformer extends FunctorialOperatorState with ModulePathTransformer {
  /**
    * Applies a module.
    *
    * Invariants:
    *
    *  - m.isInstanceOf[Theory] => applyModule(m).isInstanceOf[Theory] (actually not any longer?)
    *  - m.isInstanceOf[View] => applyModule(m).isInstanceOf[View] (actually not any longer?)
    *  - post condition: returned module is added
    *    - (a) to state.processedModules
    *    - (b) to [[DiagramInterpreter.addResult() interp.addResult()]]
    *
    * take care not not needlessly compute, check state.processedModules first:
    * state.processedModules.get(m.path).foreach(return _)
    */
  protected def applyModule(m: Module)(implicit interp: DiagramInterpreter, state: DiagramState): Option[Module]
}

/**
  * Linearly transforms modules to modules in a diagram.
  *
  * It is left to implementations how the type of input module relates to the type of output module:
  * for instance, the implementation [[LinearOperator]]s guarantees that applyModule() transforms
  * theories to theories and views to views.
  * On the other hand, the implementation (TODO: mention connectors) transforms theories to (diagram-connecting)
  * views and transforms views not at all.
  */
trait LinearTransformer extends FunctorialTransformer with LinearOperatorState {
  // beware of the ordering of the "with" (not commutative)
  // ContainerElement[Declaration] declares getDeclarations: List[Declaration] and we want that
  // (ContentElement.getDeclarations: List[ContentElement] is too weak)
  // type Container = ContentElement with ContainerElement[Declaration]

  type Container = ModuleOrLink

  protected def applyContainerBegin(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Boolean

  /**
    * pre-condition: only called if applyContainerBegin returned true before
    * post-condition:
    *   - state.processedElements contains an entry (inContainer.path, outContainer)
    *   - for inContainer fulfilling [[DiagramInterpreter.hasToplevelResult()]]
    *     outContainer must be a [[Module]] and be added via [[DiagramInterpreter.addToplevelResult()]].
    */
  protected def applyContainerEnd(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Unit

  /**
    * Post-condition: all added declaration added to ''interp''
    * (i.e. called [[DiagramInterpreter.add()]] *and* [[DiagramInterpreter.endAdd()]]).
    */
  protected def applyDeclaration(container: Container, containerState: LinearState, decl: Declaration)(implicit interp: DiagramInterpreter, state: DiagramState): Unit

  /**
    * Post-condition: all added declaration added to ''interp''
    * (i.e. called [[DiagramInterpreter.add()]] *and* [[DiagramInterpreter.endAdd()]]).
    */
  protected def applyStructure(container: Container, containerState: LinearState, s: Structure)(implicit interp: DiagramInterpreter, state: DiagramState): Unit

  /**
    * Post-condition: all added declaration added to ''interp''
    * (i.e. called [[DiagramInterpreter.add()]] *and* [[DiagramInterpreter.endAdd()]]).
    */
  protected def applyIncludeData(container: Container, containerState: LinearState, include: IncludeData)(implicit interp: DiagramInterpreter, state: DiagramState): Unit

  final protected def applyModule(inModule: Module)(implicit interp: DiagramInterpreter, state: DiagramState): Option[Module] = {
    if (applyContainer(inModule)) {
      state.processedElements.get(inModule.path) match {
        case None =>
          interp.errorCont(ImplementationError(s"Diagram operator ${getClass} did signal it did processing on ${inModule.path}, but actually did not register anything there."))
          None

        case Some(m: Module) => Some(m)
        case Some(x) => throw GeneralError(s"Diagram operator ${getClass} transformed toplevel module ${inModule} to something else, namely $x")
      }
    } else {
      None
    }
  }

  /**
    * @return true if element was processed (or alraedy had been processed), false otherwise.
    */
  final protected def applyContainer(inContainer: Container)(implicit interp: DiagramInterpreter, state: DiagramState): Boolean = {
    if (state.processedElements.contains(inContainer.path)) {
      return true
    }

    val inLinearState = state.initAndRegisterNewLinearState(inContainer)

    if (!applyContainerBegin(inContainer, inLinearState)) {
      return false
    }

    inContainer.getDeclarations.foreach(decl => {
      inLinearState.registerDeclaration(decl)
      decl match {
        case Include(includeData) => applyIncludeData(inContainer, inLinearState, includeData)
        case s: Structure => applyStructure(inContainer, inLinearState, s)
        case decl: Declaration => applyDeclaration(inContainer, inLinearState, decl)
      }
    })

    applyContainerEnd(inContainer, inLinearState)
    true
  }
}

/**
  * Linearly transforms theories to theories and views to views.
  *
  * A small abstraction over [[LinearOperator]]s that on top perform some parsing of the input diagram
  * expression and construction of the output diagram expression.
  */
trait LinearModuleTransformer extends LinearTransformer {
  val operatorDomain: MPath
  val operatorCodomain: MPath

  final override protected def applyContainerBegin(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Boolean = {
    val outContainer = inContainer match {
      case inModule: Module =>
        val outPath = applyModulePath(inContainer.path.toMPath)
        val outModule = inModule match {
          case thy: Theory =>
            Theory.empty(outPath.doc, outPath.name, thy.meta)

          case view: View =>
            applyModule(interp.ctrl.getAs(classOf[Module], view.from.toMPath))
            applyModule(interp.ctrl.getAs(classOf[Module], view.to.toMPath))

            containerState.inherit(diagState.getLinearState(view.to.toMPath))

            View(
              outPath.doc, outPath.name,
              OMMOD(applyModulePath(view.from.toMPath)), OMMOD(applyModulePath(view.to.toMPath)),
              view.isImplicit
            )
        }

        diagState.seenModules += inModule.path

        if (diagState.inputToplevelModules.contains(inModule.path)) {
          interp.addToplevelResult(outModule)
        }

        outModule

      case s: Structure => s.tp match {
        case Some(OMMOD(structureDomain)) =>
          applyContainer(interp.ctrl.getAs(classOf[Module], structureDomain))

          // inherit linear state from module where structure is declared
          containerState.inherit(diagState.getLinearState(s.home.toMPath))

          // TODO: s.dfC is thrown away
          new Structure(
            home = OMMOD(applyModulePath(s.path.module)),
            name = s.name,
            TermContainer.asAnalyzed(OMMOD(applyModulePath(structureDomain))), TermContainer.empty(),
            s.isImplicit, s.isTotal
          )
        case _ => ???
      }
    }

    interp.add(outContainer)
    diagState.processedElements.put(inContainer.path, outContainer)

    true
  }

  final override protected def applyContainerEnd(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Unit = {
    val outContainer = diagState.processedElements(inContainer.path).asInstanceOf[ContainerElement[_]]
    interp.endAdd(outContainer)
  }

  final override protected def applyStructure(container: Container, containerState: LinearState, s: Structure)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = applyContainer(s)

  final override protected def applyIncludeData(container: Container, containerState: LinearState, include: IncludeData)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = {
    val ctrl = interp.ctrl

    if (include.args.nonEmpty) ???

    val newFrom: MPath = include.from match {
      case `operatorDomain` => operatorCodomain.toMPath

      // classic case for include preserving behavior of linear operators
      case from if state.seenModules.contains(from) =>
        applyModule(ctrl.getAs(classOf[Module], from))

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
        // theory included wasn't contained in diagram actually
        throw GeneralError(s"Unbound module in diagram: ${container.path} contains include of ${include.from} " +
          s"which is a module that is neither contained in the diagram nor included in the diagram's " +
          s"base theory (${operatorDomain}), not even by an implicit morphism instead of an inclusion.")
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
        applyModule(ctrl.getAs(classOf[Module], dfPath))

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

/**
  *
  * invariants so far:
  *
  * - in and out have same domain/codomain
  * - applyDeclaration outputs declarations valid in a view (esp. for output FinalConstants that means they
  *   have a definiens)
  */
trait LinearConnectorTransformer extends LinearTransformer {
  val in: LinearModuleTransformer
  val out: LinearModuleTransformer

  // declare next two fields lazy, otherwise default initialization order entails in being null
  // see https://docs.scala-lang.org/tutorials/FAQ/initialization-order.html
  lazy val operatorDomain: MPath = in.operatorDomain
  lazy val operatorCodomain: MPath = in.operatorCodomain

  // doing this just in the Scala object would throw hard-to-debug "Exception at Initialization" errors
  private def runSanityCheck(): Unit = {
    if (in.operatorDomain != out.operatorDomain) {
      throw GeneralError(s"Can only connect between two LinearModuleTransformers with same domain, got ${in.operatorDomain} and ${out.operatorDomain} for in and out, respectively.")
    }
    if (in.operatorCodomain != out.operatorCodomain) {
      throw GeneralError(s"Can only connect between two LinearModuleTransformers with same codomain, got ${in.operatorCodomain} and ${out.operatorCodomain} for in and out, respectively.")
    }
  }

  final override protected def applyContainerBegin(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: LinearDiagramState): Boolean = {
    runSanityCheck()
    inContainer match {
      // only applicable on theories and their contents
      case _: View => false

      // we accept structures, but don't create a special out container for them
      case _: Structure => true

      case thy: Theory =>
        val outPath = applyModulePath(thy.path)

        val outContainer = View(
          outPath.doc, outPath.name,
          from = OMMOD(in.applyModulePath(thy.path)),
          to = OMMOD(out.applyModulePath(thy.path)),
          isImplicit = false
        )

        interp.addToplevelResult(outContainer)
        diagState.processedElements.put(inContainer.path, outContainer)
        true
    }
  }

  final override protected def applyContainerEnd(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: LinearDiagramState): Unit = {
    interp.endAdd(diagState.processedElements(inContainer.path).asInstanceOf[ContainerElement[_]])
  }

  final override protected def applyIncludeData(container: Container, containerState: LinearState, include: IncludeData)(implicit interp: DiagramInterpreter, state: LinearDiagramState): Unit = {
    val ctrl = interp.ctrl // shorthand

    if (include.df.nonEmpty) {
      // unsure what to do: case: input theory has defined include
      // probably, connecting view does not need to map the included declarations (?)
      ???
    }
    if (include.args.nonEmpty) {
      // unsure what to do
      ???
    }

    val (newFrom, newDf): (MPath, Term) = include.from match {
      case p if p == in.operatorDomain =>
        (in.operatorCodomain, OMIDENT(OMMOD(in.operatorCodomain)))

      // classic case for include preserving behavior of linear operators
      case from if state.seenModules.contains(from) =>
        applyModule(ctrl.getAs(classOf[Module], from))
        state.getLinearState(container.path).inherit(state.getLinearState(from))

        (in.applyModulePath(from), OMMOD(applyModulePath(include.from)))

      case from if ctrl.globalLookup.hasImplicit(OMMOD(from), OMMOD(in.operatorDomain)) =>
        // e.g. for a view v: ?S -> ?T and S, T both having meta theory ?meta,
        //      the view will feature an "include ?meta = OMIDENT(OMMOD(?meta))"
        //      but in general it might be something else
        //
        // todo: what to do here? add to context? just retain and hope there's an implicit morphism from from to operatorCodomain, too?
        (from, OMIDENT(OMMOD(from)))

      case _ =>
        // theory included wasn't contained in diagram actually
        throw GeneralError(s"Unbound module in diagram: ${container.path} contains include of ${include.from} " +
          s"which is a module that is neither contained in the diagram nor included in the diagram's " +
          s"base theory (${in.operatorDomain}), not even by an implicit morphism instead of an inclusion.")
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


/**
  * A [[LinearTransformer]] that works constant-by-constant: structural features are elaborated before calling
  * the to-be-implemented method ''applyConstant''.
  */
trait ElaboratingLinearTransformer extends LinearTransformer {
  protected def applyConstant(container: Container, c: Constant)(implicit diagInterp: DiagramInterpreter, state: LinearState): Unit

  final override protected def applyDeclaration(container: Container, containerState: LinearState, decl: Declaration)(implicit diagInterp: DiagramInterpreter, state: DiagramState): Unit = {
    decl match {
      case c: Constant => applyConstant(container, c)(diagInterp, containerState)
      case _ =>
        // do elaboration, then call applyConstant
        // diagInterp.errorCont(InvalidElement(decl, s"Linear operator ${getClass} cannot process this element " +
        //s"of u"))
        ???
    }
  }
}

trait SimpleLinearModuleTransformer extends LinearModuleTransformer with ElaboratingLinearTransformer with DefaultLinearStateOperator {
  type SimpleConstant = (LocalName, Term, Option[Term])

  /**
    *
    * @return In case the operator is not applicable on c, emit an error via diagInterp.errorHandler and
    *         return Nil.
    */
  protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: LinearState): List[SimpleConstant]

  final override protected def applyConstant(container: Container, c: Constant)(implicit interp: DiagramInterpreter, state: LinearState): Unit = {
    val simplifiedName: LocalName = container match {
      case _: Theory => c.name
      case v: View => c.name match {
        case LocalName(ComplexStep(mpath) :: domainSymbolName) if mpath == v.from.toMPath =>
          LocalName(domainSymbolName)
        case _ => c.name // fallback
      }
    }

    val rawTp = c.tp.getOrElse({
      interp.errorCont(GeneralError(s"Operator ${getClass} not applicable on constants without type component"))
      return
    })
    val rawDf = c.df

    val tp = interp.ctrl.globalLookup.ExpandDefinitions(rawTp, state.skippedDeclarationPaths)
    val df = rawDf.map(interp.ctrl.globalLookup.ExpandDefinitions(_, state.skippedDeclarationPaths))

    applyConstantSimple(container, c, simplifiedName, tp, df).foreach {
      case (name, tp, df) =>
        if (container.isInstanceOf[View] && df.isEmpty) {
          throw GeneralError(s"applyConstant of SimpleLinearOperator subclass ${this.getClass} returned empty definiens for view declaration ${c.path}")
        }

        val newName = container match {
          case _: Theory => name
          case v: View => LocalName(v.from.toMPath) / name
        }

        interp.add(new FinalConstant(
          home = OMMOD(applyModulePath(container.modulePath)),
          name = newName, alias = Nil,
          tpC = TermContainer.asAnalyzed(tp), dfC = TermContainer.asAnalyzed(df),
          rl = None, notC = NotationContainer.empty(), vs = c.vs
        ))
    }
  }
}

trait SimpleLinearConnectorTransformer extends LinearConnectorTransformer with ElaboratingLinearTransformer with DefaultLinearStateOperator {
  protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: LinearState): List[(LocalName, Term, Term)]

  final override protected def applyConstant(container: Container, c: Constant)(implicit interp: DiagramInterpreter, state: LinearState): Unit = {
    val rawTp = c.tp.getOrElse({
      interp.errorCont(GeneralError(s"Operator ${getClass} not applicable on constants without type component"))
      return
    })
    val rawDf = c.df

    val tp = interp.ctrl.globalLookup.ExpandDefinitions(rawTp, state.skippedDeclarationPaths)
    val df = rawDf.map(interp.ctrl.globalLookup.ExpandDefinitions(_, state.skippedDeclarationPaths))

    applyConstantSimple(container, c, c.name, tp, df).foreach {
      case (name, tp, df) =>
        // todo: sanity check whether name == c.name?

        interp.add(new FinalConstant(
          home = OMMOD(applyModulePath(container.modulePath)),
          name = c.name, alias = Nil,
          tpC = TermContainer.asAnalyzed(tp), dfC = TermContainer.asAnalyzed(df),
          rl = None, notC = NotationContainer.empty(), vs = c.vs
        ))
    }
  }

  final override protected def applyStructure(container: Container, containerState: SkippedDeclsExtendedLinearState, s: Structure)(implicit interp: DiagramInterpreter, state: LinearDiagramState): Unit = {
    // todo
    ???
  }
}
