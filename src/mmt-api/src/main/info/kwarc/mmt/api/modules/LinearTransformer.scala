package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api.objects.{OMIDENT, OMMOD, Term}
import info.kwarc.mmt.api.symbols.{Declaration, Include, IncludeData, Structure}
import info.kwarc.mmt.api.{GeneralError, MPath, Path}

trait FunctorialTransformer extends FunctorialOperatorState with ModulePathTransformer {
  /**
    * Applies a module.
    *
    * Invariants:
    *
    *  - m.isInstanceOf[Theory] => applyModule(m).isInstanceOf[Theory]
    *  - m.isInstanceOf[View] => applyModule(m).isInstanceOf[View]
    *  - post condition: returned module is added
    *    - (a) to state.processedModules
    *    - (b) to [[DiagramInterpreter.addResult() interp.addResult()]]
    *
    * take care not not needlessly compute, check state.processedModules first:
    * state.processedModules.get(m.path).foreach(return _)
    */
  protected def applyModule(m: Module)(implicit interp: DiagramInterpreter, state: DiagramState): Module
}

trait LinearTransformer extends FunctorialTransformer with LinearOperatorState {
  // beware of the ordering of the "with" (not commutative)
  // ContainerElement[Declaration] declares getDeclarations: List[Declaration] and we want that
  // (ContentElement.getDeclarations: List[ContentElement] is too weak)
  // type Container = ContentElement with ContainerElement[Declaration]

  type Container = ModuleOrLink

  protected def applyContainerBegin(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Boolean

  /**
    * post-condition: add itself to state.processedElements
    */
  protected def applyContainerEnd(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: DiagramState): Unit

  /**
    * post-condition: adds [[decl]] to [[state.processedDeclarations]]
    * @return Either Nil or a list with (1 + connectionTypes.size) elements, each of them being a list containing
    *         declarations (possibly none).
    */
  protected def applyDeclaration(container: Container, containerState: LinearState, decl: Declaration)(implicit diagInterp: DiagramInterpreter, state: LinearState): List[Declaration]

  protected def applyStructure(container: Container, containerState: LinearState, s: Structure)(implicit interp: DiagramInterpreter, state: DiagramState): List[Declaration]

  protected def applyIncludeData(container: Container, containerState: LinearState, include: IncludeData)(implicit interp: DiagramInterpreter, state: DiagramState): List[Declaration]

  final protected def applyModule(inModule: Module)(implicit interp: DiagramInterpreter, state: DiagramState): Module = {
    applyContainer(inModule)
    state.processedElements(inModule.path) match {
      case m: Module => m
      case _ => throw new GeneralError("LinearOperator transformed module into an element of other type")
    }
  }

  final protected def applyContainer(inContainer: Container)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = {
    if (state.processedElements.contains(inContainer.path)) {
      return
    }

    val inLinearState = state.initAndRegisterNewLinearState(inContainer)

    if (!applyContainerBegin(inContainer, inLinearState)) {
      return
    }

    inContainer.getDeclarations.foreach(decl => {
      inLinearState.registerDeclaration(decl)
      val newDeclarations: List[Declaration] = decl match {
        case Include(includeData) => applyIncludeData(inContainer, inLinearState, includeData)
        case s: Structure => applyStructure(inContainer, inLinearState, s)
        case decl: Declaration => applyDeclaration(inContainer, inLinearState, decl)
      }

      newDeclarations.foreach(interp.add)
    })

    applyContainerEnd(inContainer, inLinearState)
  }
}

trait LinearModuleTransformer extends LinearTransformer {
  protected val operatorDomain: MPath
  protected val operatorCodomain: MPath

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

          new Structure(
            home = OMMOD(applyModulePath(s.path.module)),
            name = s.name,
            s.tpC, s.dfC,
            s.isImplicit, s.isTotal
          )
        case _ => ???
      }
    }

    interp.add(outContainer)
    diagState.processedElements += (outContainer.path, outContainer)

    true
  }

  final override protected def applyStructure(container: Container, s: Structure)(implicit interp: DiagramInterpreter, state: DiagramState): List[Declaration] = {
    applyContainer(s)
    List(state.processedElements(s.path).asInstanceOf[Structure])
  }

  final override protected def applyIncludeData(container: Container, include: IncludeData)(implicit interp: DiagramInterpreter, state: DiagramState): List[Declaration] = {
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

    List(Include(OMMOD(applyModulePath(container.modulePath)), newFrom, args = Nil, newDf, include.total))
  }
}

trait LinearInToOutConnectionCreator extends LinearTransformer {
  final override protected def applyContainerBegin(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: LinearDiagramState): Boolean = inContainer match {
    // only applicable on theories and their contents
    case _: View => false

    // we accept structures, but don't create a special out container for them
    case _: Structure => true

    case thy: Theory =>
      val outPath = applyModulePath(thy.path)

      val outContainer = View(
        outPath.doc, outPath.name,
        from = thy.toTerm,
        to = OMMOD(applyModulePath(thy.path)),
        isImplicit = false
      )

      interp.add(outContainer)
      diagState.processedElements += (outContainer.path, outContainer)
      true
  }

  final override protected def applyContainerEnd(inContainer: Container, containerState: LinearState)(implicit interp: DiagramInterpreter, diagState: LinearDiagramState): Unit = {} // nop
}

/**
  *
  * todo Navid: remove after refactoring (2020-11-17)


abstract class ConnectionType {
    // dummy field needed to invoke nominal subtyping wrt. InToOut and OutToIn classes below
    protected val _type: String
    def applyModuleName(name: LocalName): LocalName
    final def applyModulePath(p: MPath): MPath = p.doc ? applyModuleName(p.name)
  }
  abstract case class InToOutMorphismConnectionType() extends ConnectionType() {
    override protected val _type: String = "inToOut"
  }
  abstract case class OutToInMorphismConnectionType() extends ConnectionType() {
    override protected val _type: String = "outToIn"
  }

  object InToOutMorphismConnectionType {
    def suffixed(suffix: String): InToOutMorphismConnectionType = new InToOutMorphismConnectionType {
      override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple(suffix)
    }
  }

  object OutToInMorphismConnectionType {
    def suffixed(suffix: String): OutToInMorphismConnectionType = new OutToInMorphismConnectionType {
      override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple(suffix)
    }
  }
*/