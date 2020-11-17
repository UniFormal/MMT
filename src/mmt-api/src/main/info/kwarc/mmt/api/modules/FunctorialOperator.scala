package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.{ComplexStep, ContainerElement, ContentElement, ContentPath, GeneralError, InvalidElement, LocalName, MPath, Path}

abstract class FunctorialOperator extends DiagramOperator with FunctorialOperatorState {
  protected def applyModuleName(name: LocalName): LocalName
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

  protected def applyModulePath(mpath: MPath): MPath = {
    if (mpath == mpath.mainModule) {
      mpath.doc ? applyModuleName(mpath.name)
    } else {
      val newMPath = applyModulePath(mpath.mainModule)
      newMPath.doc ? LocalName(newMPath.name.steps ::: mpath.name.drop(1))
    }
  }

  protected def acceptDiagram(diagram: Term): Option[List[MPath]]
  protected def submitDiagram(newModules: List[MPath]): Term

  final override def apply(diagram: Term, interp: DiagramInterpreter)(implicit ctrl: Controller): Option[Term] = diagram match {
    case OMA(OMS(`head`), List(inputDiagram)) =>
      interp(inputDiagram).flatMap(acceptDiagram) match {
        case Some(modulePaths) =>
          val modules: Map[MPath, Module] = modulePaths.map(p => (p, interp.ctrl.getAs(classOf[Module], p))).toMap
          val state = initDiagramState(diagram, modules, interp)

          val newModulePaths = modulePaths.map(modulePath => {
            val newModule = applyModule(interp.get(modulePath))(interp, state)

            state.processedElements.get(modulePath) match {
              case Some(`newModule`) => // ok
              case Some(m) if m != newModule =>
                throw new Exception("...")

              case None =>
                throw new Exception("...")
            }
            if (!interp.hasToplevelResult(newModule.path)) {
              throw GeneralError("diagram operators' applyModule should insert resulting module to DiagramInterpreter")
            }

            newModule.path
          })
          // todo: instead get new module paths from interp?
          Some(submitDiagram(newModulePaths))

        case None => None
      }

    case _ => None
  }
}

abstract class LinearOperator extends FunctorialOperator with LinearOperatorState {
  // to be overridden in subclasses:
  protected val operatorDomain: MPath
  protected val operatorCodomain: MPath
  protected val connectionTypes : List[ConnectionType] = Nil

  /**
    * post-condition: adds [[decl]] to [[state.processedDeclarations]]
    * @return Either Nil or a list with (1 + connectionTypes.size) elements, each of them being a list containing
    *         declarations (possibly none).
    */
  protected def applyDeclaration(container: Container, decl: Declaration)(implicit diagInterp: DiagramInterpreter, state: DiagramState): List[List[Declaration]]

  // internal things

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

  final override def acceptDiagram(diagram: Term): Option[List[MPath]] = diagram match {
    case SimpleDiagram(`operatorDomain`, modulePaths) => Some(modulePaths)
    case SimpleDiagram(dom, _) if dom != operatorDomain =>
      // todo check for implicit morphism from `domain` to actual domain
      None
    case _ => None
  }
  final override def submitDiagram(newModules: List[MPath]): Term = SimpleDiagram(operatorCodomain, newModules)

  final private def applyContainerPath(path: ContentPath): ContentPath = {
    ???
  }

  final override protected def applyModule(inModule: Module)(implicit interp: DiagramInterpreter, state: DiagramState): Module = {
    applyContainer(inModule)
    state.processedElements(inModule.path) match {
      case m: Module => m
      case _ => throw new GeneralError("LinearOperator transformed module into an element of other type")
    }
  }

  // beware of the ordering of the "with" (not commutative)
  // ContainerElement[Declaration] declares getDeclarations: List[Declaration] and we want that
  // (ContentElement.getDeclarations: List[ContentElement] is too weak)
  // type Container = ContentElement with ContainerElement[Declaration]

  type Container = ModuleOrLink

  final protected def applyContainer(inContainer: Container)(implicit interp: DiagramInterpreter, state: DiagramState): Unit = {
    // variable naming conventions:
    //
    // inModule: the module from the input diagram
    // outContainer: the module to which the inModule is being transformed

    if (state.processedElements.contains(inContainer.path)) {
      return
    }

    val inLinearState = state.initAndRegisterNewLinearState(inContainer)

    val outContainer: Container = {

      inContainer match {
        case inModule: Module =>
          val outPath = applyModulePath(inContainer.path.toMPath)
          val outModule = inModule match {
            case thy: Theory =>
              Theory.empty(outPath.doc, outPath.name, thy.meta)

            case view: View =>
              applyModule(state.inputToplevelModules(view.from.toMPath))
              applyModule(state.inputToplevelModules(view.to.toMPath))

              inLinearState.inherit(state.getLinearState(view.to.toMPath))

              View(
                outPath.doc, outPath.name,
                OMMOD(applyModulePath(view.from.toMPath)), OMMOD(applyModulePath(view.to.toMPath)),
                view.isImplicit
              )
          }

          state.seenModules += inModule.path

          if (state.inputToplevelModules.contains(inModule.path)) {
            interp.addToplevelResult(outModule)
          }

          outModule

        case s: Structure => s.tp match {
          case Some(OMMOD(structureDomain)) =>
            applyContainer(interp.ctrl.getAs(classOf[Module], structureDomain))

            // inherit linear state from module where structure is declared
            inLinearState.inherit(state.getLinearState(s.home.toMPath))

            new Structure(
              home = OMMOD(applyModulePath(s.path.module)),
              name = s.name,
              s.tpC, s.dfC,
              s.isImplicit, s.isTotal
            )
          case _ => ???
        }
      }
    }

    state.processedElements.put(inContainer.path, outContainer)
    interp.add(outContainer)

    val connectionModules: List[Module] = inContainer match {
      // connection modules are currently only created for theories
      case thy: Theory => connectionTypes.map(conn => {
        val path = conn.applyModulePath(thy.path)

        val (from, to) = conn match {
          case InToOutMorphismConnectionType() =>
            (inContainer.path, outContainer.path)
          case OutToInMorphismConnectionType() =>
            (outContainer.path, inContainer.path)
        }
        View(path.doc, path.name, OMMOD(from.toMPath), OMMOD(to.toMPath), isImplicit = false)
      })

      case _ => Nil // connection modules get only created between in theory <-> out theory
    }
    connectionModules.foreach(interp.add)
    if (state.inputToplevelModules.contains(inContainer.modulePath)) {
      connectionModules.foreach(interp.addToplevelResult)
    }

    inContainer.getDeclarations.foreach(decl => {
      inLinearState.registerDeclaration(decl)
      val newDeclarations: List[List[Declaration]] = decl match {
        case Include(includeData) => applyIncludeData(inContainer, includeData)
        case s: Structure => applyStructure(inContainer, s)
        case decl: Declaration => applyDeclaration(inContainer, decl)(interp, state)
      }

      if (inContainer.isInstanceOf[Theory] && newDeclarations.nonEmpty && newDeclarations.size != 1 + connectionTypes.size) {
        // todo: reactivate later
        // throw GeneralError("Linear operator returned incompatible number of output declaration lists")
      }

      // below, be cautious about using newDeclarations.{head,tail} as newDeclarations may be empty
      //newDeclarations.headOption.foreach(_.foreach(outContainer.add(_)))
      //newDeclarations.headOption.foreach(_.foreach(interp.add))

      newDeclarations.flatten.foreach(interp.add)
      /*newDeclarations.drop(1).zip(connectionModules).map {
        case (newDecls, conn) => newDecls.foreach(conn.add(_))
      }*/
    })

    state.processedElements.put(inContainer.path.asInstanceOf[Path], outContainer)
  }

  final private def applyStructure(container: Container, s: Structure)(implicit interp: DiagramInterpreter, state: DiagramState): List[List[Declaration]] = {
    applyContainer(s)
    
    connectionTypes.foreach()
    List(List(state.processedElements(s.path).asInstanceOf[Structure]))
  }

  /**
    * modifies [[state.processedDeclarations]] accordingly
    * @param module
    * @param include
    * @param interp
    * @param state
    * @return
    */
  final private def applyIncludeData(container: Container, include: IncludeData)(implicit interp: DiagramInterpreter, state: DiagramState): List[List[Declaration]] = {
    val ctrl = interp.ctrl

    if (include.args.nonEmpty) {
      ???
    }

    val newFroms: List[MPath] = include.from match {
      case `operatorDomain` =>
        // TODO this is wrong on connections
        operatorCodomain.toMPath :: connectionTypes.map(_ => operatorCodomain.toMPath)

      case from if state.seenModules.contains(from) =>
        applyModule(interp.ctrl.getAs(classOf[Module], from))

        if (container.isInstanceOf[Theory]) {
          state.getLinearState(container.path).inherit(state.getLinearState(from))
        }

        applyModulePath(from) :: connectionTypes.map {
          case InToOutMorphismConnectionType() =>
            from
          case OutToInMorphismConnectionType() =>
            applyModulePath(from)
        }

      case from if ctrl.globalLookup.hasImplicit(OMMOD(from), OMMOD(operatorDomain)) =>
        // e.g. for a view v: ?S -> ?T and S, T both having meta theory ?meta,
        //      the view will feature an "include ?meta = OMIDENT(OMMOD(?meta))"
        //      but in general it might be something else
        //
        // todo: what to do here? add to context? just retain and hope there's an implicit morphism from from to operatorCodomain, too?
        if (!container.isInstanceOf[View]) {
          // what to do with connections?
          ???
        }
        List(from)

      case _ =>
        // theory included wasn't contained in diagram actually
        throw GeneralError(s"Unbound module in diagram: ${container.path} contains include of ${include.from} " +
          s"which is a module that is neither contained in the diagram nor included in the diagram's " +
          s"base theory (${operatorDomain}), not even by an implicit morphism instead of an inclusion.")
    }

    val newDfsRaw: Option[List[Term]] = include.df.map {
      case OMIDENT(`operatorDomain`) =>
        // todo probably wrong on connections
        OMIDENT(OMMOD(operatorCodomain)) :: connectionTypes.map(_ => OMIDENT(OMMOD(operatorCodomain)))

      case OMIDENT(OMMOD(thy)) if state.seenModules.contains(thy) =>
        List(OMIDENT(OMMOD(applyModulePath(thy))))

      case OMIDENT(thy) if ctrl.globalLookup.hasImplicit(thy, OMMOD(operatorDomain)) =>
        // e.g. for a view v: ?S -> ?T and S, T both having meta theory ?meta,
        //      the view will feature an "include ?meta = OMIDENT(OMMOD(?meta))"
        //      but in general it might be something else
        //
        // todo: what to do here? add to context? just retain and hope there's an implicit morphism from from to operatorCodomain, too?
        if (!container.isInstanceOf[View]) {
          // what to do with connections?
          ???
        }
        List(OMIDENT(thy))

      case OMMOD(dfPath) if state.seenModules.contains(dfPath) =>
        // ???: error: morphism provided as definiens to include wasn't contained in diagram
        applyModule(state.inputToplevelModules(dfPath))

        if (container.isInstanceOf[View]) {
          state.getLinearState(container.path).inherit(state.getLinearState(dfPath))
        }

        OMMOD(applyModulePath(dfPath)) :: connectionTypes.map(conn => OMMOD(conn.applyModulePath(dfPath)))

      case _ =>
        ???
    }

    val newDfs: List[Option[Term]] = newDfsRaw.map(list => list.map(Some(_))).getOrElse(
      List.fill(1 + connectionTypes.size)(None)
    )

    val outInclude = Include(
      home = OMMOD(applyModulePath(container.modulePath)),
      from = newFroms.head,
      args = Nil,
      df = newDfs.head,
      total = include.total
    )

    container match {
      // only theories feature connections so far
      case _: Theory =>
        val connectionIncludes = connectionTypes.zipWithIndex.map {
          case (conn, idx) => Include.assignment(
            home = OMMOD(conn.applyModulePath(container.modulePath)),
            from = newFroms(idx),
            df = newDfs(idx)
          )
        }

        List(outInclude) :: connectionIncludes.map(List(_))

      case _ =>
        List(List(outInclude))
    }
  }
}

/**
  * A [[LinearOperator]] that works constant-by-constant: declarations that are not [[Constant]]s (e.g.
  * [[Structure]]s) are treated appropriately.
  */
abstract class ElaborationBasedLinearOperator extends LinearOperator {
  protected def applyConstant(container: Container, c: Constant)(implicit diagInterp: DiagramInterpreter, state: LinearState): List[List[Declaration]]

  final override protected def applyDeclaration(container: Container, decl: Declaration)(implicit diagInterp: DiagramInterpreter, state: DiagramState): List[List[Declaration]] = {
    decl match {
      case c: Constant => applyConstant(container, c)(diagInterp, state.getLinearState(container.path))
      case s: Structure =>
        //s.
        ???
      case _ =>
        // do elaboration, then call applyConstant
        // diagInterp.errorCont(InvalidElement(decl, s"Linear operator ${getClass} cannot process this element " +
          //s"of u"))
        ???
    }
  }
}

/**
  * A [[LinearOperator]] that works (type, definiens)-by-(type, definiens): all declarations that are
  * not [[FinalConstant]]s (with a type) are treated appropriately and subclasses only need to implement a method
  * receiving a [[Term]] for the type component and an ''Option[Term]'' for an optional definiens.
  *
  * Moreover, for convenience the linear state is fixed to be the one from [[DefaultLinearStateOperator]].
  */
abstract class SimpleLinearOperator extends ElaborationBasedLinearOperator with DefaultLinearStateOperator {
  type SimpleConstant = (LocalName, Term, Option[Term])

  /**
    *
    * @return Either Nil or a list consisting of (1 + connectionTypes.size) lists, each of them containing
    *         declarations (possibly none).
    *         In case the operator is not applicable on c, emit an error via diagInterp.errorHandler and
    *         return Nil.
    */
  protected def applyConstantSimple(container: Container, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit diagInterp: DiagramInterpreter, state: LinearState): List[List[SimpleConstant]]

  // helper functions to make up a nice DSL
  final protected def MainResults(decls: SimpleConstant*): List[List[SimpleConstant]] = List(decls.toList)
  final protected def MainResults(initDecls: Seq[SimpleConstant], decls: SimpleConstant*): List[List[SimpleConstant]] = MainResults((initDecls.toList ::: decls.toList) : _*)

  // Conn results are almost always morphisms, hence definiens must always be given (no Option[Term])
  final protected def ConnResults(decls: (LocalName, Term, Term)*): List[List[SimpleConstant]] =
    List(decls.map(d => (d._1, d._2, Some(d._3))).toList)

  final protected val NoResults: List[List[SimpleConstant]] = Nil

  final override protected def applyConstant(container: Container, c: Constant)(implicit diagInterp: DiagramInterpreter, state: LinearState): List[List[Declaration]] = {
    val simplifiedName: LocalName = container match {
      case _: Theory => c.name
      case v: View => c.name match {
        case LocalName(ComplexStep(mpath) :: domainSymbolName) if mpath == v.from.toMPath =>
          LocalName(domainSymbolName)
        case _ => c.name // fallback
      }
    }

    val rawTp = c.tp.getOrElse({
      diagInterp.errorCont(GeneralError(s"Operator ${getClass} not applicable on constants without type component"))
      return Nil
    })
    val rawDf = c.df

    val tp = diagInterp.ctrl.globalLookup.ExpandDefinitions(rawTp, state.skippedDeclarationPaths)
    val df = rawDf.map(diagInterp.ctrl.globalLookup.ExpandDefinitions(_, state.skippedDeclarationPaths))

    val ret = applyConstantSimple(container, c, simplifiedName, tp, df)
    if (ret.isEmpty) {
      return Nil
    } else if (ret.size != 1 + connectionTypes.size) {
      throw GeneralError("invalid return value of applyConstantSimple")
    }

    val outConstants: List[Constant] = ret.head.map {
      case (name, tp, df) =>
        if (container.isInstanceOf[View] && df.isEmpty) {
          throw GeneralError(s"applyConstant of SimpleLinearOperator subclass ${this.getClass} returned empty definiens for view declaration ${c.path}")
        }

        val newName = container match {
          case _: Theory => name
          case v: View => LocalName(v.from.toMPath) / name
        }

        new FinalConstant(
          home = OMMOD(applyModulePath(container.modulePath)),
          name = newName, alias = Nil,
          tpC = TermContainer.asParsed(tp), dfC = TermContainer.asParsed(df),
          rl = None, notC = NotationContainer.empty(), vs = c.vs
        )
    }

    val connectionConstants: List[List[Constant]] = connectionTypes.zip(ret.tail).map {
      case (conn, connectionConstants) => connectionConstants.map {
        case (name, tp, df) =>

          val assignmentName = conn match {
            case InToOutMorphismConnectionType() => LocalName(container.modulePath) / name
            case OutToInMorphismConnectionType() => LocalName(applyModulePath(container.modulePath)) / name
          }

          new FinalConstant(
            home = OMMOD(conn.applyModulePath(container.modulePath)),
            name = assignmentName, alias = Nil,
            tpC = TermContainer.asParsed(tp), dfC = TermContainer.asParsed(df),
            rl = None, notC = NotationContainer.empty(), vs = c.vs
          )
      }
    }

    outConstants :: connectionConstants
  }
}
