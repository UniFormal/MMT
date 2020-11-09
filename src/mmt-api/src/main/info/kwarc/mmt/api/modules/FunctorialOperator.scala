package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api.{ComplexStep, GeneralError, GlobalName, LocalName, MPath, ParametricRule, ParseError, Rule}
import info.kwarc.mmt.api.checking.CheckingCallback
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects.{Context, OMA, OMBINDC, OMID, OMIDENT, OMMOD, OMS, Term}
import info.kwarc.mmt.api.symbols.{Constant, Declaration, FinalConstant, Include, IncludeData, OMSReplacer, TermContainer}

import scala.collection.mutable

abstract class FunctorialOperator extends DiagramOperator {
  protected type State = DefaultState
  protected def initState(diagram: Term, modulePaths: List[MPath], interp: DiagramInterpreter): State = {
    val modules: Map[MPath, Module] = modulePaths.map(p => (p, interp.ctrl.getAs(classOf[Module], p))).toMap
    new DefaultState(modules)
  }

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
  protected def applyModule(m: Module)(implicit interp: DiagramInterpreter, state: State): Module

  protected class DefaultState(var inputModules: Map[MPath, Module]) {
    var processedModules: mutable.Map[MPath, Module] = mutable.Map()
  }

  protected def applyModulePath(mpath: MPath): MPath = mpath.doc ? applyModuleName(mpath.name)

  protected def acceptDiagram(diagram: Term): Option[List[MPath]]
  protected def submitDiagram(newModules: List[MPath]): Term

  final override def apply(diagram: Term, interp: DiagramInterpreter)(implicit ctrl: Controller): Option[Term] = diagram match {
    // circumvent some MMT parsing bug
    case OMA(OMS(`head`), List(OMBINDC(_, _, t))) =>
      apply(OMA(OMS(head), t), interp)
    case OMA(OMS(`head`), List(inputDiagram)) =>
      interp(inputDiagram).flatMap(acceptDiagram) match {
        case Some(modulePaths) =>
          val state = initState(diagram, modulePaths, interp)
          val newModulePaths = modulePaths.map(modulePath => {
            val newModule = applyModule(interp.get(modulePath))(interp, state)

            state.processedModules.get(modulePath) match {
              case Some(`newModule`) => // ok
              case Some(m) if m != newModule =>
                throw new Exception("...")

              case None =>
                throw new Exception("...")
            }
            if (!interp.hasResult(newModule.path)) {
              throw GeneralError("diagram operators' applyModule should insert resulting module to DiagramInterpreter")
            }

            modulePath
          })
          // todo: instead get new module paths from interp?
          Some(submitDiagram(newModulePaths))

        case None => None
      }

    case _ => None
  }
}

abstract class LinearOperator extends FunctorialOperator {
  protected val operatorDomain: MPath
  protected val operatorCodomain: MPath

  protected val connectionTypes : List[ConnectionType] = Nil

  abstract class ConnectionType {
    def applyModuleName(name: LocalName): LocalName
    final def applyModulePath(p: MPath): MPath = p.doc ? applyModuleName(p.name)
  }
  abstract case class InToOutMorphismConnectionType() extends ConnectionType()
  abstract case class OutToInMorphismConnectionType() extends ConnectionType()

  // todo: how should this method signal *unapplicability*/partiality (i.e. error)?
  protected def applyDeclaration(module: Module, decl: Declaration)(implicit solver: CheckingCallback): List[List[Declaration]]

  final override def acceptDiagram(diagram: Term): Option[List[MPath]] = diagram match {
    case SimpleDiagram(`operatorDomain`, modulePaths) => Some(modulePaths)
    case SimpleDiagram(dom, _) if dom != operatorDomain =>
      // todo check for implicit morphism from `domain` to actual domain
      None
    case _ => None
  }
  final override def submitDiagram(newModules: List[MPath]): Term = SimpleDiagram(operatorCodomain, newModules)

  // inModule: the module from the input diagram
  // outModule: the module to which the inModule is being transformed
  final override protected def applyModule(inModule: Module)(implicit interp: DiagramInterpreter, state: State): Module = {
    state.processedModules.get(inModule.path).foreach(return _)

    val outModule: Module = {
      val newModulePath = applyModulePath(inModule.path)
      inModule match {
        case thy: Theory =>
          Theory.empty(newModulePath.doc, newModulePath.name, thy.meta)

        case view: View =>
          View(
            newModulePath.doc, newModulePath.name,
            OMMOD(applyModulePath(view.from.toMPath)), OMMOD(applyModulePath(view.to.toMPath)),
            view.isImplicit
          )
      }
    }

    interp.addResult(outModule)
    state.processedModules.put(inModule.path, outModule)

    val connectionModules: List[Module] = inModule match {
      case _: View => Nil // connection modules get only created between in theory <-> out theory
      case _: Theory => connectionTypes.map(conn => {
        val path = conn.applyModulePath(inModule.path)

        val (from, to) = conn match {
          case InToOutMorphismConnectionType() =>
            (inModule.path, outModule.path)
          case OutToInMorphismConnectionType() =>
            (outModule.path, inModule.path)
        }
        View(path.doc, path.name, OMMOD(from), OMMOD(to), isImplicit = false)
      })
    }

    connectionModules.foreach(interp.addConnection)

    inModule.getDeclarations.foreach(decl => {
      val newDeclarations = decl match {
        case Include(includeData) => applyInclude(inModule, includeData)
        case decl: Declaration => applyDeclaration(inModule, decl)(interp.solver)
      }

      if (inModule.isInstanceOf[Theory] && newDeclarations.size != 1 + connectionTypes.size) {
        throw GeneralError("Linear operator returned incompatible number of output declaration lists")
      }

      newDeclarations.head.foreach(outModule.add(_))

      newDeclarations.tail.zip(connectionModules).map {
        case (newDecls, conn) => newDecls.foreach(conn.add(_))
      }
    })

    outModule
  }

  final private def applyInclude(module: Module, include: IncludeData)(implicit interp: DiagramInterpreter, state: State): List[List[Declaration]] = {
    val ctrl = interp.ctrl

    if (include.args.nonEmpty) {
      ???
    }

    val newFroms: List[MPath] = include.from match {
      case `operatorDomain` =>
        // TODO this is wrong on connections
        operatorCodomain.toMPath :: connectionTypes.map(_ => operatorCodomain.toMPath)

      case from if state.inputModules.contains(from) =>
        applyModule(state.inputModules(from))
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
        if (!module.isInstanceOf[View]) {
          // what to do with connections?
          ???
        }
        List(from)

      case _ =>
        // theory included wasn't contained in diagram actually
        throw GeneralError("diagram possessed theory that included another theory that wasn't in the diagram to begin with")
    }

    val newDfsRaw: Option[List[Term]] = include.df.map {
      case OMIDENT(`operatorDomain`) =>
        // todo probably wrong on connections
        OMIDENT(OMMOD(operatorCodomain)) :: connectionTypes.map(_ => OMIDENT(OMMOD(operatorCodomain)))

      case OMIDENT(thy) if ctrl.globalLookup.hasImplicit(thy, OMMOD(operatorDomain)) =>
        // e.g. for a view v: ?S -> ?T and S, T both having meta theory ?meta,
        //      the view will feature an "include ?meta = OMIDENT(OMMOD(?meta))"
        //      but in general it might be something else
        //
        // todo: what to do here? add to context? just retain and hope there's an implicit morphism from from to operatorCodomain, too?
        if (!module.isInstanceOf[View]) {
          // what to do with connections?
          ???
        }
        List(OMIDENT(thy))

      case OMMOD(dfPath) =>
        // ???: error: morphism provided as definiens to include wasn't contained in diagram
        applyModule(state.inputModules.getOrElse(dfPath, throw GeneralError(
          "morphism that was mentioned in include definiens in diagram isn't itself contained in diagram"
        )))
        OMMOD(applyModulePath(dfPath)) :: connectionTypes.map(conn => OMMOD(conn.applyModulePath(dfPath)))

      case _ =>
        ???
    }

    val newDfs: List[Option[Term]] = newDfsRaw.map(list => list.map(Some(_))).getOrElse(
      List.fill(1 + connectionTypes.size)(None)
    )

    val outInclude = Include(
      home = OMMOD(applyModulePath(module.path)),
      from = newFroms.head,
      args = Nil,
      df = newDfs.head,
      total = include.total
    )

    module match {
      case _: View =>
        List(List(outInclude))

      case _: Theory =>
        val connectionIncludes = connectionTypes.zipWithIndex.map {
          case (conn, idx) => Include.assignment(
            home = OMMOD(conn.applyModulePath(module.path)),
            from = newFroms(idx),
            df = newDfs(idx)
          )
        }

        List(outInclude) :: connectionIncludes.map(List(_))
    }
  }
}

abstract class ElaborationBasedLinearOperator extends LinearOperator {
  protected def applyConstant(module: Module, c: Constant)(implicit solver: CheckingCallback): List[List[Declaration]]

  final override protected def applyDeclaration(module: Module, decl: Declaration)(implicit solver: CheckingCallback): List[List[Declaration]] = decl match {
    case c: Constant => applyConstant(module, c)
    case _ =>
      // do elaboration, then call applyConstant
      ???
  }
}

abstract class SimpleLinearOperator extends ElaborationBasedLinearOperator {
  protected def applyConstantSimple(module: Module, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit solver: CheckingCallback): List[List[(LocalName, Term, Option[Term])]]

  final override protected def applyConstant(module: Module, c: Constant)(implicit solver: CheckingCallback): List[List[Declaration]] = {
    val simplifiedName: LocalName = module match {
      case _: Theory => c.name
      case v: View => c.name match {
        case LocalName(ComplexStep(mpath) :: domainSymbolName) if mpath == v.from.toMPath =>
          LocalName(domainSymbolName)
        case _ => c.name // fallback
      }
    }

    val ret = applyConstantSimple(module, c, simplifiedName, c.tp.get, c.df)
    if (ret.size != 1 + connectionTypes.size) {
      throw GeneralError("invalid return value of applyConstantSimple")
    }

    val outConstants: List[Constant] = ret.head.map {
      case (name, tp, df) =>
        val newName = module match {
          case _: Theory => name
          case v: View => LocalName(v.from.toMPath) / name
        }

        new FinalConstant(
          home = OMMOD(applyModulePath(module.path)),
          name = newName, alias = Nil,
          tpC = TermContainer.asParsed(tp), dfC = TermContainer.asParsed(df),
          rl = None, notC = NotationContainer.empty(), vs = c.vs
        )
    }

    val connectionConstants: List[List[Constant]] = connectionTypes.zipWithIndex.map {
      case (conn, idx) =>
        ret(idx).map {
          case (name, tp, df) =>

            val assignmentName = conn match {
              case InToOutMorphismConnectionType() => LocalName(module.path) / name
              case OutToInMorphismConnectionType() => LocalName(applyModulePath(module.path)) / name
            }

            new FinalConstant(
              home = OMMOD(conn.applyModulePath(module.path)),
              name = assignmentName, alias = Nil,
              tpC = TermContainer.asParsed(tp), dfC = TermContainer.asParsed(df),
              rl = None, notC = NotationContainer.empty(), vs = c.vs
            )
        }
    }

    outConstants :: connectionConstants
  }
}


trait SystematicRenaming extends FunctorialOperator {
  protected def rename(tag: String, name: LocalName): LocalName = name.suffixLastSimple("_" +tag)
  protected final def rename(tag: String, path: GlobalName): GlobalName =
    applyModulePath(path.module) ? rename(tag, path.name)

  protected def rename(tag: String, ctx: Context, term: Term): Term = {
    new OMSReplacer {
      override def replace(p: GlobalName): Option[Term] = ctx.collectFirst {
        case vd if vd.name == LocalName(ComplexStep(p.module) :: p.name) =>
          OMS(rename(tag, p))
      }
    }.apply(term, ctx)
  }
}

object CopyOperator extends ParametricRule {
  override def apply(controller: Controller, home: Term, args: List[Term]): Rule = args match {
    case List(OMS(opSymbol), OMMOD(dom), OMMOD(cod)) =>
      new SimpleLinearOperator /*with SystematicRenaming */{

        override val head: GlobalName = opSymbol
        override protected val operatorDomain: MPath = dom
        override protected val operatorCodomain: MPath = cod

        override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_copy")

        private val firstCopyConn = new InToOutMorphismConnectionType {
          override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_copy1")
        }
        private val secondCopyConn = new InToOutMorphismConnectionType {
          override def applyModuleName(name: LocalName): LocalName = name.suffixLastSimple("_copy2")
        }

        override val connectionTypes: List[ConnectionType] = List(firstCopyConn, secondCopyConn)

        override def applyConstantSimple(module: Module, c: Constant, name: LocalName, tp: Term, df: Option[Term])(implicit solver: CheckingCallback): List[List[(LocalName, Term, Option[Term])]] = {

          List(
            List(
              (name.suffixLastSimple("1"), tp, df),
              (name.suffixLastSimple("2"), tp, df)
            ),
            List((name, tp, Some(OMID(firstCopyConn.applyModulePath(module.path) ? name.suffixLastSimple("1"))))),
            List((name, tp, Some(OMID(firstCopyConn.applyModulePath(module.path) ? name.suffixLastSimple("2")))))
          )
        }
      }

    case _ => throw ParseError("invalid usage. correct usage: rule ...?CopyOperator <operator symbol to tie with> <domain theory OMMOD> <codomain theory OMMOD>")
  }
}