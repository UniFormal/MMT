package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.notations.NotationContainer
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.{ComplexStep, GeneralError, LocalName, MPath}

trait ModulePathTransformer {
  protected def applyModuleName(name: LocalName): LocalName

  final protected def applyModulePath(mpath: MPath): MPath = {
    if (mpath == mpath.mainModule) {
      mpath.doc ? applyModuleName(mpath.name)
    } else {
      val newMPath = applyModulePath(mpath.mainModule)
      newMPath.doc ? LocalName(newMPath.name.steps ::: mpath.name.drop(1))
    }
  }
}

abstract class FunctorialOperator extends DiagramOperator with FunctorialTransformer {
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

abstract class LinearOperator extends FunctorialOperator with LinearModuleTransformer {
  final override def acceptDiagram(diagram: Term): Option[List[MPath]] = diagram match {
    case SimpleDiagram(`operatorDomain`, modulePaths) => Some(modulePaths)
    case SimpleDiagram(dom, _) if dom != operatorDomain =>
      // todo check for implicit morphism from `domain` to actual domain
      None
    case _ => None
  }
  final override def submitDiagram(newModules: List[MPath]): Term = SimpleDiagram(operatorCodomain, newModules)
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
