package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.CheckingCallback
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._

import scala.collection.mutable

class DiagramInterpreter(val ctrl: Controller, val solver: CheckingCallback, rules: RuleSet) {
  // need mutable.LinkedHashMap as it guarantees to preserve insertion order (needed for commit())
  private var transientResults : mutable.LinkedHashMap[MPath, Module] = mutable.LinkedHashMap()
  // need mutable.LinkedHashMap as it guarantees to preserve insertion order (needed for commit())
  private var transientConnections : mutable.LinkedHashMap[MPath, Module] = mutable.LinkedHashMap()

  private var _committedModules = mutable.ListBuffer[Module]()

  private var operators: List[DiagramOperator] = rules.get(classOf[DiagramOperator]).toList

  private def addToMapWithoutClash[K,V](map: mutable.Map[K, V], key: K, value: V): Unit = {
    if (map.getOrElseUpdate(key, value) != value) {
      throw new Exception("...")
    }
  }


  private def addTransaction(ctrl: Controller, elements: Iterable[StructuralElement]): Unit = {
    val addedSofar = mutable.ListBuffer[Path]()
    try {
      for (element <- elements) {
        ctrl.add(element)
        addedSofar += element.path
      }
    } catch {
      case err: api.Error =>
        addedSofar.reverse.foreach(ctrl.delete)
        throw err
    }
  }

  def commit(): Unit = {
    val transientModules = (transientResults.values ++ transientConnections.values).toList
    addTransaction(ctrl, transientModules)

    _committedModules ++= transientModules
    transientResults.clear()
    transientConnections.clear()
  }

  def committedModules: List[Module] = _committedModules.toList

  def addResult(m: Module): Unit = {
    addToMapWithoutClash(transientResults, m.path, m)
  }

  def hasResult(p: MPath): Boolean = transientResults.contains(p)

  def addConnection(m: Module): Unit = {
    addToMapWithoutClash(transientConnections, m.path, m)
  }

  def get(p: MPath): Module = {
    transientResults.getOrElse(p,
      transientConnections.getOrElse(p,
        ctrl.getAs(classOf[Module], p)
      )
    )
  }

  def apply(diag: Term): Option[Term] = diag match {
      case OMA(OMS(head), _) =>
        val matchingOp = operators.find(_.head == head).getOrElse(throw api.GeneralError(s"no diagram operator applicable to head ${head}, overall these ones were in scope: ${operators}"))

        val result = matchingOp(diag, this)(ctrl)
        commit()
        result

      case x => Some(x)
    }
}

abstract class DiagramOperator extends SyntaxDrivenRule {
  // make parent class' head function a mere field, needed to include it in pattern matching patterns (otherwise: "stable identifier required, but got head [a function!]")
  val head: GlobalName

  // need access to Controller for generative operators (i.e. most operators)
  // todo: upon error, are modules inconsistently added to controller? avoid that.
  def apply(diagram: Term, interp: DiagramInterpreter)(implicit ctrl: Controller): Option[Term]
}

abstract class FunctorialOperator extends DiagramOperator {
  protected type State = DefaultState
  def initState: State

  def applyModuleName(name: LocalName): LocalName
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
  def applyModule(m: Module)(implicit interp: DiagramInterpreter, state: State): Module

  protected class DefaultState(
                                var processedModules: mutable.Map[MPath, Module],
                                var inputModules: Map[MPath, Module]
                              )

  def applyModulePath(mpath: MPath): MPath = mpath.doc ? applyModuleName(mpath.name)

  def acceptDiagram(diagram: Term): Option[List[MPath]]
  def submitDiagram(newModules: List[MPath]): Term

  final override def apply(diagram: Term, interp: DiagramInterpreter)(implicit ctrl: Controller): Option[Term] = diagram match {
    // circumvent some MMT parsing bug
    case OMA(OMS(`head`), List(OMBINDC(_, _, t))) =>
      apply(OMA(OMS(head), t), interp)
    case OMA(OMS(`head`), List(inputDiagram)) =>
      interp(inputDiagram).flatMap(acceptDiagram) match {
        case Some(modulePaths) =>
          val state = initState
          val newModulePaths = modulePaths.map(modulePath => {
            val newModule = applyModule(interp.get(modulePath))(interp, state)

            state.processedModules.get(modulePath) match {
              case Some(`newModule`) => // ok
              case Some(m) if m != newModule =>
                throw new Exception("...")

              case None =>
                throw new Exception("...")
            }
            interp.hasResult(newModule.path)

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

  protected val connectionTypes : List[Connection] = Nil

  sealed abstract class Connection {
    def applyModuleName(name: LocalName): LocalName
    final def applyModulePath(p: MPath): MPath = p.doc ? applyModuleName(p.name)
  }
  final abstract case class InToOutMorphismConnection() extends Connection()
  final abstract case class OutToInMorphismConnection() extends Connection()

  // todo: how should this method signal *unapplicability*/partiality (i.e. error)?
  def applyDeclaration(module: Module, decl: Declaration)(implicit solver: CheckingCallback): List[Declaration]

  final override def acceptDiagram(diagram: Term): Option[List[MPath]] = diagram match {
    case SimpleDiagram(`operatorDomain`, modulePaths) => Some(modulePaths)
    case SimpleDiagram(dom, _) if dom != operatorDomain =>
      // todo check for implicit morphism from `domain` to actual domain
      None
    case _ => None
  }
  final override def submitDiagram(newModules: List[MPath]): Term = SimpleDiagram(operatorCodomain, newModules)

  final override def applyModule(module: Module)(implicit interp: DiagramInterpreter, state: State): Module = {
    state.processedModules.get(module.path).foreach(return _)
    lazy val ctrl = interp.ctrl

    val newModulePath = applyModulePath(module.path)
    val newModule = module match {
      case thy: Theory =>
        Theory.empty(newModulePath.doc, newModulePath.name, thy.meta)

      case view: View =>
        View(
          newModulePath.doc, newModulePath.name,
          OMMOD(applyModulePath(view.from.toMPath)), OMMOD(applyModulePath(view.to.toMPath)),
          view.isImplicit
        )
    }

    interp.addResult(newModule)
    state.processedModules.put(newModulePath, newModule)

    val newDeclarations = mutable.ListBuffer[Declaration]()

    module.getDeclarations.foreach {
      // IncludeData(home: Term, from: MPath, args: List[Term], df: Option[Term], total: Boolean)
      case Include(IncludeData(_, from, Nil, df, total)) =>
        val newFrom: MPath = from match {
          case `operatorDomain` =>
            operatorCodomain.toMPath

          case from if state.inputModules.contains(from) =>
            applyModule(state.inputModules(from))
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
            ???
        }

        val newDf: Option[Term] = df.map {
          case OMIDENT(`operatorDomain`) =>
            OMIDENT(OMMOD(operatorCodomain))

          case OMIDENT(thy) if ctrl.globalLookup.hasImplicit(thy, OMMOD(operatorDomain)) =>
            // e.g. for a view v: ?S -> ?T and S, T both having meta theory ?meta,
            //      the view will feature an "include ?meta = OMIDENT(OMMOD(?meta))"
            //      but in general it might be something else
            //
            // todo: what to do here? add to context? just retain and hope there's an implicit morphism from from to operatorCodomain, too?
            OMIDENT(thy)

          case OMMOD(dfPath) =>
            // ???: error: morphism provided as definiens to include wasn't contained in diagram
            applyModule(state.inputModules.getOrElse(dfPath, ???))
            OMMOD(applyModulePath(dfPath))

          case _ =>
            ???
        }

        newDeclarations += IncludeData(
          home = OMMOD(applyModulePath(module.path)),
          from = newFrom,
          args = Nil,
          df = newDf,
          total = total
        ).toStructure

      case decl: Declaration =>
        newDeclarations ++= applyDeclaration(module, decl)(interp.solver)
    }

    newModule
  }
}

abstract class ElaborationBasedLinearOperator extends LinearOperator {
  def applyConstant(module: Module, c: Constant)(implicit solver: CheckingCallback): List[Declaration]

  final override def applyDeclaration(module: Module, decl: Declaration)(implicit solver: CheckingCallback): List[Declaration] = decl match {
    case c: Constant => applyConstant(module, c)
    case _ =>
      // do elaboration, then call applyConstant
      ???
  }
}


  /*ef transformSimpleConstant(name: LocalName, tp: Term, df: Option[Term], context: Context): List[(LocalName, Term, Option[Term])]

  final override def transformConstant(c: Constant, context: Context): List[Declaration] = {
    transformSimpleConstant(c.name, c.tp.getOrElse(return Nil), c.df, context).map {
      case (newName, tp, df) =>
        new FinalConstant(
          home = OMMOD(apply(c.path.module)),
          name = newName,
          alias = Nil,
          tpC = TermContainer.asParsed(tp),
          dfC = TermContainer.asParsed(df),
          rl = None,
          notC = NotationContainer.empty(),
          vs = c.vs
        )
    }
  }
}*/
/*
trait SystematicRenaming extends NameInferrableDiagramOperator {
  protected def rename(tag: String, name: LocalName): LocalName = name.suffixLastSimple("_" +tag)
  protected final def rename(tag: String, path: GlobalName): GlobalName = apply(path.module) ? rename(tag, path.name)

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
      new SimpleLinearOperator with SystematicRenaming {

        override protected val operatorSymbol: GlobalName = opSymbol
        override protected val operatorDomain: MPath = dom
        override protected val operatorCodomain: MPath = cod

        override def transformModuleName(name: LocalName): LocalName = name.suffixLastSimple("_copy")

        override def transformSimpleConstant(name: LocalName, tp: Term, df: Option[Term], ctx: Context): List[(LocalName, Term, Option[Term])] = {

          List(
            (rename("1", name), rename("1", ctx, tp), df.map(rename("1", ctx, _))),
            (rename("2", name), rename("2", ctx, tp), df.map(rename("2", ctx, _))),
          )
        }
      }

    case _ => throw ParseError("invalid usage. correct usage: rule ...?CopyOperator <operator symbol to tie with> <domain theory OMMOD> <codomain theory OMMOD>")
  }
}
*/
object SimpleDiagram {
  private val constant = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?simple_diagram", NamespaceMap.empty)

  def apply(baseTheory: MPath, paths: List[MPath]): Term = {
    OMA(OMS(constant), OMMOD(baseTheory) :: paths.map(OMMOD(_)))
  }
  def unapply(t: Term): Option[(MPath, List[MPath])] = t match {
    case OMA(OMS(`constant`), OMMOD(baseTheory) :: pathTerms) =>
      val paths = pathTerms.collect {
        case OMMOD(path) => path
        case _ => return None
      }
      Some((baseTheory, paths))

    case _ => None
  }
}
