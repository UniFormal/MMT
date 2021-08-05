package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.{ContainerElement, ContentPath, ErrorHandler, GeneralError, GlobalName, MPath, Path, RuleSet, StructuralElement}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{AbstractTheory, Link, Module, Theory}
import info.kwarc.mmt.api.objects.{Context, OMBINDC, OMMOD, StatelessTraverser, Term, Traverser}
import info.kwarc.mmt.api.symbols.DerivedModule
import info.kwarc.mmt.api.uom.SimplificationUnit

import scala.collection.mutable


/**
  *
  * todo: added results/connections are buffered until commit() has been called. If operators invoke certain
  *       controller functions on buffered modules, this can lead to errors or inconsistent results.
  *       As long as operators don't query the controller/simplifier/... for buffered modules, it's okay.
  *       Long-term: either drop this buffering or implement staged controllers (an idea FR once had)
  */
class DiagramInterpreter(private val interpreterContext: Context, private val rules: RuleSet, val ctrl: Controller, val errorCont: ErrorHandler) {

  private val transientPaths: mutable.ListBuffer[Path] = mutable.ListBuffer()

  // need mutable.LinkedHashMap as it guarantees to preserve insertion order (needed for commit())
  private val transientToplevelResults : mutable.LinkedHashMap[MPath, Module] = mutable.LinkedHashMap()

  def toplevelResults: List[Module] = transientToplevelResults.values.toList

  private val operators: Map[GlobalName, NamedDiagramOperator] = rules.get(classOf[NamedDiagramOperator]).map(op =>
    (op.head, op)
  ).toMap

  def add(elem: StructuralElement): Unit = {
    ctrl.add(elem)
    transientPaths += elem.path
  }

  def endAdd(elem: ContainerElement[_]): Unit = {
    ctrl.endAdd(elem)
  }

  /**
    * [[add()]] plus registering as a toplevel result
    * @param m
    */
  def addToplevelResult(m: Module): Unit = {
    add(m)
    transientToplevelResults.put(m.path, m)
  }

  def hasToplevelResult(m: MPath): Boolean = transientToplevelResults.contains(m)

  def get(p: MPath): Module = ctrl.getAs(classOf[Module], p)

  /**
    * Hack to remove unbound constants that MMT introduces for some reason in diagram expressions
    *
    * TODO: resolve this bug together with Florian
    */
  private val removeOmbindc: Term => Term = {
    val traverser = new StatelessTraverser {
      override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
        case OMBINDC(_, _, List(scope)) => Traverser(this, scope)
        case t => Traverser(this, t)
      }
    }

    traverser.apply(_, Context.empty)
  }

  /**
    * Tries to fully evaluate a diagram expression and to delegate to [[NamedDiagramOperator]]s found
    * in [[interpreterContext]].
    *
    * @param t The input diagram expression
    */
  def apply(t: Term): Option[Diagram] = {
    object HasHead {
      def unapply(t: Term): Option[ContentPath] = t.head
    }

    removeOmbindc(t) match {
      // nothing to do, already an encoded diagram
      case DiagramTermBridge(diag) => Some(diag)

      case OMMOD(diagramDerivedModule @ m) if ctrl.getO(m).exists(_.isInstanceOf[DerivedModule]) =>
        Some(InstallDiagram.parseOutput(diagramDerivedModule)(ctrl.library))

      // otherwise coerce atomic reference to modules to single diagrams
      // (note: these modules do not need to even exist yet; use case is to specify names of output
      //  diagrams for diagram operators)
      case OMMOD(m) => Some(Diagram.singleton(m))

      case operatorExpression @ HasHead(p: GlobalName) if operators.contains(p) =>
        // no simplification needed at this point
        // the called operator may still simplify arguments on its own later on
        val matchingOp = operators(p)
        val opResult = matchingOp(operatorExpression)(this, ctrl)
        opResult.flatMap(apply)

      case diag =>
        val simplifiedDiag = {
          val su = SimplificationUnit(
            context = interpreterContext,
            expandDefinitions = true,
            fullRecursion = true
          )

          // first expand all definitions as simplifier doesn't seem to definition-expand in cases like
          // t = OMA(OMS(?s), args) // here ?s doesn't get definition-expanded
          removeOmbindc(ctrl.simplifier(ctrl.library.ExpandDefinitions(diag, _ => true), su))
        }

        if (simplifiedDiag == diag) {
          throw GeneralError(s"Cannot interpret diagram expression below, which is already fully simplified:\n" +
            s"`$simplifiedDiag`\n" +
            s"Is the operator you are trying to apply in-scope in the meta theory you specified for the diagram " +
            s"structural feature (`diagram d : ?meta := ...`)? " +
            "Does the operator (a Scala object, not class!) have the correct `head` field?")
        }

        apply(simplifiedDiag)
    }
  }
}