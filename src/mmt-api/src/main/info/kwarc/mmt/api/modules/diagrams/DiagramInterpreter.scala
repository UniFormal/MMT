package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Module
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.DerivedModule
import info.kwarc.mmt.api.uom.SimplificationUnit
import info.kwarc.mmt.api._

import scala.collection.mutable

/**
  * Evaluator that fully evaluates diagram expressions to [[Diagram Diagrams]], i.e.,
  * effectively lists of [[MPath module paths]].
  * The main method for evaluation is [[apply()]].
  *
  * By design, evaluation of diagram expressions is not side-effect free. Instead,
  * it is standard for such evaluations to add new [[Module]]s to the [[Controller]].
  *
  * Instances of this class are mainly used in
  *
  *  - [[InstallDiagram]]: used to evaluate diagram expressions occurring "top-level" in
  *    derived module declarations for diagrams)
  *  - as arguments passed throughout interfaces of [[DiagramOperator]] and [[LinearFunctor]]s for these reasons:
  *    - to allow diagram operators to add modules (via [[add()]] and [[endAdd()]])
  *    - to allow diagram operators to access the [[Controller]] for reasons besides adding modules (via [[ctrl]])
  *    - to report errors (via [[errorCont]]) and to evaluate sub-diagram expressions (needed for some higher-order
  *    diagram operators)
  *
  * @param interpreterContext The context in which the evaluation should run. In particular,
  *                           the set of [[NamedDiagramOperator]]s that can be evaluated in diagram
  *                           expressions is effectively taken from this context.
  *                           (NB: recall that [[NamedDiagramOperator]]s are rules, after all,
  *                            thus can appear in contexts)
  *
  * @param errorCont Error handler used by diagram operators to report any errors (incl.
  *                  being undefined, i.e., not applicable, on certain diagrams, modules, or constants)
  *
  * todo: added results/connections are buffered until commit() has been called. If operators invoke certain
  *       controller functions on buffered modules, this can lead to errors or inconsistent results.
  *       As long as operators don't query the controller/simplifier/... for buffered modules, it's okay.
  *       Long-term: either drop this buffering or implement staged controllers (an idea FR once had)
  */
class DiagramInterpreter(val ctrl: Controller, private val interpreterContext: Context, val errorCont: ErrorHandler) {

  private val transientPaths: mutable.ListBuffer[Path] = mutable.ListBuffer()

  // need mutable.LinkedHashMap as it guarantees to preserve insertion order (needed for commit())
  private val transientToplevelResults : mutable.LinkedHashMap[MPath, Module] = mutable.LinkedHashMap()

  def toplevelResults: List[Module] = transientToplevelResults.values.toList

  val operators: Map[GlobalName, NamedDiagramOperator] =
    RuleSet
      .collectRules(ctrl, interpreterContext).get(classOf[NamedDiagramOperator])
      .map(op => (op.head, op))
      .toMap

  def add(elem: StructuralElement): Unit = {
    ctrl.add(elem)
    transientPaths += elem.path
  }

  def endAdd(elem: ContainerElement[_]): Unit = {
    ctrl.endAdd(elem)
  }

  /**
    * [[add()]] plus registering as a toplevel result
    */
  def addToplevelResult(m: Module): Unit = {
    add(m)
    transientToplevelResults.put(m.path, m)
  }

  def hasToplevelResult(m: MPath): Boolean = transientToplevelResults.contains(m)

  def get(p: MPath): Module = ctrl.getAs(classOf[Module], p)

  /**
    * Fully evaluates a diagram expression and returns a [[Diagram]] of [[MPath module paths]] upon success.
    */
  def apply(t: Term): Option[Diagram] = {
    object HasHead {
      def unapply(t: Term): Option[ContentPath] = t.head
    }
    /**
      * Hack to remove unbound constants that MMT introduces for some reason in diagram expressions
      * TODO: resolve this bug together with Florian
      */
    def removeOmbindc(t: Term): Term = {
      val traverser = new StatelessTraverser {
        override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
          case OMBINDC(_, _, List(scope)) => Traverser(this, scope)
          case t => Traverser(this, t)
        }
      }

      traverser.apply(t, Context.empty)
    }

    removeOmbindc(t) match {
      // already a fully evaluated diagram
      case DiagramTermBridge(diag) => Some(diag)

      // a reference to top-level diagram declaration (that hopefully has already been evaluated and whose
      // output stored)
      case OMMOD(diagramDerivedModule @ m) if ctrl.getO(m).exists(_.isInstanceOf[DerivedModule]) =>
        Some(InstallDiagram.parseOutput(diagramDerivedModule)(ctrl.library))

      // evaluate remaining references to modules as singleton diagrams
      // (this behavior serving as syntactic sugar to spare people from writing singleton diagrams all over
      //  the place in diagram expressions)
      case OMMOD(m) => Some(Diagram.singleton(m))

      // application of a diagram operator
      case operatorExpression @ HasHead(p: GlobalName) if operators.contains(p) =>
        // Beware that we do not simplify the input term; in particular diagram operators are expected
        // to simplify their own arguments if needed.
        val opResult = operators(p)(operatorExpression)(this, ctrl)
        opResult.flatMap(apply)

      // if everything else fails, run simplification and try again (via recursion)
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

        // simplification was no-op, avoid infinite recursion
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