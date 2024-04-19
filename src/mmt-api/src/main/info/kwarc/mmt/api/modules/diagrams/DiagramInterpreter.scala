package info.kwarc.mmt.api.modules.diagrams

/**
  * Evaluation of diagrams, both top-level (with the [[InstallDiagram]] derived module
  * declaration) and on terms (with the [[DiagramInterpreter]]).
  *
  * See [[DiagramInterpreter.apply()]] for the syntax of diagram expressions.
  */

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.Module
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols.{DerivedModule, ModuleLevelFeature}
import info.kwarc.mmt.api.uom.{SimplificationEnvironment, SimplificationUnit}
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.ExtendedCheckingEnvironment
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.notations.Marker
import info.kwarc.mmt.api.presentation.ConsoleWriter
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.web.SyntaxPresenterServer

import scala.collection.mutable

/**
  * Module-level structural feature for (i) [[DiagramInterpreter evaluating diagram expressions]]
  * and for (ii) saving resulting diagram.
  *
  * Usage in MMT surface syntax is as below. There, `diagexpr` is evaluated using [[DiagramInterpreter]]
  * in context `?meta` and after elaboration the output [[Diagram]] (or [[Diagram.empty]] on failure)
  * is stored in `diag.dfC.normalized.`
  * See also [[InstallDiagram.saveOutput]] and [[InstallDiagram.parseOutput]].
  *
  * '''
  * diagram diag : ?meta = diagexpr
  * '''
  *
  * For debugging, all elaborations of this structural feature print to stdout the syntax presentation of
  * the diagram that resulted from evaluation on stdout.
  * If needed, this feature may be hidden under a log flag in the future; but note this feature proved
  * to be tremendously helpful in the past.
  */
class InstallDiagram extends ModuleLevelFeature(InstallDiagram.feature) {
  override def getHeaderNotation: List[Marker] = Nil
  // checking in advance doesn't make sense here -- we need to evaluate (in [[modules]]) anyway to
  // detect all errors
  def check(dm: DerivedModule)(implicit env: ExtendedCheckingEnvironment): Unit = {}

  override def modules(dm: DerivedModule, rules: Option[RuleSet], env: SimplificationEnvironment): List[Module] = {
    val df = dm.dfC.normalized.getOrElse {
      env.errorCont(InvalidElement(dm, "definiens required, did you perhaps type = instead of :=?)"))
      return Nil
    }

    val diagInterp = new DiagramInterpreter(controller, dm.getInnerContext, env.errorCont)

    diagInterp(df) match {
      case Some(outputDiagram) =>
        InstallDiagram.saveOutput(dm.path, outputDiagram)(controller.library)

        // This contains all toplevel results (instead of merely outputDiagram.modules, which
        // potentially has less results, e.g. because of a focussed ZippingOperator)
        val outputModules = diagInterp.toplevelResults

        // syntax-present all modules for debugging
        outputModules.foreach(controller.presenter(_)(ConsoleWriter)) // use outputModules here, not diagInterp.toplevelResults
        // TODO: investigate why they differ
        // TODO: use log instead of println and ConsoleWriter here
        println(s"""

${this.getClass.getSimpleName} debug
-------------------------------------
input: $df
operators in scope: ${diagInterp.operators.map(_.getClass.getSimpleName).mkString(", ")}

all output    : see above
primary output: see ${SyntaxPresenterServer.getURIForDiagram(URI("http://localhost:8080"), dm.path)}, or here:
                $outputDiagram
-------------------------------------

""")

        outputModules

      case None =>
        InstallDiagram.saveOutput(dm.path, Diagram.empty)(controller.library)
        env.errorCont(InvalidElement(dm, "could not evaluate diagram expressions, see errors above"))
        Nil
    }
  }
}

object InstallDiagram {
  val feature: String = "diagram"

  /**
    * Saves a diagram into a diagram module.
    * @param dm Path to the diagram module (i.e., an instance of the [[InstallDiagram]] structural feature)
    */
  def saveOutput(dm: MPath, diagram: Diagram)(implicit library: Lookup): Unit = {
    library.getModule(dm).dfC.normalized = Some(diagram.toTerm)
  }

  /**
    * Parses the stored output of a previously elaborated [[InstallDiagram]] structural feature.
    *
    * @throws GetError if the module referenced by `diagPath` cannot be found
    * @throws InvalidElement if the module doesn't correspond to a usage of the [[InstallDiagram]] structural feature
    * @throws InvalidElement if the module hasn't been elaborated before
    * @return All module entries of the output diagram (as were the result of elaboration before).
    */
  def parseOutput(dm: MPath)(implicit library: Lookup): Diagram = {
    val diagModule = library.get(dm) match {
      case diagModule: DerivedModule if diagModule.feature == InstallDiagram.feature => diagModule
      case s => throw InvalidElement(s, s"referenced module `$s` is not an instance of the " +
        s"${InstallDiagram.feature} structural feature")
    }

    diagModule.dfC.normalized match {
      case Some(DiagramTermBridge(diag)) => diag
      case Some(t) => throw InvalidObject(t, s"not a valid output in dfC of ${InstallDiagram.feature} structural feature")

      case None =>
        throw InvalidElement(diagModule, s"module `$dm` references an instance of the ${InstallDiagram.feature} " +
          s"structural feature but `.dfC.normalized` is empty. The most likely error is that elaboration hasn't been " +
          s"run on that module yet. Have you typechecked the file containing the module yet? " +
          s"You need to typecheck/elaborate ${InstallDiagram.feature} structural features in every new MMT session " +
          s"because their output isn't persisted to OMDoc yet.")
    }
  }
}

/**
  * Evaluator that fully evaluates diagram expressions to [[Diagram Diagrams]], i.e.,
  * effectively lists of [[MPath module paths]].
  * The main method for evaluation is [[apply()]] and it accept a [[Term]]. The syntax
  * of terms that are valid diagram expression is documented at [[apply()]].
  *
  * By design, evaluation of diagram expressions is not side-effect free. Instead,
  * it is standard for such evaluations to add new [[Module]]s to the [[Controller]].
  *
  * Instances of this class are mainly used in
  *
  *  - [[InstallDiagram]]: used to evaluate diagram expressions occurring top-level as the definiens
  *    of a diagram module (see the [[InstallDiagram]] structural feature)
  *  - as arguments passed throughout interfaces of [[UnaryOperator]] and [[LinearFunctor]]s for these reasons:
  *    - to allow diagram operators to add modules (via [[add]] and [[endAdd]])
  *    - to allow diagram operators to access the [[Controller]] for reasons besides adding modules (via [[ctrl]])
  *    - to report errors (via [[errorCont]]) and to evaluate sub-diagram expressions (needed for some higher-order
  *    diagram operators)
  *
  * @param interpreterContext The context in which the evaluation should run. In particular,
  *                           the set of [[NamedOperator]]s that can be evaluated in diagram
  *                           expressions is effectively taken from this context.
  *                           (NB: recall that [[NamedOperator]]s are rules, after all,
  *                           thus can appear in contexts)
  * @param errorCont Error handler used by diagram operators to report any errors (incl.
  *                  being undefined, i.e., not applicable, on certain diagrams, modules, or constants)
  * @todo maybe rename to DiagramEvaluator? any every `interp: DiagramInterpreter` to `eval: DiagramEvaluator`?
  *
  * todo: added results/connections are buffered until commit() has been called. If operators invoke certain
  *       controller functions on buffered modules, this can lead to errors or inconsistent results.
  *       As long as operators don't query the controller/simplifier/... for buffered modules, it's okay.
  *       Long-term: either drop this buffering or implement staged controllers (an idea FR once had)
  */
class DiagramInterpreter(val ctrl: Controller, private val interpreterContext: Context, val errorCont: ErrorHandler) {

  private val addedModules: mutable.LinkedHashSet[Module] = mutable.LinkedHashSet[Module]()
  private val transientPaths: mutable.ListBuffer[Path] = mutable.ListBuffer()

  // need mutable.LinkedHashMap as it guarantees to preserve insertion order (needed for commit())
  private val transientToplevelResults : mutable.LinkedHashMap[MPath, Module] = mutable.LinkedHashMap()

  def toplevelResults: List[Module] = transientToplevelResults.values.toList

  val operators: Map[GlobalName, NamedOperator] =
    RuleSet
      .collectRules(ctrl, interpreterContext).get(classOf[NamedOperator])
      .map(op => (op.head, op))
      .toMap

  def add(elem: StructuralElement): Unit = {
    ctrl.add(elem)
    transientPaths += elem.path
  }

  def endAdd(elem: ContainerElement[_]): Unit = {
    ctrl.endAdd(elem)
    elem match {
      case module: Module => addedModules += module
      case _ => /* do nothing */
    }
  }

  def getAddedModules: List[Module] = addedModules.toList

  /**
    * [[add]] plus registering as a toplevel result
    */
  def addToplevelResult(m: Module): Unit = {
    add(m)
    transientToplevelResults.put(m.path, m)
  }

  def hasToplevelResult(m: MPath): Boolean = transientToplevelResults.contains(m)

  def get(p: MPath): Module = ctrl.getAs(classOf[Module], p)

  /**
    * Fully evaluates a diagram expression and returns a [[Diagram]] of [[MPath module paths]] upon success.
    *
    * Syntax of terms that represent valid diagram expressions:
    *
    * - the [[Term]] representation of a [[Diagram]] (see [[Diagram.toTerm]] and [[DiagramTermBridge]]);
    *   evaluated as the diagram itself
    * - an [[OMMOD]] referencing a diagram module (of the [[InstallDiagram]] structural feature)
    *   that has already been elaborated; evaluated as the output stored in it (see [[InstallDiagram.parseOutput]])
    * - an [[OMMOD]] referencing any other kind of module; evaluated as a singleton diagram of that module
    * - `OMA(... OMA( ... (OMA(diagop, arg1), ... ), argn)` where `diagop` references a constant
    *   for which there is a corresponding [[NamedOperator]] rule in [[interpreterContext]] (see [[operators]]);
    *   evaluated as [[NamedOperator.apply]] called with the whole term
    * - any other [[Term]] is simplified and tried again for the above cases.
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
    // TODO debug whether usage of removeOmbindc is really necessary
    removeOmbindc(t) match {
      // already a fully evaluated diagram, atomic diagram
      case DiagramTermBridge(diag) => Some(diag)

      // a reference to top-level diagram declaration (that hopefully has already been evaluated and whose
      // output stored)
      case OMMOD(diagramDerivedModule) if ctrl.getAsO(classOf[DerivedModule], diagramDerivedModule).isDefined =>
        Some(InstallDiagram.parseOutput(diagramDerivedModule)(ctrl.library))

      // evaluate remaining references to modules as singleton diagrams
      // (this behavior serving as syntactic sugar to spare people from writing singleton diagrams all over
      //  the place in diagram expressions)
      case OMMOD(m) => Some(Diagram.singleton(m))

      // application of a diagram operator
      case operatorExpression @ HasHead(p: GlobalName) if operators.contains(p) =>
        // Beware that we do not simplify the input term; in particular diagram operators are expected
        // to simplify their own arguments if needed.
        val opResult = operators(p)(operatorExpression)(this)
        opResult.flatMap(apply)

      // if everything else fails, run simplification and try again (via recursion)
      case diag =>
        val simplifiedDiag = {
          val su = SimplificationUnit(
            context = interpreterContext,
            expandConDefs = true,
            expandVarDefs = true,
            fullRecursion = true
          )

          // first expand all definitions as simplifier doesn't seem to definition-expand in cases like
          // t = OMA(OMS(?s), args) // here ?s doesn't get definition-expanded
          // TODO: resolve ombindc usage
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
