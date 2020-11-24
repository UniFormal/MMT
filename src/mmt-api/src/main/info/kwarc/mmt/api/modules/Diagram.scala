package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations.Marker
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.ConsoleWriter
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.uom.{SimplificationEnvironment, SimplificationUnit}

import scala.collection.mutable

object Diagram {
  val feature: String = "diagram"
}

/**
  * Module-level structural feature for installing diagrams into the ambient theory graph.
  *
  * @todo Navid: add example
  */
class Diagram extends ModuleLevelFeature(Diagram.feature) {
  override def getHeaderNotation: List[Marker] = Nil
  /** */
  def check(dm: DerivedModule)(implicit env: ExtendedCheckingEnvironment): Unit = {}

  override def modules(dm: DerivedModule, rules: Option[RuleSet], env: SimplificationEnvironment): List[Module] = {
    val df = dm.dfC.normalized.getOrElse(throw LocalError(s"diagram structural feature requires definiens (did you perhaps type = instead of :=?)"))

    // shadow rule parameter as [[ElaborationBasedSimplifier.applyElementEnd()]] always passes None so far
    val rules = RuleSet.collectRules(controller, dm.getInnerContext)
    val diagInterp = new DiagramInterpreter(dm.getInnerContext, rules, controller, env.errorCont)

    diagInterp(df) match {
      case Some(outputDiagram) =>
        diagInterp.toplevelResults.foreach(controller.presenter(_)(ConsoleWriter))
        println(s"\nAbove MMT surface syntax was printed by Scala class 'DiagramPublisher' for debugging reasons. Input diagram was: ${df}")

        dm.dfC.set(outputDiagram)
        diagInterp.toplevelResults

      case None =>
        env.errorCont(InvalidElement(dm, "Diagram operator returned None"))
        Nil
    }
  }
}

/**
  * A diagram operator whose main functionality is given by [[apply()]].
  *
  * The method [[apply()]] receives a diagram and a [[DiagramInterpreter]] instance and
  * may then inspect the input elements in the diagram and freely add new (output)
  * elements via calls to the [[DiagramInterpreter]].
  *
  * All diagram operators extend [[SyntaxDrivenRule]], i.e. are [[Rule MMT rules]] that can
  * be loaded into MMT theories -- thus suitable for a modular approach to making diagram
  * operators available to end users -- and they carry a [[head]] symbol identifying them.
  *
  * To implement a new operator:
  *
  *   1. create an untyped constant in some MMT theory (in surface syntax), e.g.
  *      ''my_diag_op # MY_DIAG_OP'',
  *
  *   2. subclass [[DiagramOperator]] (or rather one of the many subclasses that suits you,
  *      most likely [[LinearOperator]]) and make [[head]] point to the URI of the just created symbol,
  *
  *   3. and load the diagram operator via ''rule <juri>scala://...'' (e.g. directly after the symbol from
  *      point 1).
  *
  * @see [[LinearOperator]]
  */
abstract class DiagramOperator extends SyntaxDrivenRule {
  // make parent class' head function a mere field, needed to include it in pattern matching patterns (otherwise: "stable identifier required, but got head [a function!]")
  override val head: GlobalName

  // need access to Controller for generative operators (i.e. most operators)
  // todo: upon error, are modules inconsistently added to controller? avoid that.
  def apply(diagram: Term, interp: DiagramInterpreter)(implicit ctrl: Controller): Option[Term]
}

object SequencedDiagramOperators extends DiagramOperator {
  final override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?sequence_diagram_operators")

  final override def apply(diagram: Term, interp: DiagramInterpreter)(implicit ctrl: Controller): Option[Term] = diagram match {
    case OMA(OMA(OMS(`head`), diagOps), diagram) =>
      val results = diagOps.flatMap(op => {
        val result = interp(OMA(op, diagram))
        if (result.isEmpty) {
          interp.errorCont(GeneralError("Failed to evaluate some diagram expression in expression of sequenced " +
            s"diagram operators. The specific expression was `$op`, the overall sequenced expression was " +
            s"`$diagram`. If subsequent diagram operators in the latter depend on the failed one, they may " +
            "fail, too."))
        }
        result
      })
      Some(OMA(OMS(head), results))

    case _ => None
  }
}

/**
  *
  * todo: added results/connections are buffered until commit() has been called. If operators invoke certain
  *       controller functions on buffered modules, this can lead to errors or inconsistent results.
  *       As long as operators don't query the controller/simplifier/... for buffered modules, it's okay.
  *       Long-term: either drop this buffering or implement staged controllers (an idea FR once had)
  *
  * @param interpreterContext
  * @param ctrl
  * @param solver
  * @param rules
  */
class DiagramInterpreter(private val interpreterContext: Context, private val rules: RuleSet, val ctrl: Controller, val errorCont: ErrorHandler) {

  private val transientPaths: mutable.ListBuffer[Path] = mutable.ListBuffer()

  // need mutable.LinkedHashMap as it guarantees to preserve insertion order (needed for commit())
  private val transientToplevelResults : mutable.LinkedHashMap[MPath, Module] = mutable.LinkedHashMap()

  def toplevelResults: List[Module] = transientToplevelResults.values.toList

  private val operators: Map[GlobalName, DiagramOperator] = rules.get(classOf[DiagramOperator]).map(op =>
    (op.head, op)
  ).toMap

  private def addToMapWithoutClash[K,V](map: mutable.Map[K, V], key: K, value: V, err: => Throwable): Unit = {
    if (map.getOrElseUpdate(key, value) != value) {
      throw err
    }
  }

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
    * Tries to evaluate a diagram expression and to delegate to [[DiagramOperator]]s found
    * in [[interpreterContext]].
    *
    * At most one step is performed.
    *
    * @param diag The input diagram
    * @return If diag is a [[SimpleDiagram]], it is returned as-is. Otherwise, if an operator
    *         in [[interpreterContext]] matches, that opreator is applied on diag.
    *         Yet otherwise, diag is simplified, and upon changes due to simplification in the term,
    *         [[apply()]] is tried again. If diag was alraedy stable, an [[Error]] is thrown.
    */
  def apply(diag: Term): Option[Term] = {
    val diag_ = removeOmbindc(diag)
    val head = diag_.head

    head.foreach {
      case p: GlobalName if operators.contains(p) =>
        // no simplification needed at this point
        // the called operator may still simplify arguments on its own later on
        val matchingOp = operators(p)
        return matchingOp(diag_, this)(ctrl)

      case _ => // carry on
    }

    diag_ match {
      case SimpleDiagram(_, _) => return Some(diag_)
      case _ => // carry on
    }

    val simplifiedDiag = {
      val su = SimplificationUnit(
        context = interpreterContext,
        expandDefinitions = true,
        fullRecursion = true
      )

      // first expand all definitions as simplifier doesn't seem to definition-expand in cases like
      // t = OMA(OMS(?s), args) // here ?s doesn't get definition-expanded
      removeOmbindc(ctrl.simplifier(ctrl.globalLookup.ExpandDefinitions(diag_, _ => true), su))
    }

    if (simplifiedDiag == diag_) {
      throw GeneralError(s"Cannot interpret diagram expressions below. Neither a diagram operator rule matches " +
        s"nor is it a SimpleDiagram: $simplifiedDiag. Is the operator you are trying to apply in-scope in " +
        s"the meta theory you specified for the diagram structural feature (`diagram d : ?meta := ...`)?")
    }

    apply(simplifiedDiag)
  }
}

object SimpleDiagram {
  private val constant = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?simple_diagram")

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
