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
        diagInterp.committedModules.foreach(controller.presenter(_)(ConsoleWriter))
        println(s"\nAbove MMT surface syntax was printed by Scala class 'DiagramPublisher' for debugging reasons. Input diagram was: ${df}")

        dm.dfC.set(outputDiagram)
        diagInterp.committedModules

      case None =>
        throw GeneralError("Found diagram operator was partial on input diagram")
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
  // need mutable.LinkedHashMap as it guarantees to preserve insertion order (needed for commit())
  private val transientResults : mutable.LinkedHashMap[MPath, Module] = mutable.LinkedHashMap()
  // need mutable.LinkedHashMap as it guarantees to preserve insertion order (needed for commit())
  private val transientConnections : mutable.LinkedHashMap[MPath, Module] = mutable.LinkedHashMap()

  private val _committedModules = mutable.ListBuffer[Module]()

  private val operators: Map[GlobalName, DiagramOperator] = rules.get(classOf[DiagramOperator]).map(op =>
    (op.head, op)
  ).toMap

  private def addToMapWithoutClash[K,V](map: mutable.Map[K, V], key: K, value: V, err: => Throwable): Unit = {
    if (map.getOrElseUpdate(key, value) != value) {
      throw err
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
    if (transientConnections.contains(m.path)) {
      throw GeneralError(s"Attempted to add module ${m.path} to DiagramInterpreter as a result, but a module " +
        s"with the same path had already been added as a connection result.")
    }
    addToMapWithoutClash(transientResults, m.path, m, err = GeneralError(
      s"Attempted to add module ${m.path} to DiagramInterpreter as a result, but a *different* module with the +" +
        s"same path had already been added"
    ))
  }

  def hasResult(p: MPath): Boolean = transientResults.contains(p)

  def addConnection(m: Module): Unit = {
    if (transientResults.contains(m.path)) {
      throw GeneralError(s"Attempted to add module ${m.path} to DiagramInterpreter as a connection result, but a module " +
        s"with the same path had already been added as a main result.")
    }
    addToMapWithoutClash(transientConnections, m.path, m, err = GeneralError(
      s"Attempted to add module ${m.path} to DiagramInterpreter as a connection result, but a *different* module with " +
        s"the same path had already been added"
    ))
  }

  def get(p: MPath): Module = {
    transientResults.getOrElse(p,
      transientConnections.getOrElse(p,
        ctrl.getAs(classOf[Module], p)
      )
    )
  }

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

  def apply(diag: Term): Option[Term] = {
    val simplifiedDiag = removeOmbindc(removeOmbindc(diag) match {
      case OMA(OMS(head), _) if operators.contains(head) && false => // todo: for now always simplify for debugging
        // no simplification needed at this point
        // the called operator may still simplify arguments on its own later on
        diag
      case t =>
        val su = SimplificationUnit(
          context = interpreterContext,
          expandDefinitions = true,
          fullRecursion = true
        )
        ctrl.simplifier(t, su)
    })

    simplifiedDiag match {
      case t @ OMA(OMS(head), _) if operators.contains(head) =>
        val matchingOp = operators(head)

        val result = matchingOp(t, this)(ctrl)
        commit()
        result

      case t @ SimpleDiagram(_, _) =>
        Some(t)

      case _ =>
        throw GeneralError(s"Cannot interpret diagram expressions below. Neither a diagram operator rule matches " +
          s"nor is it a SimpleDiagram: ${simplifiedDiag}. Is the operator you are trying to apply in-scope in " +
          s"the meta theory you specified for the diagram structural feature (`diagram d : ?meta := ...`)?")
    }
  }
}

object SimpleDiagram {
  private val constant = Path.parseS("http://cds.omdoc.org/urtheories/modexp-test?DiagramOperators?simple_diagram")

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
