package info.kwarc.mmt.api.modules

import info.kwarc.mmt.api
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking.CheckingCallback
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom.SimplificationUnit

import scala.collection.mutable

class DiagramInterpreter(private val interpreterContext: Context, val ctrl: Controller, val solver: CheckingCallback, rules: RuleSet) {
  // need mutable.LinkedHashMap as it guarantees to preserve insertion order (needed for commit())
  private var transientResults : mutable.LinkedHashMap[MPath, Module] = mutable.LinkedHashMap()
  // need mutable.LinkedHashMap as it guarantees to preserve insertion order (needed for commit())
  private var transientConnections : mutable.LinkedHashMap[MPath, Module] = mutable.LinkedHashMap()

  private var _committedModules = mutable.ListBuffer[Module]()

  private var operators: Map[GlobalName, DiagramOperator] = rules.get(classOf[DiagramOperator]).map(op =>
    (op.head, op)
  ).toMap

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
        throw GeneralError(s"Cannot interpret diagram expressions below. Neither a diagram operator rule matches" +
          s"nor is it a SimpleDiagram: ${simplifiedDiag}")
    }
  }
}

abstract class DiagramOperator extends SyntaxDrivenRule {
  // make parent class' head function a mere field, needed to include it in pattern matching patterns (otherwise: "stable identifier required, but got head [a function!]")
  val head: GlobalName

  // need access to Controller for generative operators (i.e. most operators)
  // todo: upon error, are modules inconsistently added to controller? avoid that.
  def apply(diagram: Term, interp: DiagramInterpreter)(implicit ctrl: Controller): Option[Term]
}

object SimpleDiagram {
  private val constant = Path.parseS("http://cds.omdoc.org/urtheories/modexp-test?DiagramOperators?simple_diagram", NamespaceMap.empty)

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
