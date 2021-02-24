package info.kwarc.mmt.api.modules.diagrams

/*
  * Diagram operators implementation:
  *
  * in mmt-api, it consists of files {Diagram, DiagramState, DiagramTransformer, FunctorialOperator, StandardOperators}.scala.
  *
  *
  * Design decisions
  * ====================
  *
  * - separation into named and anonymous operators in Operators.scala and LinearTransformer.scala,
  *   respectively.
  *
  *   Named operators are associated to an MMT symbol and inherit SyntaxDrivenRule.
  *
  * - use cases of anonymous operators so far:
  *
  *   - named parametric operators that create anonymous (parametrized!) operators on-the-fly at runtime
  *
  * - Parametric operators *cannot* be implemented by ParametricRules:
  *
  *   For parametric operators, you'd like to load them once via ''rule scala://...ParametricOperator''
  *   and to be able to use them afterwards with *arbitrary* parameters.
  *   If they were ParametricRules, you'd have to have a ''rule'' declaration every time you'd like
  *   to use the operator with a different set of parameters.
  *
  * - DiagramState is complicated because you'd like every operator to be able to carry its own state
  *   (which might be more than the default state) while ensuring type safety.
  *
  * - Invariant of operator states:
  *
  *   - operator states are a mathematical function of the context (i.e. pure)
  *   - we should throw the states away after processing ''diagram'' declarations
  *     => if later another ''diagram'' declaration appears that necessitates some of the thrown away
  *     states, we just recompute
  */

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.Module
import info.kwarc.mmt.api.notations.Marker
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.ConsoleWriter
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.uom.{SimplificationEnvironment, SimplificationUnit}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.web.SyntaxPresenterServer

import scala.collection.mutable

object InstallDiagram {
  val feature: String = "diagram"

  def saveOutput(diagFeaturePath: MPath, diagram: Diagram)(implicit library: Lookup): Unit = {
    library.getModule(diagFeaturePath).dfC.normalized = Some(diagram.toTerm)
  }

  /**
    * Parses the output of a previously elaborated [[InstallDiagram]] structural feature.
    *
    * @throws GetError if the module referenced by diagramModulePath cannot be found
    * @throws InvalidElement if the module doesn't correspond to a usage of the Diagram structural feature
    * @throws InvalidElement if the module hasn't been elaborated before
    * @return All module entries of the output diagram (as were the result of elaboration before).
    */
  def parseOutput(diagPath: MPath)(implicit library: Lookup): Diagram = {
    val diagModule = library.get(diagPath) match {
      case diagModule: DerivedModule if diagModule.feature == InstallDiagram.feature =>
        diagModule

      case s => throw InvalidElement(s, s"referenced diagram DerivedModule `$s` not a derived module or doesn't have diagram feature")
    }

    diagModule.dfC.normalized match {
      case Some(DiagramTermBridge(diag)) => diag

      case Some(t) =>
        throw InvalidObject(t, "not a valid output in dfC of diagram structural feature")

      case None =>
        throw InvalidElement(diagModule, "referenced diagram DerivedModule doesn't have definiens. Have you run the Elaborator on it? In case the diagram is given in a file, have you built the file? (note that diagrams aren't written to OMDoc yet, so you always need to rebuild upon runtime.")
    }
  }
}

/**
  * Module-level structural feature for installing diagrams into the ambient theory graph.
  *
  * @todo Navid: add example
  */
class InstallDiagram extends ModuleLevelFeature(InstallDiagram.feature) {
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
        InstallDiagram.saveOutput(dm.path, outputDiagram)(controller.library)
        val outputModules = outputDiagram.modules.map(controller.getModule)

        // syntax-present output diagram for quick debugging
        outputModules.foreach(controller.presenter(_)(ConsoleWriter)) // use outputModules here, not diagInterp.toplevelResults
                                                                      // TODO: investigate why they differ
        println(s"""

${this.getClass.getSimpleName} debug
-------------------------------------
input: $df
operators in scope: ${rules.get(classOf[DiagramOperator]).map(op => op.getClass.getSimpleName).mkString(", ")}

output: see above, at ${SyntaxPresenterServer.getURIForDiagram(URI("http://localhost:8080"), dm.path)}, or here:
        $outputDiagram
-------------------------------------

""")

        outputModules

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


  /**
    * Call [[DiagramInterpreter.apply()]] on diagram arguments before inspecting them!
    *
    * // need access to Controller for generative operators (i.e. most operators)
    * // todo: upon error, are modules inconsistently added to controller? avoid that.
    *
    * @param diagram
    * @param interp
    * @param ctrl
    * @return
    */
  def apply(diagram: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term]
}

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

  private val operators: Map[GlobalName, DiagramOperator] = rules.get(classOf[DiagramOperator]).map(op =>
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
    * Tries to fully evaluate a diagram expression and to delegate to [[DiagramOperator]]s found
    * in [[interpreterContext]].
    *
    * @param t The input diagram expression
    */
  def apply(t: Term): Option[Diagram] = {
    object HasHead {
      def unapply(t: Term): Option[ContentPath] = t.head
    }

    removeOmbindc(t) match {
      case DiagramTermBridge(diag) => Some(diag)

      case OMMOD(diagramDerivedModule) =>
        Some(InstallDiagram.parseOutput(diagramDerivedModule)(ctrl.library))

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

/**
  * A diagram of MMT [[Module]]s over some optional meta diagram.
  *
  * Among other uses, diagrams serve as the domain and codomain of most diagram operators, see
  * [[RelativeBaseTransformer]] as the most general trait fleshing this out.
  * If a diagram operator has domain `Dom`, then you may apply the operator to all diagrams
  * `D` "that are over `Dom`". Concretely, the operator is applicable iff.
  * `D.mt.exists(Dom.subsumes(_))`.
  *
  * FUTURE WORK: diagrams may also contain commutativity assertions.
  *
  * @param modules The contained [[Module]]s as referenced via their paths.
  * @param mt Meta diagram
  */
sealed case class Diagram(modules: List[MPath], mt: Option[Diagram] = None) {
  def toTerm: Term = mt match {
    case Some(baseDiagram) => OMA(OMS(DiagramTermBridge.basedDiagram), baseDiagram.toTerm :: DiagramTermBridge.PathCodec(modules))
    case None => OMA(OMS(DiagramTermBridge.rawDiagram), DiagramTermBridge.PathCodec(modules))
  }

  /**
    * All modules contained in this diagram and contained in all meta diagrams.
    */
  lazy val getAllModules: Set[MPath] = modules.toSet ++ mt.map(_.getAllModules).getOrElse(Set.empty[MPath])

  def hasImplicitFrom(source: MPath)(implicit lookup: Lookup): Boolean = {
    getAllModules.exists(m => lookup.hasImplicit(source, m))
  }

  def hasImplicitTo(target: MPath)(implicit lookup: Lookup): Boolean = {
    getAllModules.exists(m => lookup.hasImplicit(m, target))
  }

  def subsumes(other: Diagram)(implicit lookup: Lookup): Boolean = {
    val doesSubsume = other.mt.forall(subsumes) && other.modules.forall(hasImplicitTo)
    doesSubsume
  }

  /**
    * Computes naive union, probably not what we want for long-term
    */
  def union(other: Diagram)(implicit lookup: Lookup): Diagram = {
    val newModules = (modules ++ other.modules).distinct
    val newMeta = (mt, other.mt) match {
      case (Some(m1), Some(m2)) => Some(m1.union(m2))
      case (Some(m1), _) => Some(m1)
      case (_, Some(m2)) => Some(m2)
      case (_, _) => None
    }

    Diagram(newModules, newMeta)
  }

  /**
    * Closes a diagram wrt. a meta diagram.
    *
    * E.g. suppose you got a hierachy of theories encoding FOL (FOLForall, FOLExists, FOLForallNDIntro,
    * FOLForallNDElim, FOLForallND, ...). Suppose the theory FOL includes them all, and that everything
    * is based on a formalization of propositional logic PL that is also modular.
    * We can close the singleton diagram FOL wrt. PL to get all FOL theories, but not PL.
    */
  def closure(intendedMeta: Diagram)(implicit lookup: Lookup): Diagram = {
    require(mt.isEmpty, "closure of diagram that already has meta diagram not yet implemented")
    val metaModules = intendedMeta.getAllModules

    val queue = mutable.ListBuffer[MPath]()
    val seen = mutable.HashSet[MPath]()
    val collected = mutable.HashSet[MPath]()

    queue ++= modules

    while (queue.nonEmpty) {
      val next = queue.remove(0)

      if (!seen.contains(next)) {
        seen += next

        val m = lookup.getModule(next)
        if (!metaModules.exists(metaModule => lookup.hasImplicit(m.path, metaModule))) {
          val referencedModulePaths = m.getDeclarations.flatMap {
            case s: Structure => getAllModulePaths(s.from) ++ getAllModulePaths(s.to)
            case nm: NestedModule => Set(nm.module.path)
            case _ => Set.empty
          }.toSet

          collected += m.path
          queue ++= referencedModulePaths
        }
      }
    }

    Diagram(collected.toList, Some(intendedMeta))
  }

  private def getAllModulePaths(t: Term): Set[MPath] = {
    val paths = mutable.HashSet[MPath]()
    modulePathExtractor(t, paths)

    paths.toSet
  }

  private val modulePathExtractor = new Traverser[mutable.Set[MPath]] {
    override def traverse(t: Term)(implicit con: Context, state: State): Term = t match {
      case OMMOD(mp) =>
        state += mp
        t
      case _ => Traverser(this, t)
    }
  }
}

object Diagram {
  val empty: Diagram = Diagram(List())
  def singleton(theory: MPath): Diagram = Diagram(List(theory))

  def union(diags: Seq[Diagram])(implicit lookup: Lookup): Diagram = diags.reduceLeft((d1, d2) => d1.union(d2))
}

object DiagramTermBridge {
  val rawDiagram: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?raw_diagram")
  val basedDiagram: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?based_diagram")

  def apply(diag: Diagram): Term = diag.toTerm

  def unapply(t: Term): Option[Diagram] = t match {
    case OMA(OMS(`rawDiagram`), PathCodec(modules)) =>
      Some(Diagram(modules, None))

    case OMA(OMS(`basedDiagram`), DiagramTermBridge(baseDiagram) :: PathCodec(modules)) =>
      Some(Diagram(modules, Some(baseDiagram)))

    case _ => None
  }

  object PathCodec {
    def apply(pathTerms: List[MPath]): List[Term] = pathTerms.map(OMMOD(_))

    def unapply(pathTerms: List[Term]): Option[List[MPath]] = Some(pathTerms.collect {
      case OMMOD(path) => path
      case _ => return None
    })
  }
}
