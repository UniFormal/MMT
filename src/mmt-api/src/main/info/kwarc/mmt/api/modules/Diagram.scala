package info.kwarc.mmt.api.modules

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
import info.kwarc.mmt.api.modules.diagops.LinearOperator
import info.kwarc.mmt.api.notations.Marker
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.ConsoleWriter
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.uom.{SimplificationEnvironment, SimplificationUnit}
import info.kwarc.mmt.api.utils.URI
import info.kwarc.mmt.api.web.SyntaxPresenterServer

import scala.collection.mutable

object Diagram {
  val feature: String = "diagram"

  def saveOutput(diagPath: MPath, output: Term)(implicit lookup: Lookup): Unit = {
    lookup.getModule(diagPath).dfC.normalized = Some(output)
  }

  /**
    * Parses the output of a previously elaborated [[Diagram]] structural feature.
    *
    * @throws GetError if the module referenced by diagramModulePath cannot be found
    * @throws InvalidElement if the module doesn't correspond to a usage of the Diagram structural feature
    * @throws InvalidElement if the module hasn't been elaborated before
    * @return All module entries of the output diagram (as were the result of elaboration before).
    */
  def parseOutput(diagPath: MPath)(implicit lookup: Lookup): DiagramT = {
    val diagModule = lookup.get(diagPath) match {
      case diagModule: DerivedModule if diagModule.feature == Diagram.feature =>
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
        Diagram.saveOutput(dm.path, outputDiagram)(controller.globalLookup)

        // syntax-present output diagram for quick debugging
        diagInterp.toplevelResults.foreach(controller.presenter(_)(ConsoleWriter))
        println(s"""

${this.getClass.getSimpleName} debug
-------------------------------------
input: $df
operators in scope: ${rules.get(classOf[DiagramOperator]).map(op => op.getClass.getSimpleName).mkString(", ")}

output: see above, at ${SyntaxPresenterServer.getURIForDiagram(URI("http://localhost:8080"), dm.path)}, or here:
        ${outputDiagram.toStr(shortURIs = true)}
-------------------------------------

""")

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
    * Tries to evaluate a diagram expression and to delegate to [[DiagramOperator]]s found
    * in [[interpreterContext]].
    *
    * At most one step is performed.
    *
    * @param diag The input diagram
    * @return If diag is a [[BasedDiagram]], it is returned as-is. Otherwise, if an operator
    *         in [[interpreterContext]] matches, that opreator is applied on diag.
    *         Yet otherwise, diag is simplified, and upon changes due to simplification in the term,
    *         [[apply()]] is tried again. If diag was alraedy stable, an [[Error]] is thrown.
    */
  def apply(t: Term): Option[Term] = {
    object HasHead {
      def unapply(t: Term): Option[ContentPath] = t.head
    }

    removeOmbindc(t) match {
      case operatorExpression @ HasHead(p: GlobalName) if operators.contains(p) =>
        // no simplification needed at this point
        // the called operator may still simplify arguments on its own later on
        val matchingOp = operators(p)
        val opResult = matchingOp(operatorExpression)(this, ctrl)
        opResult

      case diag @ DiagramTermBridge(_) => Some(diag)

      case OMMOD(diagramDerivedModule) =>
        Some(Diagram.parseOutput(diagramDerivedModule)(ctrl.globalLookup).toTerm)

      case diag =>
        val simplifiedDiag = {
          val su = SimplificationUnit(
            context = interpreterContext,
            expandDefinitions = true,
            fullRecursion = true
          )

          // first expand all definitions as simplifier doesn't seem to definition-expand in cases like
          // t = OMA(OMS(?s), args) // here ?s doesn't get definition-expanded
          removeOmbindc(ctrl.simplifier(ctrl.globalLookup.ExpandDefinitions(diag, _ => true), su))
        }

        if (simplifiedDiag == diag) {
          throw GeneralError(s"Cannot interpret diagram expressions below. Neither a diagram operator rule matches " +
            s"nor is it a BasedDiagram: $simplifiedDiag. Is the operator you are trying to apply in-scope in " +
            s"the meta theory you specified for the diagram structural feature (`diagram d : ?meta := ...`)? " +
            "Does the operator (a Scala object, not class!) have the correct `head` field?")
        }

        apply(simplifiedDiag)
    }
  }
}

// to be renamed to Diagram (after structural feature has been renamed)
sealed case class DiagramT(modules: List[MPath], mt: Option[DiagramT]) {
  def toTerm: Term = mt match {
    case Some(baseDiagram) => OMA(OMS(DiagramTermBridge.basedDiagram), baseDiagram.toTerm :: DiagramTermBridge.PathCodec(modules))
    case None => OMA(OMS(DiagramTermBridge.rawDiagram), DiagramTermBridge.PathCodec(modules))
  }

  /**
    * Returns modules contained in this diagram and contained in all meta diagrams.
    */
  def getAllModules: Set[MPath] = modules.toSet ++ mt.map(_.getAllModules).getOrElse(Set.empty[MPath])

  /**
    * Closes a diagram wrt. a meta diagram.
    *
    * E.g. suppose you got a hierachy of theories encoding FOL (FOLForall, FOLExists, FOLForallNDIntro,
    * FOLForallNDElim, FOLForallND, ...). Suppose the theory FOL includes them all, and that everything
    * is based on a formalization of propositional logic PL that is also modular.
    * We can close the singleton diagram FOL wrt. PL to get all FOL theories, but not PL.
    */
  def closure(intendedMeta: DiagramT)(implicit lookup: Lookup): DiagramT = {
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

    DiagramT(collected.toList, Some(intendedMeta))
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

object DiagramTermBridge {
  val rawDiagram: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?raw_diagram")
  val basedDiagram: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?based_diagram")

  def apply(diag: DiagramT): Term = diag.toTerm

  def unapply(t: Term): Option[DiagramT] = t match {
    case OMA(OMS(`rawDiagram`), PathCodec(modules)) =>
      Some(DiagramT(modules, None))

    case OMA(OMS(`basedDiagram`), DiagramTermBridge(baseDiagram) :: PathCodec(modules)) =>
      Some(DiagramT(modules, Some(baseDiagram)))

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

object BasedDiagram {
  private val constant = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?based_diagram")

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

  // we probably want to have case classes for RawDiagram and BasedDiagram, which would simplify
  // this function tremendously

  /**
    * Merges two diagrams, keeps only distinct diagram elements.
    *
    * If they are both compatible [[BasedDiagram]]s, the result is a [[BasedDiagram]] again.
    * Otherwise, they will both be merged into a [[RawDiagram]].
    *
    * ''BasedDiagram(b1, paths1)'' and ''BasedDiagram(b2, paths2)'' are compatible if there is either
    * an implicit morphism b1 -> b2 or b2 -> b1. IN both cases, they will be merged into a [[BasedDiagram]]
    * with paths ''(paths1 ::: paths2).distinct'' over the codomain of the implicit morphism (i.e., the
    * stronger base theory).
    */
  def unionWithOther(diag1: Term, diag2: Term)(implicit lookup: Lookup): Term = {
    val paths1 = diag1 match {
      case BasedDiagram(_, paths) => paths
      case RawDiagram(paths) => paths
    }
    val paths2 = diag2 match {
      case BasedDiagram(_, paths) => paths
      case RawDiagram(paths) => paths
    }
    val paths = (paths1 ::: paths2).distinct

    diag1 match {
      case RawDiagram(paths1) => diag2 match {
        case RawDiagram(_) => RawDiagram(paths)
        case BasedDiagram(_, paths2) => RawDiagram(paths)
      }
      case BasedDiagram(base1, paths1) => diag2 match {
        case RawDiagram(paths2) => RawDiagram(paths)
        // base 2 is stronger
        case BasedDiagram(base2, paths2) if lookup.hasImplicit(base1, base2) => BasedDiagram(base2, paths)
        // base 1 is stronger
        case BasedDiagram(base2, paths2) if lookup.hasImplicit(base2, base1) => BasedDiagram(base1, paths)
        case BasedDiagram(_, paths2) => RawDiagram(paths)
      }
    }
  }
}

object RawDiagram {
  private val constant = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?raw_diagram")

  def apply(paths: List[MPath]): Term = {
    OMA(OMS(constant), paths.map(OMMOD(_)))
  }
  def unapply(t: Term): Option[List[MPath]] = t match {
    case OMA(OMS(`constant`), pathTerms) =>
      val paths = pathTerms.collect {
        case OMMOD(path) => path
        case _ => return None
      }
      Some(paths)

    case _ => None
  }
}
