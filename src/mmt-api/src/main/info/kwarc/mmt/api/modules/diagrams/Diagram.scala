package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.Module
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._

import scala.collection.mutable

/**
  * A diagram of MMT [[Module]]s given by [[MPath module paths]] -- an atomic [[DiagramInterpreter diagram expression]].
  * In most cases, the diagram consists of [[info.kwarc.mmt.api.modules.Theory theories]] and
  * [[info.kwarc.mmt.api.modules.View views]].
  *
  * Diagrams serve as inputs and outputs of [[UnaryOperator]]s.
  * [[LinearFunctor]]s furthermore use diagrams to specify their domain and codomain;
  * upon application, they translate diagrams *over* their domain diagram to diagrams *over* their codomain diagram.
  * For this reason, diagrams may have meta diagrams (by the parameter `mt`).
  *
  * Diagrams should be closed in the sense that all modules that are referenced from modules contained in this diagram
  * should also be contained in this diagram or in the meta diagram `mt`.
  * This is not a strict requirement, e.g., [[LinearFunctor]]s also work on unclosed diagrams, but their behavior might
  * be unexpected (e.g., because functors are applied to modules [by recursing into module references] that had never
  * been specified in the input diagram).
  *
  * In the future, diagrams might also contain commutativity assertions.
  *
  * @param modules The contained [[Module]]s given by their paths.
  *                All module references occurring in `modules` should be
  * @param mt Optional meta diagram. Diagrams to which [[LinearFunctor]]s are applied are usually expected to have
  *           meta diagrams. Otherwise, inapplicability will be signaled in one way or another, see
  *           [[LinearFunctor.applyDiagram]].
  *
  * @todo Specify which includes are part of the diagram, e.g. if a diagram contains theories S and T and T includes S,
  *       must the diagram also contain the include? Some systems (Hets or Specware, not sure) allow to suppress such
  *       includes in diagrams, which has an effect of applied operations (e.g., diagram operators, colimits).
  */
sealed case class Diagram(modules: List[MPath], mt: Option[Diagram] = None) {
  /** The term representation; short-hand for [[DiagramTermBridge.apply]]. */
  def toTerm: Term = DiagramTermBridge(this)

  /**
    * All modules contained in the diagram and (recursively) in the meta diagram.
    */
  lazy val getAllModules: Set[MPath] = modules.toSet ++ mt.map(_.getAllModules).getOrElse(Set.empty[MPath])

  def hasImplicitFrom(source: Term)(implicit lookup: Lookup): Boolean = {
    getAllModules.exists(m => lookup.hasImplicit(source, OMMOD(m)))
  }
  def hasImplicitFrom(source: MPath)(implicit lookup: Lookup): Boolean = hasImplicitFrom(OMMOD(source))

  def hasImplicitTo(target: Term)(implicit lookup: Lookup): Boolean = {
    getAllModules.exists(m => lookup.hasImplicit(OMMOD(m), target))
  }
  def hasImplicitTo(target: MPath)(implicit lookup: Lookup): Boolean = hasImplicitTo(OMMOD(target))

  def subsumes(other: Diagram)(implicit lookup: Lookup): Boolean = {
    val doesSubsume = other.mt.forall(subsumes) && other.modules.forall(hasImplicitTo)
    doesSubsume
  }

  /**
    * Computes the union by distinctly-unioning module paths and recursively unioning the meta diagrams.
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
  /** The empty diagram with no meta diagram. */
  val empty: Diagram = Diagram(List())
  /** The singleton diagram of exactly one [[info.kwarc.mmt.api.modules.Theory theory]] with no meta diagram. */
  def singleton(theory: MPath): Diagram = Diagram(List(theory))

  /** The union of multiple diagrams; equivalent to chaining the diagrams with [[Diagram.union()]]. */
  def union(diags: Seq[Diagram])(implicit lookup: Lookup): Diagram = diags.reduceLeft((d1, d2) => d1.union(d2))
}

// todo needed anyway?
sealed case class DiagramFunctor(dom: Diagram, cod: Diagram, functor: Map[MPath, Term], metaFunctor: Option[DiagramFunctor] = None) {
  def apply(m: MPath): Term = apply(OMMOD(m))
  def apply(t: Term): Term = t match {
    case OMMOD(m) => functor.getOrElse(m, metaFunctor.map(_(m)).getOrElse(t)) // default to return input as-is
    case OMIDENT(m) => OMIDENT(apply(m))
    case OMCOMP(mors) => OMCOMP(mors.map(apply))
    case _ => ???
  }
}

// todo needed anyway?
object DiagramFunctor {
  def identity(dom: Diagram): DiagramFunctor = DiagramFunctor(
    dom,
    dom,
    dom.modules.map(m => m -> OMMOD(m)).toMap
  )
}

/**
  * todo needed anyway?
  * @param functor
  * @param tx maps every theory `T` in [[functor.dom]] to a morphism from `T` to [[functor.apply() functor.apply(T)]]
  * @param metaConnection
  */
sealed case class DiagramConnection(functor: DiagramFunctor, tx: Map[MPath, Term], metaConnection: Option[DiagramConnection] = None) {

  val dom: Diagram = functor.dom
  val cod: Diagram = functor.cod

  /* no cases for OMIDENT, OMCOMP as apply is only applicable on theory expressions */
  def applyTheory(thy: MPath): Term = {
    // default to return input as-is
    tx.getOrElse(thy, metaConnection.map(_.applyTheory(thy)).getOrElse(OMMOD(thy)))
  }
}

// todo needed anyway?
object DiagramConnection {
  def identity(dom: Diagram): DiagramConnection = DiagramConnection(
    DiagramFunctor.identity(dom),
    dom.modules.map(m => m -> OMIDENT(OMMOD(m))).toMap
  )

  object Singleton {
    def apply(domTheory: MPath, codTheory: MPath, mor: Term): DiagramConnection = {
      DiagramConnection(
        DiagramFunctor(Diagram(List(domTheory)), Diagram(List(codTheory)), Map(domTheory -> OMMOD(codTheory))),
        Map(domTheory -> mor)
      )
    }

    def unapply(connection: DiagramConnection): Option[(MPath, MPath, Term)] = connection match {
      case DiagramConnection(
        DiagramFunctor(Diagram(List(dom), None), Diagram(List(cod), None), functor, None),
        tx,
        None
      ) if functor == Map(dom -> OMMOD(cod)) && tx.size == 1 && tx.contains(dom) =>
        Some((dom, cod, tx(dom)))
      case _ => None
    }
  }
}

/**
  * Bridge with apply/unapply methods between [[Diagram]]s and [[Term]]s.
  *
  * Diagrams containing module paths `m1, ..., mn`
  *
  *  - without meta diagram are encoded as `OMA([[DiagramTermBridge.rawDiagram]], OMMMOD(m1), ..., OMMOD(mn))`.
  *  - with meta diagram mt are encoded as `OMA([[DiagramTermBridge.basedDiagram]], |mt|, OMMOD(m1), ..., OMMOD(mn))`
  *    where |mt| is the encoding of mt
  */
object DiagramTermBridge {
  val rawDiagram: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?raw_diagram")
  val basedDiagram: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?based_diagram")

  def apply(diag: Diagram): Term = diag.mt match {
    case Some(baseDiagram) => OMA(
      OMS(DiagramTermBridge.basedDiagram),
      baseDiagram.toTerm :: PathCodec(diag.modules)
    )
    case None => OMA(OMS(DiagramTermBridge.rawDiagram), DiagramTermBridge.PathCodec(diag.modules))
  }

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

// todo move somewhere else
object SymbolPaths {
  private val path = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?symbol_paths")

  def apply(symbols: Seq[GlobalName]): Term = OMA(OMS(path), symbols.map(OMS(_)).toList)
  def unapply(t: Term): Option[List[GlobalName]] = t match {
    case OMA(OMS(`path`), symbols) =>
      Some(symbols.map {
        case OMS(symbol) => symbol
        case _ => return None
      })
    case _ => None
  }
}
