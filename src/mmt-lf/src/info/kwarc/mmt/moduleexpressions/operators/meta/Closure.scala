package info.kwarc.mmt.moduleexpressions.operators.meta

import info.kwarc.mmt.api.DPath
import info.kwarc.mmt.api.archives.Archive
import info.kwarc.mmt.api.checking.{CheckingCallback, ComputationRule, History}
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.Module
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.uom._
import info.kwarc.mmt.api.utils.BreadthFirstSearch
import info.kwarc.mmt.moduleexpressions.operators.{Combinators, Common}

object DiagramClosure extends FlexaryConstantScala(Combinators._path, "diagram_closure")

object ComputeDiagramClosure extends ComputationRule(DiagramClosure.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = tm match {
    case DiagramClosure(modulePathTerms@_*) =>
      val modulePaths = modulePathTerms.flatMap {
        case OMMOD(modulePath) => List(modulePath)
        case _ =>
          solver.error("Encountered non-module path given to diagram closure operator")
          Nil
      }

      val modules = modulePaths.map(solver.lookup.getModule)
      val closedSetOfModules = getClosureOfModules(
        solver.lookup,
        modules.toSet,
        withinDocuments = modules.map(_.parent).toSet
      )
      val anonDiag = Common.asAnonymousDiagram(solver, closedSetOfModules)

      Simplify(anonDiag.toTerm)
  }

  private def getClosureOfModules(lookup: Lookup, modules: Set[Module], withinDocuments: Set[DPath]): Set[Module] = {
    BreadthFirstSearch.collect(modules.toSeq, module => {
      module
        .getAllIncludes
        .map(_.from)
        .filter(path => withinDocuments.contains(path.doc))
        .map(lookup.getModule).toSet
    })
  }
}
