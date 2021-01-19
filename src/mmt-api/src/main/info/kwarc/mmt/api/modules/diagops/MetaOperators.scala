package info.kwarc.mmt.api.modules.diagops

/**
  * Foundation-independent *meta* diagram operators, e.g. to sequence diagram operators, or
  * to build union, intersection of diagrams (not yet implemented).
  */

import info.kwarc.mmt.api.{GeneralError, GlobalName, MPath, Path}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.libraries.Lookup
import info.kwarc.mmt.api.modules.{BasedDiagram, DiagramInterpreter, DiagramOperator, DiagramT, DiagramTermBridge, Module, RawDiagram, Theory}
import info.kwarc.mmt.api.objects.{Context, OMA, OMMOD, OMS, Term, Traverser}
import info.kwarc.mmt.api.symbols.{NestedModule, Structure}

import scala.collection.mutable


/**
  * Sequences diagram operators and merges their result.
  *
  * ''OMA(OMA(OMS(head), diagOps), diagram)'' represents an invocation of this operator.
  *
  * All diagram operators referenced in diagOps will be applied in order left-to-right to diagram.
  *
  * If all individual results were compatible [[BasedDiagram]]s, the result is also a [[BasedDiagram]]
  * with suitable base.
  * Otherwise, the results are merged into a [[RawDiagram]].
  */
object SequencedDiagramOperators extends DiagramOperator {
  final override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?sequence_diagram_operators")

  final override def apply(rawDiagram: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = rawDiagram match {
    case OMA(OMA(OMS(`head`), diagOps), diagram) =>
      val results = diagOps.flatMap(op => {
        val result = interp(OMA(op, diagram))
        if (result.isEmpty) {
          interp.errorCont(GeneralError(s"Failed to apply operator `$op` in operator " +
            s" sequence `$diagOps`. The overall diagram expression was `$diagram`. If " +
            "subsequent diagram operators in the latter depend on the failed one, they may " +
            "fail, too."))
        }
        result
      })

      Some(results.reduceLeft[Term]((diag1, diag2) => BasedDiagram.unionWithOther(diag1, diag2)(ctrl.globalLookup)))

    case _ => None
  }
}

/**
  * Closes a diagram wrt. a meta diagram.
  *
  * E.g. suppose you got a hierachy of theories encoding FOL (FOLForall, FOLExists, FOLForallNDIntro,
  * FOLForallNDElim, FOLForallND, ...). Suppose the theory FOL includes them all, and that everything
  * is based on a formalization of propositional logic PL that is also modular.
  * We can close the singleton diagram FOL wrt. PL to get all FOL theories, but not PL.
  */
object ClosureDiagramOperator extends DiagramOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?closure_operator")

  override def apply(t: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = t match {
    // so far only support closing diagrams with no meta diagram
    case OMA(OMS(`head`), List(metaDiagramTerm, diagramTerm)) =>

      (interp(metaDiagramTerm), interp(diagramTerm)) match {
        case (Some(DiagramTermBridge(metaDiagram)), Some(DiagramTermBridge(diagram @ DiagramT(_, None)))) =>
          Some(diagram.closure(metaDiagram)(interp.ctrl.globalLookup).toTerm)

        case _ => None
      }

    case _ => None
  }
}

object UnionDiagramOperator extends DiagramOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?union_operator")

  override def apply(t: Term)(implicit interp: DiagramInterpreter, ctrl: Controller): Option[Term] = t match {
    case OMA(OMS(`head`), diagramTerms) =>
      val diagrams = diagramTerms.map(interp.apply).collect {
        case Some(DiagramTermBridge(diag @ DiagramT(_, None))) => diag
        case _ => return None
      }

      Some(DiagramTermBridge(DiagramT(diagrams.flatMap(_.modules), None)))

    case _ => None
  }
}