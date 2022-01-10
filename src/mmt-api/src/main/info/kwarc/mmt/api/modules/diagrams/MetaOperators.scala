package info.kwarc.mmt.api.modules.diagrams

/**
  * Foundation-independent *meta* diagram operators, e.g. to sequence diagram operators, or
  * to build union, intersection of diagrams (not yet implemented).
  */

import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{OMA, OMS, Term}
import info.kwarc.mmt.api.{GeneralError, GlobalName, Path}

/**
  * Sequences diagram operators and merges their result.
  *
  * ''OMA(OMA(OMS(head), diagOps), diagram)'' represents an invocation of this operator.
  *
  * All diagram operators referenced in diagOps will be applied in order left-to-right to diagram.
  */
object SequencedDiagramOperators extends NamedOperator {
  final override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?sequence_diagram_operators")

  final override def apply(rawDiagram: Term)(implicit interp: DiagramInterpreter): Option[Term] = rawDiagram match {
    case OMA(OMA(OMS(`head`), diagOps), diagram) =>
      val outDiagrams: List[Diagram] = diagOps
        .map(op => (op, interp(OMA(op, diagram))))
        .flatMap {
          case (_, Some(diag)) => Some(diag)
          case (op, None) =>
            interp.errorCont(GeneralError(s"Failed to apply operator `$op` in operator " +
              s" sequence `$diagOps`. The overall diagram expression was `$diagram`. If " +
              "subsequent diagram operators in the latter depend on the failed one, they may " +
              "fail, too."))
            None
        }

      Some(Diagram.union(outDiagrams)(interp.ctrl.library).toTerm)

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
object ClosureDiagramOperator extends NamedOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?closure_operator")

  override def apply(t: Term)(implicit interp: DiagramInterpreter): Option[Term] = t match {
    // so far only support closing diagrams with no meta diagram
    case OMA(OMS(`head`), List(metaDiagramTerm, diagramTerm)) =>

      (interp(metaDiagramTerm), interp(diagramTerm)) match {
        case (Some(metaDiagram), Some(diag @ Diagram(_, None))) =>
          val closedDiagram = diag.closure(metaDiagram)(interp.ctrl.library)
          // Some diagram operators (hackily) expect their input diagram to be sorted in dependency order
          val sortedDiagram = Diagram(closedDiagram.modules.sortWith(interp.ctrl.globalLookup.hasImplicit), closedDiagram.mt)

          Some(sortedDiagram.toTerm)

        case _ => None
      }

    case _ => None
  }
}

object UnionDiagramOperator extends NamedOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?union_operator")

  override def apply(t: Term)(implicit interp: DiagramInterpreter): Option[Term] = t match {
    case OMA(OMS(`head`), diagramTerms) =>
      val diagrams = diagramTerms.flatMap(interp.apply)
      Some(Diagram.union(diagrams)(interp.ctrl.library).toTerm)

    case _ => None
  }
}

// naive implementation, no error reporting on diagram meta inconsistencies between subtrahend, minuend
object DifferenceOperator extends NamedOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?difference")

  override def apply(t: Term)(implicit interp: DiagramInterpreter): Option[Term] = t match {
    case OMA(OMS(`head`), List(subtrahendTerm, minuendTerm)) =>
      val Some((subtrahend, minuend)) = interp(subtrahendTerm) zip interp(minuendTerm)
      Some(subtrahend.copy(
        modules = subtrahend.modules.filterNot(minuend.modules.contains)
      ).toTerm)

    case _ => None
  }
}

// naive implementation, no error reporting on diagram meta inconsistencies between subtrahend, minuend
object RebaseOperator extends NamedOperator {
  override val head: GlobalName = Path.parseS("http://cds.omdoc.org/urtheories?DiagramOperators?rebase")

  override def apply(t: Term)(implicit interp: DiagramInterpreter): Option[Term] = t match {
    case OMA(OMS(`head`), List(diagramTerm, newMetaDiagramTerm)) =>
      val Some((diagram, newMeta)) = interp(diagramTerm) zip interp(newMetaDiagramTerm)

      Some(diagram.copy(mt = Some(newMeta)).toTerm)

    case _ => None
  }
}
