package info.kwarc.mmt.api.modules.diagops

/**
  * Foundation-independent *meta* diagram operators, e.g. to sequence diagram operators, or
  * to build union, intersection of diagrams (not yet implemented).
  */

import info.kwarc.mmt.api.{GeneralError, GlobalName, Path}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.modules.{BasedDiagram, DiagramInterpreter, DiagramOperator, RawDiagram}
import info.kwarc.mmt.api.objects.{OMA, OMS, Term}


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