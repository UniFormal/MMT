package info.kwarc.mmt.moduleexpressions.operators

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.checking._
import info.kwarc.mmt.api.objects.{Term, _}
import info.kwarc.mmt.api.refactoring.linkinversion.{ContinuationStyle, RewriteError, RewriteErrorHandler, SkipDeclaration}
import info.kwarc.mmt.api.uom._

object Generalizer extends BinaryConstantScala(Combinators._path, "generalizer") {
  /** the label of the distinguished node of the output diagram */
  val nodeLabel = LocalName("pres")
}

object ComputeGeneralized extends ComputationRule(Generalizer.path) {
  def apply(solver: CheckingCallback)(tm: Term, covered: Boolean)(implicit stack: Stack, history: History): Simplifiability = {

    /* Unwrap input */
    val Generalizer(OMMOD(inputTheoryPath), OMMOD(inputMorphismPath)) = tm

    val RToS = solver.lookup.getView(inputMorphismPath)
    val OMMOD(theoryR) = RToS.from
    val OMMOD(theoryS) = RToS.to
    val theoryT = solver.lookup.getTheory(inputTheoryPath)

    val generalizedTheoryPath = theoryT.path.parent ? (theoryT.path.name + "Generalized")
    val generatedMorphismPath = RToS.path.parent ? (RToS.path.name + "Generated")

    val rewriteErrorHandler = new RewriteErrorHandler {
      override def apply(error: RewriteError): ContinuationStyle = {
        SkipDeclaration
      }
    }

    // Where to get controller from?
    // Rewrite all code transitively to only use Lookup?
    // LFLinkInverter.invertLink(...)

    Recurse
  }

}
