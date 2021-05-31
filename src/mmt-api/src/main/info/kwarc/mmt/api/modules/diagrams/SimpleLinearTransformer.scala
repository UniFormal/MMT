package info.kwarc.mmt.api.modules.diagrams

import info.kwarc.mmt.api.ContentPath
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.symbols.{Constant, FinalConstant}

/**
  * Linearly transforms things
  * while hiding much of the complexity of their contents.
  *
  * Implementors only need to give a ''applyConstantSimple'' method.
  */
trait SimpleLinearTransformer extends LinearTransformer {
  /**
    * Maps a constant to a list of assignments in the connecting morphism.
    *
    * @return A list of assignments (simpleName, assignmentTerm), which is used by [[applyConstant]]
    *         to build a [[FinalConstant]] with the right name, empty type container, and a definiens container
    *         containing assignmentTerm.
    */
  protected def applyConstantSimple(c: Constant, tp: Term, df: Option[Term])(implicit interp: DiagramInterpreter): List[Constant]

  final override def applyConstant(c: Constant, container: Container)(implicit interp: DiagramInterpreter): Unit = {
    val curSkippedDeclarations = skippedDeclarations(c.path.module).toSet.asInstanceOf[Set[ContentPath]]

    def expand(t: Term): Term =
      interp.ctrl.library.ExpandDefinitions(t, curSkippedDeclarations)

    val expandedTp = c.tp.map(expand).getOrElse({NotApplicable(c, "no type component"); return})
    val expandedDf = c.df.map(expand)

    val outConstants = applyConstantSimple(c, expandedTp, expandedDf)
    outConstants.foreach(interp.add)
  }
}

trait SimpleLinearFunctor extends LinearFunctor with SimpleLinearTransformer
trait SimpleLinearConnector extends LinearConnector with SimpleLinearTransformer