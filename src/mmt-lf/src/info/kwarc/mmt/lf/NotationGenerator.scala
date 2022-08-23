package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import frontend._
import symbols._
import objects._
import notations._

/**
 * A ChangeListerner that adds notations for constants that are inference rules and do not provide notations.
 *
 * A constant is an inference rule if it takes some parameters (of non-judgment types) and some hypotheses (of judgment type)
 * and return a judgment types.
 *
 * For parsing, it makes the parameters implicit.
 *
 * For presentation, it renders a proof tree with the hypotheses above, the infered type below, and
 * the symbol name and the parameters to the left of the horizontal line.
 */
class NotationGenerator extends ChangeListener {
   override val logPrefix = "notation-gen"

   override def onAdd(e: StructuralElement): Unit = {e match {
      case c: Constant =>
         implicit val lup = controller.globalLookup
         val notC = c.notC
         if (notC.parsing.isDefined && notC.presentation.isDefined) return
         val tpU = c.tpC.get.getOrElse(return) // nothing to do if there is no (function) type
         val tp = parser.ParseResult.fromTerm(tpU).term
         //TODO handle free variables here?
         val (args, scp) = FunType.unapply(tp).getOrElse(return)
         val numTotalArgs = args.length
         if (numTotalArgs == 0 || ! JudgmentTypes.isJudgment(scp)) return
         val numImplicitArgs = args.prefixLength {case (_, argType) => ! JudgmentTypes.isJudgment(argType)}
         log(s"adding notation for ${c.name} ($numImplicitArgs implicit args, $numTotalArgs total args")
         if (notC.parsing.isEmpty) {
            val parseMarkers = SymbolName() ::
                Range(0,numImplicitArgs).map {i => ImplicitArg(i+1)}.toList :::
                Range(numImplicitArgs,numTotalArgs).map {i => SimpArg(i+1)}.toList
            val nt = new TextNotation(Mixfix(parseMarkers), Precedence.integer(0), Some(LF.theoryPath), false)
            metadata.Generated.set(nt)
            c.notC.parsingDim.set(nt)
         }
         if (notC.presentation.isEmpty) {
            val tree = if (numImplicitArgs == numTotalArgs) Nil else {
               val hyps = Range(numImplicitArgs,numTotalArgs).map {i => SimpArg(i+1)}.toList
               List(FractionMarker(hyps, List(InferenceMarker), true))
            }
            val presentationMarkers : List[Marker] = tree ::: SymbolName() ::
               Range(0,numImplicitArgs).map {i => ImplicitArg(i+1)}.toList
            val nt = new TextNotation(Mixfix(presentationMarkers), Precedence.integer(0), Some(LF.theoryPath), false)
            metadata.Generated.set(nt)
            c.notC.presentationDim.set(nt)
         }
      case _ =>
   }}
}
