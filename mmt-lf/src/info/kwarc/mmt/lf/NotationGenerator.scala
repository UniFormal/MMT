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
   /**
    * Determines whether a type is a judgment type
    * @param tp the argument type of a constant
    * @return true if tp is atomic and formed from a symbol with role "Judgment" or a function type returning such a judgment type
    * 
    * Other extensions may want to override this to consider more types as judgment types
    * (e.g., Sigma types formed from judgment types).
    */
   protected def isJudgment(tp: Term): Boolean = tp match {
      case FunType(_, ApplySpine(OMS(s),_)) =>
         //this can throw errors if the implicit graph is not fully loaded
         try {
            controller.globalLookup.getConstant(s).rl == Some("Judgment")
         } catch {case e: Error =>
            false
         }
      case _ => false
   }
   def onUpdate(e: ContentElement) {onAdd(e)}
   def onAdd(e: ContentElement) {e match {
      case c: Constant =>
         val notC = c.notC
         if (notC.parsing.isDefined && notC.presentation.isDefined) return
         val tpU = c.tpC.get.getOrElse(return) // nothing to do if there is no (function) type
         val (_, tp) = parser.ObjectParser.splitOffUnknowns(tpU)
         val (args, scp) = FunType.unapply(tp).getOrElse(return)
         val numTotalArgs = args.length
         if (numTotalArgs == 0 || ! isJudgment(scp)) return
         val numImplicitArgs = args.prefixLength {case (_, argType) => ! isJudgment(argType)}
         log(s"adding notation for ${c.name} ($numImplicitArgs implicit args, $numTotalArgs total args")
         if (notC.parsing.isEmpty) {
            val parseMarkers = SymbolName() ::
                Range(0,numImplicitArgs).map {i => ImplicitArg(i+1)}.toList :::
                Range(numImplicitArgs,numTotalArgs).map {i => Arg(i+1)}.toList
            val nt = new TextNotation(Mixfix(parseMarkers), Precedence.integer(0), Some(LF._path))
            metadata.Generated.set(nt)
            c.notC.parsingDim.set(nt)
         }
         if (notC.presentation.isEmpty) {
            val tree = if (numImplicitArgs == numTotalArgs) Nil else {
               val hyps = Range(numImplicitArgs,numTotalArgs).map {i => Arg(i+1)}.toList
               List(FractionMarker(hyps, List(InferenceMarker), true))
            }
            val presentationMarkers : List[Marker] = tree ::: SymbolName() ::
               Range(0,numImplicitArgs).map {i => ImplicitArg(i+1)}.toList
            val nt = new TextNotation(Mixfix(presentationMarkers), Precedence.integer(0), Some(LF._path))
            metadata.Generated.set(nt)
            c.notC.presentationDim.set(nt)
         }
      case _ =>
   }}
}
