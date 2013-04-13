package info.kwarc.mmt.lf.modulo
import info.kwarc.mmt.api._
import objects._
import info.kwarc.mmt.lf._

object RewriteTerm extends InferenceRule(LFModulo.Rewrite.path) {
   def apply(solver: Solver)(univ: Term)(implicit stack: Stack) : Option[Term] = univ match {
      case LFModulo.Rewrite(cont, left, right) =>
         val leftI = solver.inferType(left)(stack ++ cont).getOrElse(return None)
         val rightI = solver.inferType(right)(stack ++ cont).getOrElse(return None)
         val eq = solver.checkEquality(leftI,rightI,None)(stack ++ cont)
         if (eq) Some(OMS(Typed.kind)) else None
      case _ => None // impossible
   } 
}