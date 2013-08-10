package info.kwarc.mmt.lf.modulo
import info.kwarc.mmt.api._
import objects._
import info.kwarc.mmt.lf._

object RewriteTerm extends InferenceRule(LFModulo.Rewrite.path, OfType.path) {
   def apply(solver: Solver)(univ: Term)(implicit stack: Stack, history: History) : Option[Term] = univ match {
      case LFModulo.Rewrite(cont, left, right) =>
         val leftI = solver.inferType(left)(stack ++ cont, history).getOrElse(return None)
         val rightI = solver.inferType(right)(stack ++ cont, history).getOrElse(return None)
         val eq = solver.check(Equality(stack ++ cont,leftI,rightI,None))
         if (eq) Some(OMS(Typed.kind)) else None
      case _ => None // impossible
   } 
}