package info.kwarc.mmt.lf.itp.Utils.Unifier

import info.kwarc.mmt.api.checking.{CheckingUnit, Solver}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.{Context, Equality, Stack, Term}
import info.kwarc.mmt.api.{MPath, RuleSet}

object MoreGeneralUnifier {
  def apply(t : Term ,  t0 : Term , uks : Context , uks0 : Context,    c : Controller , pth : MPath , cctx : Context, rls : RuleSet, localContext : Context): Option[(Context , Term)] = {
    val cu = CheckingUnit(None, cctx , uks ++ uks0 , null )
    // val rules = RuleSet.collectRules(c, Context(pth))
    val solver = new Solver(c, cu, rls)
    val res = solver.apply(Equality( Stack(localContext),  t, t0 ,None ))

    if (res && !solver.hasUnresolvedConstraints && ! solver.hasUnsolvedVariables && solver.getErrors.isEmpty){
      val tmp = solver.getPartialSolution
      val tres = solver.substituteSolution(t)
      Some((tmp , tres))
    }else {
      None
    }
  }
}
