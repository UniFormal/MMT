package info.kwarc.mmt.lf.itp.Utils.Unifier

import info.kwarc.mmt.api.RuleSet
import info.kwarc.mmt.api.checking.{CheckingUnit, Solver}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.Context.context2list
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.itp.Utils.TermUtil

object Unifier {
  def apply(t: Term, uks: Context, query: Term, c: Controller, cctx: Context, rls: RuleSet, lctx: Context): Option[(Context , Term)] = {
    val mtcher = new Matcher(c, rls )
    val names = TermUtil.getNotLocallyBoundNames(t)
    /// val (ndup, dup) = uks.partition(names.contains(_))
    val alphauks = Context.makeFresh( uks , names ++ lctx.map(_.name)  )
    val alphaquery = query.substitute(alphauks._2)(PlainSubstitutionApplier)

    mtcher.apply(lctx, t, alphauks._1, alphaquery) match {
      case MatchFail =>
      case MatchSuccess(solution, false) => {
        val dbg = ()
      }
      case MatchSuccess(solution, true) => {
        return {
          val alphasol = solution.map(ss => {val tmp = alphauks._2.find(_.target.asInstanceOf[OMV].name == ss.name) ; VarDecl(tmp.get.name, df = ss.target)})
          val solctx  =  Context.list2context(alphasol)  // Context(alphasol.map(ss => VarDecl(n = ss.name , df = ss.target)) : _*)
          val solt = alphaquery.substitute(solution)(PlainSubstitutionApplier)
          Some((solctx, solt))
        }
      }
    }


    val cu = CheckingUnit(None, cctx , alphauks._1 , null )
    // val rules = RuleSet.collectRules(c, Context(pth))
    val solver = new Solver(c, cu, rls)
    val res = solver.apply(Equality( Stack((lctx)),  t, alphaquery ,None ))

    if (res && !solver.hasUnresolvedConstraints && ! solver.hasUnsolvedVariables && solver.getErrors.isEmpty){
      val tmp = solver.getPartialSolution
      val alphasol = tmp.map(v => {val tmp0 = alphauks._2.find(ss => ss.target.asInstanceOf[OMV].name == v.name).get  ; new VarDecl(tmp0.name , None ,  v.tp , v.df , None   )} )
      val tres = solver.substituteSolution(alphaquery)
      Some((alphasol , tres))
    }else {
      None
    }
  }
}
