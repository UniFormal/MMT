package info.kwarc.mmt.lf.itp.Utils.Unifier

import info.kwarc.mmt.api.RuleSet
import info.kwarc.mmt.api.checking.{CheckingUnit, Solver}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects.Context.context2list
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.lf.itp.Utils.TermUtil

/**
  * the goal of the [[Unifier]]-object is to provide a cleaner way to unfiy two terms using MMT's [[Solver]]
  */
object Unifier {
  /**
    * use the unifier in latin2 instead
    * @param t
    * @param uks
    * @param query
    * @param c
    * @param cctx
    * @param rls
    * @param lctx
    * @return
    */
  def apply(t: Term, uks: Context, query: Term, c: Controller, cctx: Context, rls: RuleSet, lctx: Context): Option[(Context , Term)] = {
    // sometimes the matcher can find unifications that the MMT-solver can't (usually it, the matcher, not caring whether a solution is unique)
    val mtcher = new Matcher(c, rls )
    val names = TermUtil.getNotLocallyBoundNames(t)

    val alphauks = Context.makeFresh( uks , names ++ lctx.map(_.name)  )
    val alphaquery = query.substitute(alphauks._2)(PlainSubstitutionApplier)

    mtcher.apply(lctx, t, alphauks._1, alphaquery) match {
      case MatchFail =>
      case MatchSuccess(solution, false) => {

      }
      case MatchSuccess(solution, true) => {
        return {
          val alphasol = solution.map(ss => {val tmp = alphauks._2.find(_.target.asInstanceOf[OMV].name == ss.name) ; VarDecl(tmp.get.name, df = ss.target)})
          val solctx  =  Context.list2context(alphasol)
          val solt = alphaquery.substitute(solution)(PlainSubstitutionApplier)
          Some((solctx, solt))
        }
      }
    }

    // the solver-unifier is just an Equality check incorporating unknowns
    val cu = CheckingUnit(None, cctx , alphauks._1 , null )

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
