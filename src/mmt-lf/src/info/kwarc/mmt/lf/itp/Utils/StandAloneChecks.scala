package info.kwarc.mmt.lf.itp.Utils

import info.kwarc.mmt.api.checking.{CheckingUnit, History, Solver, SolverState}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.{LocalName, MPath, RuleSet}

object StandAloneChecks {
  def standAloneEqCheck(s: Solver, uks: List[(LocalName, Term)], localctx: Context, tm: Term, tm0: Term) = {
    val clondestate = new SolverState() //.copyValues(s.currentStateObj.getCurrentState)
    clondestate.copyValues(s.state.getCurrentState)
    val clones = new Solver(s.controller, s.checkingUnit, s.rules, Some(clondestate))
    clones.addUnknowns(uks.map(f => VarDecl(f._1, tp = f._2)), None)
    clones.check(Equality(Stack(localctx), tm, tm0, None))(new History(Nil))
  }

  def standAloneEqCheck(c: Controller, mp: MPath, uks: List[(LocalName, Term)], localctx: Context, tm: Term, tm0: Term) = {
    val cu = new CheckingUnit(None, Context(mp), uks.map(f => VarDecl(f._1, tp = f._2)), null)
    val rules = RuleSet.collectRules(c, Context(mp))
    val clones = new Solver(c, cu, rules, None)
    clones.check(Equality(Stack(localctx), tm, tm0, None))(new History(Nil))
  }

  def standAloneInfer(t : Term , c : Controller , mp : MPath , uks : Context , lctx : Context) : (Context , Term) = {
    val cu = new CheckingUnit(None , Context(mp) , uks , null)
    val rules = RuleSet.collectRules(c, Context(mp))
    val sol = new Solver(c, cu , rules , None)
    val Some(res) = sol.inferType(t , false )(Stack(lctx) , new History(Nil))
    (sol.getPartialSolution , res )
  }
}
