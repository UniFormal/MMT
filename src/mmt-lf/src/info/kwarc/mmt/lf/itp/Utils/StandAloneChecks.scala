package info.kwarc.mmt.lf.itp.Utils

import info.kwarc.mmt.api.checking.{CheckingUnit, History, Solver, SolverState}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.{LocalName, MPath, RuleSet}

/**
  * use the standalone checks in latin2
  * object that holds several functions that can be used for executino functions that incorporate a solver without modifying it
  */
object StandAloneChecks {
  /**
    * check that two term are equal
    * @param s
    * @param uks
    * @param localctx
    * @param tm
    * @param tm0
    * @return
    */
  def standAloneEqCheck(s: Solver, uks: List[(LocalName, Term)], localctx: Context, tm: Term, tm0: Term) = {
    val clondestate = s.getCurrentState().copy() //.copyValues(s.currentStateObj.getCurrentState)
    val clones = new Solver(s.controller, s.checkingUnit, s.rules, Some(clondestate))
    clones.addUnknowns(uks.map(f => VarDecl(f._1, tp = f._2)), None)
    clones.check(Equality(Stack(localctx), tm, tm0, None))(new History(Nil))
  }

  /**
    * check that two terms are equal
    * @param c needed for the construction of the temporary
    * @param mp
    * @param uks
    * @param localctx
    * @param tm
    * @param tm0
    * @return
    */
  def standAloneEqCheck(c: Controller, mp: MPath, uks: List[(LocalName, Term)], localctx: Context, tm: Term, tm0: Term) = {
    val cu = new CheckingUnit(None, Context(mp), uks.map(f => VarDecl(f._1, tp = f._2)), null)
    val rules = RuleSet.collectRules(c, Context(mp))
    val clones = new Solver(c, cu, rules, None)
    clones.check(Equality(Stack(localctx), tm, tm0, None))(new History(Nil))
  }

  /**
    *
    * @param t term to infer the type of
    * @param c controller needed for
    * @param mp module path needed for the construction of the temporary solver
    * @param uks needed
    * @param lctx
    * @return
    */
  def standAloneInfer(t : Term , c : Controller , mp : MPath , uks : Context , lctx : Context) : (Context , Term) = {
    val cu = new CheckingUnit(None , Context(mp) , uks , null)
    val rules = RuleSet.collectRules(c, Context(mp))
    val sol = new Solver(c, cu , rules , None)
    val Some(res) = sol.inferType(t , false )(Stack(lctx) , new History(Nil))
    (sol.getPartialSolution , res )
  }
}
