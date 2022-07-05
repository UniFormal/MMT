package info.kwarc.mmt.api.proving.itp

import info.kwarc.mmt.api.checking.{CheckingUnit, History, Solver}
import info.kwarc.mmt.api.frontend.{Controller, Report}
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.presentation.{MMTSyntaxPresenter, NotationBasedPresenter, PresentationContext}

object ProofUtil {
  /**
    * print an MMT-Object with as many, non repeating brackets as possible.
    * @param controller
    * @param report
    * @param o the MMT-Object to be printed
    * @return
    */
  def presentBracketed(controller : Controller ,  report : Report,  o :Obj) : String = {

    val ps = controller.presenter.asInstanceOf[MMTSyntaxPresenter]
    val tmp = ps.objectLevel
    val c = controller
    val r = report
    val b = new NotationBasedPresenter  /* with info.kwarc.mmt.api.frontend.Extension  */ {
      controller = c
      report = r

      override def twoDimensional: Boolean = false

      override def doUnbracketedGroup(body: => Unit)(implicit pc: PresentationContext): Unit = super.doBracketedGroup(body)
    }
    val bpres = new MMTSyntaxPresenter(b)

    bpres.asString(o)
  }

  /**
    * fucntion to infer the type of a term without potentially polluting the main solver
    * @param t term to infer the type of
    * @param uks unknowns that occurre within t
    * @param currctx the surrounding local context (usually the local proof context)
    * @param hist history needed for the infer function to write to
    * @param s the solver to copy
    * @return the potentially inferred type , solved unknowns, errors and remaining/unsolved constraints
    */
  def standAloneInfer(t : Term , uks : Context , currctx : Context , hist : History, s : Solver) = {
    //needed to make a new solver, using null as judgment is an awkward but working workaround
    val cu = new CheckingUnit(None , currctx ,uks , null)
    val exslv = new Solver(s.controller , cu , s.rules)
    val res = exslv.inferType(t , false)(Stack(Context.empty) ,hist)
    (res , exslv.getPartialSolution , exslv.getErrors , exslv.getConstraints)
  }

  /**
    * interface for unifying two terms under a given context
    * @param ctx local context
    * @param goal term to match ageinst
    * @param params the unification variables
    * @param query the term that contains params und sall be unified with goal
    * @param hist
    * @param s
    * @return
    */
  def unify(ctx : Context , goal : Term , params : Context , query : Term , hist : History , s : Solver)  = {

    val (newparams , subparams) = Context.makeFresh(params , ctx.variables.map(_.name).toList)
    val revert = subparams.map(x => (x.target.asInstanceOf[OMV].name , x.name))
    val unifier = new Solver(s.controller ,  CheckingUnit(None, s.constantContext  , newparams, null) , s.rules )
    val ures = unifier.apply(Equality(Stack(ctx) , goal , query ^ subparams , None))

    ures && unifier.getConstraints.isEmpty && unifier.getErrors.isEmpty match {
      case false => None
      case true => {
        Some(Substitution(unifier.getPartialSolution.toPartialSubstitution.map(x => {val tmp = revert.find(p => p._1 == x.name).get ; x.copy(name = tmp._2) }) : _ *))
      }
    }
  }


}
