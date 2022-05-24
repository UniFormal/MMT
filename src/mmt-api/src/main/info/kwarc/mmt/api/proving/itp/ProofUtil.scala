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


  def standAloneInfer(t : Term , uks : Context , currctx : Context , hist : History, s : Solver) = {
    val cu = new CheckingUnit(None , currctx ,uks , null)
    val exslv = new Solver(s.controller , cu , s.rules)
    val res = exslv.inferType(t , false)(Stack(Context.empty) ,hist)
    (res , exslv.getPartialSolution , exslv.getErrors , exslv.getConstraints)
  }

  def unify(ctx : Context , goal : Term , params : Context , query : Term , hist : History , s : Solver)  = {

    val (newparams , subparams) = Context.makeFresh(params , ctx.variables.map(_.name).toList)
    val revert = subparams.map(x => (x.target.asInstanceOf[OMV].name , x.name))
    val unifier = new Solver(s.controller ,  CheckingUnit(None, s.constantContext  , newparams, null) , s.rules )
    val ures = unifier.apply(Equality(Stack(ctx) , goal , query ^ subparams , None))

    ures && unifier.getConstraints.isEmpty && unifier.getErrors.isEmpty match {
      case false => None
      case true => {
        // val res = unifier.dryRun(false, ((p : Unit) => true))(() : Unit)
        Some(Substitution(unifier.getPartialSolution.toPartialSubstitution.map(x => {val tmp = revert.find(p => p._1 == x.name).get ; x.copy(name = tmp._2) }) : _ *))
      }
    }
  }

/*
  def executeAndReset[A](a: => A, ip : InteractiveProof): A = {
    val tmps = ip.slvr.state
 //   saveCurrState()
    val res = a
  //  undoState()

    res
  }

 */
}
