package info.kwarc.mmt.lf.itp

import info.kwarc.mmt.api.checking.{CheckingUnit, Solver}
import info.kwarc.mmt.api.frontend.Controller
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.proving.itp.Proof
import info.kwarc.mmt.api.{CPath, LocalName, MPath, RuleSet}

/**
  * contains functions to generate new proof manager [[InteractiveProof]]
  */
object ProofGenerator {
  /**
    * generate a new proof manager
    * @param c crontoller used in the proof manager
    * @param mp the MPath that this proof manager belongs to
    * @param cp the content path this proof manager belongs to
    * @param tp term to be provern
    * @return a new proof manager
    */
  def setupNewProof(c: Controller, mp: Option[MPath], cp: Option[CPath], tp: Term) = {
    val goal = VarDecl(LocalName("", "goal"), tp, null)
    val mpc = if (mp.isEmpty) Context.empty else Context(mp.get)
    val cu = new CheckingUnit(cp, mpc, Context(goal), Inhabitable(Stack(Context()), tp))
    val s = new Solver(c, cu, RuleSet.collectRules(c, mpc))

    val p = new Proof(tp, mpc, tp )
    p.proofTerm = OMV(LocalName("", "goal"))
    p.proofTermAlt = OMV(LocalName("", "goal"))
    Some(new InteractiveProof(s, p, mp))
  }

  /**
    * like [[setupNewProof]] but gets an explicit proof context
    * @param s
    * @param tp
    * @param globalctx
    * @param mp
    * @return
    */
  def setupNewProofA(s : Solver, tp : Term, globalctx : Context , mp : MPath) = {
    val p = new Proof(tp, globalctx, tp)
    p.proofTerm = OMV(LocalName("", "goal"))
    p.proofTermAlt = OMV(LocalName("", "goal"))
    // s.addUnknowns(Context(VarDecl(LocalName("", "goal"), tp, null)) , None)
    val tmp = new InteractiveProof(s, p, Some(mp))
    tmp.slvr.addUnknowns(Context(VarDecl(LocalName("", "goal"), tp, null)) , None)
    Some(tmp)
  }


  def setupNewProofAA(s : Solver, tp : Term) = {

    val (mp , ctx ) = if (s.checkingUnit.component.isEmpty){
      (None , Context.empty)
    }else {
      val (Some(dp) , Some(mn) , _) = s.checkingUnit.component.get.parent.toTriple
      val tmpmp = dp ? mn
      (Some(tmpmp) , Context(tmpmp))
    }

    setupNewProof(s.controller , mp , s.checkingUnit.component , tp)
 /*   val p = new Proof(tp, ctx , tp)
    p.proofTerm = OMV(LocalName("", "goal"))
    p.proofTermAlt = OMV(LocalName("", "goal"))
    // s.addUnknowns(Context(VarDecl(LocalName("", "goal"), tp, null)) , None)
    val tmp = new InteractiveProof(s, p, mp)
    tmp.slvr.addUnknowns(Context(VarDecl(LocalName("", "goal"), tp, null)) , None)
    Some(tmp)


  */

  }
}
