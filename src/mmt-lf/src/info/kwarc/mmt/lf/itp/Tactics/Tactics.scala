package info.kwarc.mmt.lf.itp.Tactics

import info.kwarc.mmt.api.LocalName
import info.kwarc.mmt.api.objects.{Context, Equality, Free, OMID, OMV, PlainSubstitutionApplier, Stack, Sub, Substitution, Term, Typing, VarDecl}
import info.kwarc.mmt.api.proving.itp.ProofUtil.standAloneInfer
import info.kwarc.mmt.api.proving.itp.{Goal, HasError, Msg, NoMsg, Proof, WarningMsg}
import info.kwarc.mmt.lf.itp.{InteractiveProof, ProofUtil, Tactic, TacticError, TacticParser}
import info.kwarc.mmt.lf.itp.ProofUtil.{Appremover, dryRunAllowErrors, makeSubgoals}
import info.kwarc.mmt.lf.{Apply, ApplySpine, Arrow, Lambda, Pi}


/**
  * takes unnamed hypothesis from the goal an puts them in the local context
  * |- x -> y  ==> hs:x |- y
  * @param hs optional names for the newly introduced hypothesis
  */
case class assume(hs : Option[String]) extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val Goal(g,  ctx, ukn )  = p.currentState.head
    g match {
      case Arrow(h, t) => {
        val old = p.currentState.remove(0)
        val newname = ProofUtil.pickFreshHypName(hs.getOrElse("h") , ctx)
        val newh = VarDecl(LocalName(newname) ,tp = h)
        p.currentState.insert(0, Goal(t ,  ctx ++ newh, ip.pr.getNewGoalName()) )
        if (!ip.testrun) genTerm(old , newname , h , t , ip)
        NoMsg()
      }
      case _ => HasError("assume has to be applied to a goal of the form a -> b")
    }
  }

  /**
    * update the proof terms
    * @param old old goal
    * @param s hypotehsis name
    * @param tp type of new hypothesis
    * @param bd new goal
    * @param ip
    */
  def genTerm( old : Goal , s : String, tp : Term ,bd : Term  ,ip : InteractiveProof) = {
    val g : Goal = ip.pr.getCurrentGoal
    ip.slvr.addUnknowns(Context(VarDecl(LocalName(g.ukname) , Free(g.ctx  , bd) , null)) , None)
    val newl = Lambda( Context(VarDecl(LocalName(s) , tp ,null ))   , g.makeUnknown(ip.slvr , g.ukname))
    ip.slvr.solve(old.ukname , Free( old.ctx ,newl)) (ip.hist)
    ip.pr.updateGoalProofTerm(old.ukname , Lambda( Context(VarDecl(LocalName(s) , tp ,null ))   , OMV(g.ukname)))
  }
}

/**
  * solves a goal
  * @param t term to used the first goal in [[Proof.currentState]]
  * @param uks newly introced unknowns
  */
case class use(t : Term, uks : Context) extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val sol = ip.slvr.getSolvedVariables
    if (uks.variables.exists(p => ! sol.contains(p.name))) return HasError("the term" + ip.slvr.presentObj(t) + " must not contain unkowns/holes")
    val res = ip.slvr.check(Typing(Stack(p.getCurrCtx) , t , p.getCurrentConc))(ip.hist)
    if (res) {
      val old  = p.currentState.remove(0)
      if(!ip.testrun) genTerm(old, t , ip)
      NoMsg()
    }else {
      HasError("term " + ip.slvr.presentObj(t) + " is not the same type as the goal")
    }
  }

  /**
    * updates the proof terms
    * @param old old goal
    * @param t
    * @param ip
    */
  def genTerm(old : Goal , t  :  Term , ip : InteractiveProof) = {
    ip.slvr.solve(old.ukname , Free(old.ctx  , t))(ip.hist)
    ip.pr.updateGoalProofTerm(old.ukname ,t )
  }
}

/**
  * adds hypothesis to local goal context
  * @param h hypothesis name
  * @param tup tuple consisting of the term to add to the context and the unknown it introduces
  */
case class let(h : String , tup : (Term,Context) ) extends Tactic {
  val (t , unk) = tup
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val currg@Goal(g , ctx , ukn) = p.currentState.head
    //  val sub = Substitution(unk.map(v => Sub(v.name, currg.makeUnknown(ip.slvr , (v.name)))) : _*)
    //  val newt = t.substitute(sub)(PlainSubstitutionApplier)
    val newhtp0 = ip.slvr.inferType(t , false)(Stack(ctx) , ip.hist)
    if (newhtp0.isEmpty) return HasError("error in let tactic: Could not infer the type of " + t.toString)
    val old = p.currentState.remove(0)
    val newh  = ProofUtil.pickFreshHypName(h , ctx)
    //   val newunk : Substitution = Substitution(unk.map(v => Sub(v.name, old.makeUnknown(ip.slvr , (v.name)))) : _*)
    val newhtp = newhtp0.get // .substitute(newunk)(PlainSubstitutionApplier)
    p.currentState.insert(0,Goal(g,  ctx ++ VarDecl(LocalName(newh) ,  newhtp )  , ip.pr.getNewGoalName()))
    genTerm(old, LocalName(newh), newhtp, t , ip)
    ip.pr.addukdeps(unk, ukn, LocalName(newh))
    NoMsg()
  }

  /**
    * update the proof terms
    * @param old
    * @param hname
    * @param tp
    * @param ref
    * @param ip
    */
  def genTerm(old : Goal, hname : LocalName , tp : Term, ref : Term , ip : InteractiveProof ) = {

    //   val revisedtp = unk.variables.foldLeft(tp)((res ,v) => res.substitute(Sub(v.name, Free(old.ctx , OMV(v.name))))(PlainSubstitutionApplier))
    val g = ip.pr.getCurrentGoal
    val gname = ip.pr.getCurrentGoal.ukname
    val newlam = Apply (Lambda(hname , tp ,  g.makeUnknown(ip.slvr , gname) )  , ref)
    ip.slvr.addUnknowns(Context(VarDecl(gname , Free(g.ctx , g.g) , null)) , None)
    ip.slvr.solve(old.ukname , Free(old.ctx , newlam))(ip.hist)
    ip.pr.updateGoalProofTerm(old.ukname, Apply (Lambda(hname , tp ,  OMV(gname) )  , ref))
  }
}


/**
  * applies a term to a hypothesis
  * @param h
  * @param lstup
  */
case class applyT(h : String , lstup : List[(Term, Context)] ) extends Tactic {
  val ls = lstup.map(_._1)

  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val c = p.getCurrentGoal.ctx.getO(h)
    c match {
      case None => HasError("no hypothesis named " + h + " found")
      case Some(vd@VarDecl(nm, _, tp, _, _)) => {
        val tmp = vd.toTerm
        val tmp0 = ip.slvr.inferType(ApplySpine(tmp, ls: _*))(Stack(ip.pr.getCurrentGoal.ctx), ip.hist)
        tmp0 match {
          case None => HasError("could not apply " + ls.toList + " to " + h)
          case Some(newtp) => {
            val newctx0  = ip.pr.getCurrCtx.filter(x => x.name != LocalName(h))
            val hyp = ip.pr.getCurrCtx(LocalName(h))
            val newhyp = VarDecl(hyp.name, hyp.feature, tmp0, hyp.df, hyp.not)
            val newctx = Context(newctx0 : _*) ++ newhyp
            val newG = Goal(ip.pr.getCurrentConc, newctx, ip.pr.getNewGoalName())
            val old = ip.pr.getCurrentGoal
            ip.pr.update(newG)
            genTerm(old , LocalName(h)  , newtp , ls , ip)
            NoMsg()
          }
        }
      }
    }

  }

  /**
    * update the proof terms
    * @param old
    * @param ln
    * @param tp
    * @param ctx
    * @param ip
    */
  def genTerm(old : Goal , ln : LocalName, tp : Term , ctx : List[Term] , ip : InteractiveProof) = {
    val g = ip.pr.getCurrentGoal
    val newLam = Apply(Lambda( ln , tp , g.makeUnknown(ip.slvr , g.ukname)) , (ApplySpine(OMV(ln) ,  ctx : _*)))
    val newLamA = Apply(Lambda( ln , tp ,  OMV(g.ukname)) , (ApplySpine(OMV(ln) ,  ctx : _*)))
    ip.slvr.addUnknowns(VarDecl(g.ukname , Free(g.ctx, g.g) , null) , None)
    ip.slvr.solve(old.ukname , Free(old.ctx , newLam ))(ip.hist)
    ip.pr.updateGoalProofTerm(old.ukname,  newLamA)
  }
}


/**
  * performs a backward step
  * @param tup
  */
case class backward(tup : (Term, Context)) extends Tactic {
  val (t, unk) = tup
  val unks = unk.filter(p => p.df.isDefined)
  val unku = unk.filter(p => p.df.isEmpty)

  def mergeVarWithG(ctx : List[VarDecl] , gls : List[Goal] ) : List[(VarDecl , Option[Goal])]  = (ctx , gls) match {
    case (Nil , Nil) => Nil
    case (x :: xs , ys) if x.df.isDefined => (x , None) :: mergeVarWithG(xs , ys)
    case (x :: xs , y :: ys) => (x , Some(y)) :: mergeVarWithG(xs , ys)
    case _ => throw  new TacticError("internal error in backward tactic during unification: #goals <> #unsolved variables " )
  }

  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val Goal(g,gctx, guks) = p.getCurrentGoal

    val htp = ip.slvr.inferType(t)(Stack(gctx), ip.hist)
    if (htp.isEmpty) return HasError("could not infer the type of " + t.toString)
    val makegs = try {makeSubgoals(gctx , unku , g ,  htp.get , ip.slvr).get} catch {case e => return HasError("could not unify goal") }
    val hs = makegs.filter(p  => p.df.isEmpty)
    val solved = makegs.filter(p => p.df.isDefined)
    //   val (hs , c) = ProofUtil.splitConcHyps(htp.get)
    //  if(ip.slvr.check(Equality(Stack(gctx) , c , g, None))(ip.hist)){
    val old = p.currentState.remove(0)
    val newgls = hs.map(x => Goal(x.tp.get, gctx, ip.pr.getNewGoalName() ))
    p.currentState.insertAll(0, newgls)
    genTerm(old, t, mergeVarWithG(makegs.variables.toList , newgls ) , newgls , ip)
    NoMsg()
    //  }
    //   HasError("could not unify goal with conclusion")
  }

  def genTerm(old : Goal , t : Term, params : List[(VarDecl , Option[Goal])] , newgls : List[Goal] , ip : InteractiveProof )  = {

    val newlam = ApplySpine( t , params.map(x => if (x._1.df.isEmpty) {old.makeUnknown(ip.slvr , x._2.get.ukname)} else {x._1.df.get} ) : _* )
    val newlamA = ApplySpine( t , params.map(x => {if (x._1.df.isEmpty) {OMV(x._2.get.ukname)} else {x._1.df.get}}) : _* )
    ip.slvr.addUnknowns(Context(newgls.map(x => VarDecl(x.ukname , Free(x.ctx, x.g) , null)) : _*) , None)
    ip.pr.updateGoalProofTerm(old.ukname , newlamA)
    ip.slvr.solve(old.ukname , Free(old.ctx , newlam))(ip.hist)


  }
}


/**
  * introduce a pi to the local context
  * @param hs
  */
case class fix(hs : Option[String]) extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val Goal(g,  ctx, uks )  = p.currentState.head
    g match {
      case Pi(nmm , nmtp, bd) => {
        val nm  = if (nmm.toString == "_") LocalName(ProofUtil.pickFreshHypName("h" , ip.pr.getCurrCtx)) else nmm
        val old  = p.currentState.remove(0)
        val newname = ProofUtil.pickFreshHypName(hs.getOrElse(nm.toString) , ctx)
        val newh = VarDecl(LocalName(newname) ,tp = nmtp)
        val newctx = ctx ++ newh
        val newg =bd.substitute(Substitution(Sub(nm , OMV(newname))))(PlainSubstitutionApplier)
        p.currentState.insert(0, Goal(newg ,  newctx, ip.pr.getNewGoalName()) )
        //      val tmp = newctx ++ VarDecl(LocalName("c"))
        if(!ip.testrun) genTerm(old , newname , nmtp,     ip )
        NoMsg()
      }
      case _ => HasError("fix has to be applied to a goal of the form `{a} b` or  `a -> b`")
    }
  }
  def genTerm( old : Goal , s : String, tp : Term  ,ip : InteractiveProof) = {
    val g : Goal = ip.pr.getCurrentGoal
    ip.slvr.addUnknowns(Context(VarDecl(LocalName(g.ukname) , Free(g.ctx  , g.g) , null)) , None)
    val newl = Lambda( Context(VarDecl(LocalName(s) , tp ,null ))   , g.makeUnknown(ip.slvr , g.ukname))
    ip.slvr.solve(old.ukname , Free( old.ctx ,newl)) (ip.hist)
    ip.pr.updateGoalProofTerm(old.ukname , Lambda( Context(VarDecl(LocalName(s) , tp ,null ))   , OMV(g.ukname)))
  }

}

/**
  * introduce multiple PIs to the local context
  * @param hss0
  */
case class fixs(hss0 : Option[List[String]]) extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    def loop(hss : Option[List[String]]) : Msg = {
      val c = p.getCurrentConc
      val pora = ProofUtil.isArrowOrPi(c)


      hss match {
        case None if pora => fix(None).applyToProof(p, ip) match {
          case r@HasError(s) => r
          case r@WarningMsg(s) => {
            r.combineMsg(loop(None))
          }
          case NoMsg() => loop(None)
        }
        case None => NoMsg()
        case Some(x :: xs) if pora => fix(Some(x)).applyToProof(p, ip) match {
          case HasError(s) => HasError(s)
          case r@WarningMsg(s) => r.combineMsg(loop(Some(xs)))
          case NoMsg() => loop(Some(xs))
        }
        case Some(Nil) => NoMsg()
        case Some(x :: xs) => WarningMsg("more names for hypothesis supplied then needed")
      }
    }
    loop(hss0)
  }
}


/**
  * start a new subproof
  * @param h
  * @param tup
  */
case class subproof(h : String , tup : (Term, Context)) extends Tactic{
  val (t, unk) = tup
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val Goal(g, ctx, uks) = p.getCurrentGoal
    val newh = ProofUtil.pickFreshHypName(h ,p.getCurrCtx)
    val old = p.getCurrentGoal
    p.update(Goal(g, ctx ++ VarDecl(LocalName(newh) , None , Some(t) , None,  None), ip.pr.getNewGoalName()))
    p.currentState.insert(0 , Goal(t,  ctx, ip.pr.getNewGoalName()))
    genTerm( old , p.currentState(1)  , LocalName(newh) , t , ip )
    NoMsg()
  }

  def genTerm(old : Goal , updatedgoal : Goal , ln: LocalName ,tp : Term ,  ip :InteractiveProof) = {
    val g  = ip.pr.getCurrentGoal
    val newlamA = Apply(Lambda( ln , tp , OMV(updatedgoal.ukname) )  , OMV(g.ukname))
    val newlam = Apply(Lambda( ln , tp , updatedgoal.makeUnknown(ip.slvr , updatedgoal.ukname) )  , g.makeUnknown(ip.slvr,g.ukname))
    ip.slvr.addUnknowns(Context(VarDecl(updatedgoal.ukname  , Free(updatedgoal.ctx , updatedgoal.g) , null) , VarDecl(g.ukname , Free(g.ctx, g.g) , null)) , None)
    ip.slvr.solve(old.ukname , Free(old.ctx , newlam))(ip.hist)
    ip.pr.updateGoalProofTerm(old.ukname , newlamA)
  }
}

/**
  * changes the order of goals by putting the ith goal first
  * @param i
  */
case class prefer(i : Int) extends Tactic{
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    (ip.pr.currentState.length < i, i < 0) match {
      case (true, _) => HasError("the index for the prefer tactic is larger then the number of available goals (counting starts from zero)")
      case (_ , true) => HasError("the index for the prefer tactic has to be a positive number")
      case _ => {
        val pg = ip.pr.currentState.remove(i)
        ip.pr.currentState.insert(0,pg)
        NoMsg()
      }
    }
  }
}

case class repeat(n : Option[Int] , t : Tactic)  extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = ???
}


case class applyto(n : List[Int] , t : Tactic) extends Tactic{
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = ???
}

case class revert(h : List[String]) extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = ???
}

case class autostep() {

}

/*
case class undo() extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    if(ip.history.isEmpty) {
      WarningMsg("no undo possible")
    }else{
      ip.undo
      NoMsg()
    }
  }
}
*/

/**
  * unfolds a definiition
  * @param h
  * @param target
  * @param pos
  */
case class unfold(h : String, target : Option[String] , pos : Option[String]) extends  Tactic{
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val poss  = pos.map(s => s.toInt)
    val Goal(g , gctx, guks) = ip.pr.getCurrentGoal
    val prs = new TacticParser(ip)
    val (t ,unk) = prs.currGoalParseTerm(h)
    if (unk.nonEmpty) return HasError("term to unfold contains unknowns , which it should not")
    val res = t match {
      case omid : OMID => {
        ip.slvr.getDef(omid.path.toMPath.toGlobalName)(ip.hist) match {
          case None => return HasError("could not find definition for " + omid.toString)
          case r@Some(_) => r
        }
      }
      case omv : OMV => {
        gctx.getO(omv.name) match {
          case None => None
          case Some(dd) => Some(dd.df.getOrElse( return HasError("could not find definition for " + dd.name.toString)))
        }
      }
      case _ => None
    }
    val newg = (res,target) match {
      case (None, _) => return HasError("identifier " + h + " has no definition to unfold")
      case (Some(df), None) => {
        val gg = new ProofUtil.SubTraverser(t, df , ((x , y , addc) =>  x == y ), poss).traverse(g)(p.getCurrCtx, ()) // ProofUtil.substitute(g, t , df, ((x , y , addc) =>  x == y ) )
        Goal(gg, gctx , guks)
      }
      case (Some(df), Some(tg)) => {
        if (gctx.getO(tg).isEmpty) return HasError("no such hypothesis named " + tg)
        val newctx = gctx.map(x => {
          if(x.name == LocalName(tg)) {
            val newtp =  new ProofUtil.SubTraverser(t, df , ((x , y , addc) =>  x == y ) , poss).traverse(x.tp.get)(p.getCurrCtx, ()) /// ProofUtil.substitute(x.tp.get, t , df, ((x , y , addc) =>  x == y ) )
            x.copy(tp = Some(newtp))
          } else x
        })
        Goal(g , newctx, guks)
      }
    }
    ip.pr.update(newg)
    NoMsg()
  }
}

/**
  * delete a hypothesis from the local context
  * @param h
  */
case class delete(h : String) extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val old@Goal(g, gctx, guks) = p.getCurrentGoal
    val gtmp = g.freeVars
    if (gtmp.contains(LocalName(h))) return HasError("hypothesis " + h + " is used in conclusion")
    val tmpctx = gctx.filter(p => p.name != LocalName(h))
    tmpctx.foreach(p => {
      val dff = p.df.map(t => t.freeVars).getOrElse(Nil)
      val tpf = p.tp.map(t => t.freeVars).getOrElse(Nil)
      if (dff.contains(LocalName(h))) return HasError(h + " is used in definition of " + p.name)
      if (tpf.contains(LocalName(h))) return HasError(h + " is used in type of " + p.name)
    })
    val newgname = p.goalcounter
    val newg = Goal(g, Context(tmpctx : _*), guks)
    p.update(newg)
    genTerm()
    NoMsg()
  }


  def genTerm() = {

  }
}
/*
case class Try(t : Tactic) extends Tactic  {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    try {
      ip.saveCurrState
      val res = t.applyToProof(p,ip)
      res match {
        case NoMsg() => NoMsg()
        case WarningMsg(_) => res
        case HasError(s) => {
          ip.undo
          WarningMsg(s)
        }
      }
    }
    catch {
      case e => {
        ip.undo
        WarningMsg(e.toString)
      }
    }
  }
}

 */

/*

case class setunkown(h : String , tup : ( Term, Context)) extends Tactic {
  val (t, _)  = tup
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val g@Goal(gl , ctx , uks) = p.getCurrentGoal
    val uk = uks.filter(x => x.ln.name.toString == h)
    uk match {
      case Nil => WarningMsg("setunkown: no such unkown " + h)
      case (x::Nil) => {
        val tmp = ip.slvr.check(Equality(Stack(ip.pr.getCompleteCtx) ,x.toUnkown  , t, None))(ip.hist)
        if (tmp) {NoMsg()} else {HasError("setunkown: could not set " + h + " to " + t.toString)}
      }
    }
  }
}

*/

/**
  * skip the currently focused proof
  */
case class ignore() extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    ip.pr.currentState.remove(0)
    WarningMsg("ignoring current goal, skipping it without proof")
  }
}

//TODO: replace with a tactic that only combines two tactics. also due to time constraints it was necessary that [[multiTactic]] does parsing which should not be the case
/**
  * tactic for chaining multiple tactics as in "fix x; assume h; use h"
  * @param ls
  * @param tp
  */
case class multiTactic(ls : List[String] , tp : TacticParser) extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {

    def loop(lss : List[String]): Msg = lss match {
      case Nil => NoMsg()
      case x :: xs => {
        val tmp = tp.parseAll(tp.parseSingleTactic, x)
        tmp.getOrElse(return HasError(tmp.toString)).applyToProof(p, ip) match {
          case NoMsg() => p.updateUnknowns(ip.slvr) ; loop(xs)
          case m@WarningMsg(s) => {
            p.updateUnknowns(ip.slvr)
            m.combineMsg(loop(xs))
          }
          case m@HasError(s) => m
        }
      }
    }
    loop(ls)
  }
}


/**
  * simplifies terms
  * @param tup
  * @param h
  */
case class simp(tup : Option[(Term,Context)], h : Option[String]) extends Tactic {
  lazy val Some((t,cc)) = tup
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    lazy val res = ip.slvr.simplify(t)(Stack(ip.pr.getCurrCtx) , ip.hist)
    val g@Goal(gg , ctx, uks) = ip.pr.getCurrentGoal
    (h,tup.isDefined) match {
      case (None,false) => {
        val newg = ip.slvr.simplify(gg)(Stack(ip.pr.getCurrCtx) , ip.hist)
        ip.pr.update(Goal(newg , ctx , uks))
        NoMsg()
      }
      case (None,true) => {
        val rep  = ProofUtil.substitute(gg , t , res, ((x,y,addc) => dryRunAllowErrors(ip.slvr)(ip.slvr.check(Equality(Stack(ctx ++ addc) , x , y, None)) (ip.hist))))
        ip.pr.update(Goal(rep , ctx, uks))
        NoMsg()
      }
      case (Some(nm),true) => {
        if (ctx.getO(nm).isEmpty) return HasError("no such hypothesis named " + nm)
        val newctx = ctx.map(x =>{
          if (x.name.toString == nm ){
            val newtp = ProofUtil.substitute(x.tp.get , t , res, ((x,y,addc) => dryRunAllowErrors(ip.slvr)(ip.slvr.check(Equality(Stack(ctx ++ addc) , x , y, None)) (ip.hist))))
            x.copy(tp = Some(newtp))
          }else {
            x
          }
        })
        ip.pr.update(Goal(gg, newctx, uks ))
        NoMsg()
      }
      case (Some(nm),false) => {
        if (ctx.getO(nm).isEmpty) return HasError("no such hypothesis named " + nm)

        val newctx = ctx.map(x =>{
          if (x.name.toString == nm ){
            val simptp  = ip.slvr.simplify(x.tp.get)(Stack(ip.pr.getCurrCtx) , ip.hist)
            x.copy(tp = Some(simptp))
          }else {
            x
          }
        })
        ip.pr.update(Goal(gg, newctx, uks ))
        NoMsg()
      }
    }
  }
}



case class fold(t : Term) extends Tactic { //fold a term back into a name
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {???}
}

case class addunknown(u : String) extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val Goal(g , ctx, uks ) = p.getCurrentGoal
    ip.slvr.addUnknowns(Context(VarDecl(LocalName(u))), None )
    // p.update(Goal(g , ctx , (p.getCurrentGoal.makeMiss(LocalName(u))) :: uks ))
    NoMsg()
  }
}


/**
  * does nothing, useful for debugging
  */
case class void() extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    NoMsg()
  }
}

/**
  * prints a message
  * @param s
  */
case class message(s : String) extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    WarningMsg(s)
  }
}

/**
  * progresses by directly building the proof term
  * @param t0
  * @param uks
  */
case class build(t0 : Term , uks : Context) extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val (newuks, subss) = Context.makeFresh(uks , (p.getCurrCtx ++ ip.slvr.getPartialSolution).map(_.name))
    ip.slvr.addUnknowns(newuks , None)
    val t = t0.substitute(subss)(PlainSubstitutionApplier)
    val lam : Term = new Appremover(newuks)(t, p.getCurrCtx)
    lazy val subs = newuks.map(x => new ProofUtil.ExtendUnkownInTerm( x.name , p.getCurrCtx ))
    val newt = if (p.getCurrCtx.isEmpty) t else subs.foldLeft(t)((tres , sub) => sub(tres , p.getCurrCtx))
    val tmp = ip.slvr.check(Typing(Stack(p.getCurrCtx) , newt , p.getCurrentConc))(ip.hist)
    if (!tmp){return HasError("could not tie type of goal to the given term " + ip.slvr.presentObj(t))}
    ip.slvr.getPartialSolution.foreach(f => if (f.df.isDefined && f.tp.isEmpty){
      val tmp  = ip.slvr.inferType(f.df.get , false)(Stack(p.getCurrCtx) , ip.hist)
      if (tmp.isEmpty) HasError("could not infer type of newly one or more of the introduced holes/goals")
      val tmp0 = ip.slvr.solveTyping(Typing(Stack(p.getCurrCtx)  , f.toTerm, tmp.get))(ip.hist)
      if (! tmp0) return HasError("could not solve type for " + f.name)
    })
    val newgs0 = ip.slvr.getPartialSolution.filter(p => newuks.getO(p.name).isDefined)
    if (newgs0.exists(p => p.tp.isEmpty)) return HasError("could not infer type of newly one or more of the introduced holes/goals")
    val newgs = newgs0.map(x => {
      val (tm , addctx)  = ProofUtil.remOuterFreeA(x.tp.get , p.getCurrCtx)
      Goal(tm , p.getCurrCtx ++ addctx , x.name)
    })
    val old = p.currentState.remove(0)
    p.currentState.insertAll(0, newgs)
    genTerm(old, newt , lam , ip)
    NoMsg()

  }

  def genTerm(old : Goal , newt : Term, newtt : Term ,ip : InteractiveProof) = {
    ip.slvr.solve(old.ukname , Free(old.ctx , newt))(ip.hist)
    ip.pr.updateGoalProofTerm(old.ukname , newtt)
  }
}

case class getdef() {

}

/**
  * get the type of a term
  * @param t
  * @param uks
  */
case class gettype(t : Term  , uks : Context) extends  Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val tp = standAloneInfer(t , uks , p.getCurrCtx, ip.hist, ip.slvr)
    val tmp = WarningMsg(ip.slvr.presentObj(tp._1.getOrElse(return HasError("Could not infer the type of " + ip.slvr.presentObj(t)))))
    val tmp0 = if (tp._3.nonEmpty) tmp.combineMsg(HasError("generated errors:\n" + tp._3.toString())) else tmp
    val tmp1 = if (tp._4.nonEmpty)  tmp0.combineMsg(HasError("generated constraints:\n" + tp._4.toString())) else tmp0
    tmp1
  }
}

/**
  * like [[gettype]] but prints the type fully qualified
  * @param t
  * @param uks
  */
case class gettyperaw(t : Term  , uks : Context) extends  Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    val tp = standAloneInfer(t , uks , p.getCurrCtx, ip.hist, ip.slvr)
    WarningMsg(tp._1.getOrElse(return HasError("Could not infer the type of " + ip.slvr.presentObj(t))).toString())
  }
}

/**
  * print a term fully qualified form of the term t. Example: "a + b" will be printed as   "lf?apply sometheory/mathops?add  sometheory/constants?a sometheory0/constans?b"
  * @param t
  */
case class printraw(t : Term) extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    WarningMsg(t.toString)
  }
}

/**
  * executes a forward step
  * @param hypname
  * @param t
  * @param uks
  * @param tms
  */
case class forward(hypname : String , t : Term , uks : Context, tms : List[(Term,Context)]) extends Tactic {
  override def applyToProof(p: Proof, ip: InteractiveProof): Msg = {
    //  val res = ip.slvr.asInstanceOf[ExtendedSolver].termMatcher(p.getCurrCtx , Context() , t )
    ProofUtil.simpleApplyTermToTerm(p.getCurrCtx  , tms.map(x => x._1) , t , ip.hist , ip.slvr) match {
      case Some((gls, trm , newhyp)) => {
        val gnames = gls.map(f => f.ukname) ::: ip.slvr.getPartialSolution.map(f => f.name)
        val old@Goal(g, gctx, u) = p.currentState.remove(0)
        val newhname = ProofUtil.pickFreshHypName(hypname, gctx)
        var newgname = LocalName("", "goal")
        while (gnames.contains(newgname)){
          newgname = p.getNewGoalName()
        }
        p.currentState.insertAll(0, gls )
        p.currentState.insert(0 , Goal(g , gctx ++ VarDecl(n = LocalName(newhname), tp = newhyp) , newgname))


        genTerm(old, gls ,  trm , LocalName(newhname) , newhyp, ip)
        NoMsg()
      }
      case None => HasError("could not apply " + ip.slvr.presentObj(tms.head._1) + " to " + ip.slvr.presentObj(t))
    }

  }

  def genTerm(old : Goal, gls : List[Goal],  hyptrm : Term , hypname : LocalName, hypType : Term , ip : InteractiveProof ) = {
    val curr@Goal(g, gctx, gname) = ip.pr.getCurrentGoal
    val newlam  = Apply(Lambda(hypname , hypType ,  OMV(gname) ) , hyptrm)
    val newlam0 = Apply(Lambda(hypname , hypType , curr.makeUnknown(ip.slvr, gname) ) , hyptrm)
    val glsctx = Context(VarDecl(n = gname , tp = Free(gctx ,g))  :: gls.map(x => VarDecl(n = x.ukname , tp = Free(old.ctx ,x.g))) : _ *)
    ip.slvr.addUnknowns( glsctx, None)
    ip.slvr.solve(old.ukname , Free(old.ctx , newlam0))(ip.hist)
    ip.pr.updateGoalProofTerm(old.ukname, newlam)
  }
}