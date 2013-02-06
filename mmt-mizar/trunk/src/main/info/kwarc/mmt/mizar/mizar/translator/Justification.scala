package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.lf._

object JustificationTranslator {
  
  def translateJustification(j : MizJustification) : Term = j match {
    case i : MizInference => translateInference(i)
    case s : MizSkippedProof => OMHID //TODO
    case p : MizProof => translateProof(p)
  } 
  
  
  def translateInference(i : MizInference) : Term = i match {
    case by : MizBy => translateBy(by)
    case from : MizFrom => translateFrom(from)
    case ei : MizErrorInf => OMHID //TODO
  }
  
  def translateBy(by : MizBy) : Term = {
    val refs = by.refs.map(translateRef)
    Mizar.apply(Mizar.by, refs : _*)
  }
  
  def translateFrom(from : MizFrom) : Term = {
    val refs = from.refs.map(translateRef)
    Mizar.apply(Mizar.from, refs : _*)
  }
  
  def translateRef(ref : MizRef) : Term = ref match {
    case r : MizGlobalRef => 
      val path = MMTUtils.getPath(r.aid, r.kind, r.absnr)
      OMS(path)
    case r : MizLocalRef => 
      TranslationController.resolveProp(r.nr)
  }
  
  def translateProof(p : MizProof) : Term = {
    ReasoningTranslator.translateReasoning(p.reasoning)
  }
  
}

object ReasoningTranslator {
  def translateReasoning(r : MizReasoning) : Term = {
    translateProofSteps(r.proofSteps)
  }
  
  def translateProofSteps(pfSteps : List[MizProofItem]) : Term = pfSteps match {
    case Nil => OMHID //TODO case hd :: Nil ?
    case (hd : MizSkeletonItem) :: tl => translateSkeletonItem(hd, tl)
    case (hd : MizAuxiliaryItem) :: tl => translateAuxiliaryItem(hd, tl)
    case _ => OMHID //TODO
  }
  
  def translateSkeletonItem(sk : MizProofItem, rest : List[MizProofItem]) : Term = sk match {
    case m : MizLet => translateLet(m, rest)
    case m : MizConclusion => translateConclusion(m, rest)
    case m : MizAssume => translateAssume(m, rest)
    case m : MizGiven => translateGiven(m, rest)
    case m : MizTake => translateTake(m, rest)
    case m : MizTakeAsVar => translateTakeAsVar(m, rest)
  }
  
  //Skeleton Items 
  
  //forI
  def translateLet(l : MizLet, rest : List[MizProofItem]) : Term = {
    var context : Context = Context()
    l.types.zipWithIndex map {p =>
      val tm = TypeTranslator.translateTyp(p._1)
      val name = TranslationController.addLocalConst(l.nr + p._2)
      context ++= VarDecl(name, Some(tm), None)
    }
    //TODO more precise encoding 
    Mizar.apply(Mizar.constant("forI"), OMBIND(LF.lambda, context, translateProofSteps(rest)))
  }
  
  //miz inference
  def translateConclusion(c : MizConclusion, rest : List[MizProofItem]) : Term = {
    Mizar.apply(Mizar.constant("by"), translateJustifiedProposition(c.jp, Nil), translateProofSteps(rest))
  }
  
  //impI
  def translateAssume(a : MizAssume, rest : List[MizProofItem]) : Term = {
    val decls = a.props map {p => 
      val t = PropositionTranslator.translateProposition(p)
      val name : LocalName = TranslationController.addLocalProp(p.nr)
      VarDecl(name, Some(Mizar.proof(t)), None)
    }
    
    Mizar.apply(Mizar.constant("impI"), OMBIND(LF.lambda, Context(decls :_*) , translateProofSteps(rest)))
  }
  
  //existsE
  def translateGiven(g : MizGiven, rest : List[MizProofItem]) : Term = {
    val exProp = PropositionTranslator.translateProposition(g.exSt)
    val tpDecls = g.types.zipWithIndex map {p =>
      val (tp, i) = p
      val tm = TypeTranslator.translateTyp(tp)
      val name = TranslationController.addLocalConst(g.nr + i)
      VarDecl(name, Some(tm), None)
    }
    
    val propDecls = g.props map {p => 
      val tm = PropositionTranslator.translateProposition(p)
      val name = TranslationController.addLocalProp(p.nr)
      VarDecl(name, Some(Mizar.proof(tm)), None)
    }
    
    val varName = LocalName("g0")
    val con = Context(VarDecl(varName, Some(Mizar.proof(exProp)), None))
    val exE = Mizar.apply(Mizar.constant("exE"),
        OMV(varName), 
        OMBIND(LF.lambda, Context(tpDecls ++ propDecls : _*), translateProofSteps(rest)))
    
    Mizar.apply(Mizar.constant("impI"), Lambda(con, exE))
  }
  
  
  def translateTake(t : MizTake, rest : List[MizProofItem]) : Term = {
    val tm = TypeTranslator.translateTerm(t.term)
    Mizar.apply(Mizar.constant("ExI"), tm, translateProofSteps(rest))
  }
  
  def translateTakeAsVar(t : MizTakeAsVar, rest : List[MizProofItem]) : Term = {
    val tm = TypeTranslator.translateTerm(t.term)
    val tp = TypeTranslator.translateTyp(t.typ)
    
    val name = TranslationController.addLocalConst(t.nr)
    val con = Context(VarDecl(name, Some(tp), None))
    val exI = Mizar.apply(Mizar.constant("ExI"), OMV(name), translateProofSteps(rest))
    Mizar.apply(Mizar.constant("decl"), tm, Lambda(con, exI))
  }
  
  def translateAuxiliaryItem(a : MizAuxiliaryItem, rest : List[MizProofItem]) : Term = a match {
    case m : MizJustifiedProposition => translateJustifiedProposition(m, rest)
    case m : MizConsider => translateConsider(m, rest)
    case m : MizSet => translateSet(m, rest)
    case m : MizReconsider => translateReconsider(m, rest)
    case m : MizDefPred => translateDefPred(m, rest)
    case m : MizDefFunc => translateDefFunc(m, rest)
  }
  
  // Auxiliary Items
  def translateJustifiedProposition(p : MizJustifiedProposition, rest : List[MizProofItem]) : Term = p match {
    case n : MizNow => translateNow(n, rest) 
    case i : MizIterEquality => translateIterEquality(i, rest)
    case pj : MizPropWithJust => translatePropWithJust(pj, rest)
  }
  
  def translateNow(n : MizNow, rest : List[MizProofItem]) : Term = {
    val name = TranslationController.addLocalProp(n.nr)
    val pf = translateReasoning(n.reasoning)
    Pi(Context(VarDecl(name, None, None)), Mizar.apply(Mizar.constant("inf"), pf, translateProofSteps(rest)))
  }
  
  def translateIterEquality(i : MizIterEquality, rest : List[MizProofItem]) : Term = {
    val tm = TypeTranslator.translateTerm(i.term) //TODO
    val eqs = i.iterSteps map { s => 
      val t = TypeTranslator.translateTerm(s.term)
      val inf = JustificationTranslator.translateInference(s.inf)
      Mizar.apply(Mizar.constant("by"),
          Mizar.apply(Mizar.constant("eq"), tm, t),
          inf
      )
    }
    val name = TranslationController.addLocalProp(i.nr)
    val con = Context(VarDecl(name, None, None))
    val by = Mizar.apply(Mizar.constant("by"), (eqs :+ translateProofSteps(rest)) :_*)
    Mizar.apply(Mizar.constant("inf"), Pi(con, by))
  }
  
  def translatePropWithJust(p : MizPropWithJust, rest : List[MizProofItem]) : Term = {
    val prop = PropositionTranslator.translateProposition(p.prop)
    TranslationController.addLocalProp(p.prop.nr)
    val just = JustificationTranslator.translateJustification(p.just)
    
    Mizar.apply(Mizar.constant("inf"), just, translateProofSteps(rest))
  }
  
  def translateConsider(c : MizConsider, rest : List[MizProofItem]) : Term = {
    val startnr = c.constnr
	val ex_prop = PropositionTranslator.translateProposition(c.prop)
	val ex_just = JustificationTranslator.translateJustification(c.just)
	
	val decls = c.typs.zipWithIndex map { p =>
      val tp = TypeTranslator.translateTyp(p._1)
      val name = TranslationController.addLocalConst(c.nr + p._2)
      VarDecl(name, Some(tp), None)
    }
    
    val props = c.props map { p => 
      val prop = PropositionTranslator.translateProposition(p)
      val name = TranslationController.addLocalProp(p.nr)
      VarDecl(name, Some(Mizar.proof(prop)), None)
    }
    
    val cont = Lambda(Context(decls : _*), Lambda(Context(props :_ *), translateProofSteps(rest)))
    Mizar.apply(Mizar.constant("ExE"), ex_just, Mizar.apply(Mizar.constant("inf"), cont))
  }
  
  def translateSet(s : MizSet, rest : List[MizProofItem]) : Term = {
    val tm = TypeTranslator.translateTerm(s.term)
    val tp = TypeTranslator.translateTyp(s.typ)
    val name = TranslationController.addLocalConst(s.nr)
    Mizar.apply(Mizar.constant("decl"), tm, Lambda(name, tp, translateProofSteps(rest)))
  }
  
  
  def translateReconsider(r : MizReconsider, rest : List[MizProofItem]) : Term = {
    val decls = r.terms.zipWithIndex map {p =>
      val ((mtp, mtm), i) = p
      val tm = TypeTranslator.translateTerm(mtm)
      val tp = TypeTranslator.translateTyp(mtp)
      val name = TranslationController.addLocalConst(r.nr + i)
      VarDecl(name, Some(tp), Some(tm))
    }
    
    val prop = PropositionTranslator.translateProposition(r.prop)
    //should use when foundation uses curry typing to justify new type
    val just = JustificationTranslator.translateJustification(r.just)
    Lambda(Context(decls : _*), translateProofSteps(rest))
  }
  
  
  def translateDefPred(r : MizDefPred, rest : List[MizProofItem]) : Term = {
     //we can ignore def preds because they are local and expanded in-place where they occur
    translateProofSteps(rest)
  }
  
    def translateDefFunc(r : MizDefFunc, rest : List[MizProofItem]) : Term = {
     //we can ignore def funcs because they are local and expanded in-place where they occur
    translateProofSteps(rest)
  }
}
