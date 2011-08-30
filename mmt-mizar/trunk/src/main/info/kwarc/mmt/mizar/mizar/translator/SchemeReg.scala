package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.lf._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.objects.Conversions._


object SchemeRegTranslator {
	
	def  translateRegistration(reg : MizRegistration) = {
	  reg.cluster match {
			case rc : MizRCluster => translateRCluster(rc)
			case fc : MizFCluster => translateFCluster(fc)
			case cc : MizCCluster => translateCCluster(cc)
	  }
	}
	
	def translateRCluster(rc : MizRCluster) = {
		val name = "RC" + rc.nr  
	  
		val argTypes = rc.args.map(TypeTranslator.translateTyp)
		val argNr = argTypes.length
		val typ = TypeTranslator.translateTyp(rc.typ)
		val cluster = TypeTranslator.translateCluster(rc.cluster)
	
		val matches = (OMV("n") / OMI(argNr)) ++ (SeqVar("argTypes") / SeqItemList(argTypes)) ++ 
					(OMV("typ") / typ) ++ (OMV("cluster") / cluster)
	
		val pattern = RegPatterns.MizExistentialReg
		val i = new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		TranslationController.add(i)
	}
	
	def translateFCluster(fc : MizFCluster) = {
		val name = "FC" + fc.nr  
	  
		val argTypes = fc.args.map(TypeTranslator.translateTyp)
		val argNr = argTypes.length
		val functor = TypeTranslator.translateTerm(fc.functor)
		val cluster = TypeTranslator.translateCluster(fc.cluster)
	
		val matches = (OMV("n") / OMI(argNr)) ++ (SeqVar("argTypes") / SeqItemList(argTypes)) ++ 
					(OMV("functor") / functor) ++ (OMV("cluster") / cluster)
	
		val pattern = RegPatterns.MizFunctionalReg
		val i = new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		
		TranslationController.add(i)

	}
	
	def translateCCluster(cc : MizCCluster) = {
		val name = "CC" + cc.nr  
	  
		val argTypes = cc.args.map(TypeTranslator.translateTyp)
		val argNr = argTypes.length
		val typ = TypeTranslator.translateTyp(cc.typ)
		val first = TypeTranslator.translateCluster(cc.first)
		val second = TypeTranslator.translateCluster(cc.second)

		val matches = (OMV("n") / OMI(argNr)) ++ (SeqVar("argTypes") / SeqItemList(argTypes)) ++ 
					(OMV("typ") / typ) ++ (OMV("first") / first) ++ (OMV("second") / second)
	
		val pattern = RegPatterns.MizExistentialReg
		val i = new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		TranslationController.add(i)

	}
	
	
	//Scheme
	
	def translateSchemeArg(a : MizSchemeArg) : Term = {
	  a match {
	    case f : MizSchemeFuncDecl =>
	      val args = f.argTypes.map(TypeTranslator.translateTyp(_))
	      val retType = TypeTranslator.translateTyp(f.retType)
	      OMA(LF.arrow, args ::: retType :: Nil)
	    case p : MizSchemePredDecl => 
	      val args = p.argTypes.map(TypeTranslator.translateTyp(_))
	      val retType = Mizar.constant("prop")
	      OMA(LF.arrow, args ::: retType :: Nil)

	  }
	}
	
	def translateScheme(s : MizSchemeDef) = {
	  val name = "S" + s.schemeNr
	  val premises = s.premises.map(x => PropositionTranslator.translateProposition(x))
	  val args = s.args.map(x => translateSchemeArg(x))
	  val prop = PropositionTranslator.translateProposition(s.prop)
	  //pattern
	  val inst = {
	    val pattern = SchemePatterns.MizSchemeDef
	    val matches = ("n" / OMI(args.length)) ++ (SeqVar("args") / SeqItemList(args)) ++
	    			  ("m" / OMI(premises.length)) ++ (SeqVar("premises") / SeqItemList(premises)) ++
	    			  ("prop" / prop)
	    val i = new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
	    TranslationController.add(i)

	  }
	}
	
}