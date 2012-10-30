package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.lfs._
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
		rc.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))

		val argTypes = rc.args.map(TypeTranslator.translateTyp)
		
		val argNr = argTypes.length
		
		val typ = MMTUtils.args("x", argTypes.length, MMTUtils.argTypes("x", argTypes, argTypes.length, TypeTranslator.translateTyp(rc.typ)))
		val cluster = MMTUtils.args("x", argTypes.length, MMTUtils.argTypes("x", argTypes, argTypes.length, TypeTranslator.translateCluster(rc.cluster)))
	
		val matches = (OMV("n") / OMI(argNr)) ++ (OMV("argTypes") / Sequence(argTypes :_*)) ++ 
					(OMV("typ") / typ) ++ (OMV("cluster") / cluster)
	
		val pattern = RegPatterns.MizExistentialReg
		val i = new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		TranslationController.clearLocusVarContext()
		TranslationController.addSourceRef(i, rc)
		TranslationController.add(i)
	}
	
	def translateFCluster(fc : MizFCluster) = {
		val name = "FC" + fc.nr  
		fc.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))

		val argTypes = fc.args.map(TypeTranslator.translateTyp)
		val argNr = argTypes.length
		val functor = MMTUtils.args("x", argTypes.length, MMTUtils.argTypes("x", argTypes, argTypes.length, TypeTranslator.translateTerm(fc.functor)))
		val cluster = MMTUtils.args("x", argTypes.length, MMTUtils.argTypes("x", argTypes, argTypes.length, TypeTranslator.translateCluster(fc.cluster)))
	
		val matches = (OMV("n") / OMI(argNr)) ++ (OMV("argTypes") / Sequence(argTypes : _*)) ++ 
					(OMV("functor") / functor) ++ (OMV("cluster") / cluster)
	
		val pattern = RegPatterns.MizFunctionalReg
		val i = new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		
		TranslationController.clearLocusVarContext()

		TranslationController.addSourceRef(i, fc)
		TranslationController.add(i)

	}
	
	def translateCCluster(cc : MizCCluster) = {
		val name = "CC" + cc.nr  
	  
		cc.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))

		val argTypes = cc.args.map(TypeTranslator.translateTyp)
		val argNr = argTypes.length
		val typ = MMTUtils.args("x", argTypes.length, MMTUtils.argTypes("x", argTypes, argTypes.length, TypeTranslator.translateTyp(cc.typ)))
		val first = MMTUtils.args("x", argTypes.length, MMTUtils.argTypes("x", argTypes, argTypes.length, TypeTranslator.translateCluster(cc.first)))
		val second = MMTUtils.args("x", argTypes.length, MMTUtils.argTypes("x", argTypes, argTypes.length, TypeTranslator.translateCluster(cc.second)))

		val matches = (OMV("n") / OMI(argNr)) ++ (OMV("argTypes") / Sequence(argTypes : _*)) ++ 
					(OMV("typ") / typ) ++ (OMV("first") / first) ++ (OMV("second") / second)
	
		val pattern = RegPatterns.MizConditionalReg
		val i = new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
		TranslationController.clearLocusVarContext()
		TranslationController.addSourceRef(i, cc)
		TranslationController.add(i)
	}
	
	
	//Scheme
	
	def translateSchemeArg(a : MizSchemeArg) : Term = {
	  a match {
	    case f : MizSchemeFuncDecl =>
	      val args = f.argTypes.map(TypeTranslator.translateTyp(_))
	      val retType = TypeTranslator.translateTyp(f.retType)
	      Arrow(args, retType)
	    case p : MizSchemePredDecl => 
	      val args = p.argTypes.map(TypeTranslator.translateTyp(_))
	      val retType = Mizar.constant("prop")
	      Arrow(args, retType)

	  }
	}
	
	def translateScheme(s : MizSchemeDef) = {
	  val name = "S" + s.schemeNr
	  
	  s.args.zipWithIndex.map(p => TranslationController.addLocusVarBinder(Index(OMV("x"), OMI(p._2 + 1))))

	  
	  val args = s.args.map(x => translateSchemeArg(x))
	  
	  val premises = s.premises.map(x => PropositionTranslator.translateProposition(x)).map(x => MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, x)))
	  
	  val prop = MMTUtils.args("x", args.length, MMTUtils.argTypes("x", args, args.length, PropositionTranslator.translateProposition(s.prop)))
	  //pattern
	  val inst = {
	    val pattern = SchemePatterns.MizSchemeDef
	    val matches = ("n" / OMI(args.length)) ++ (OMV("args") / Sequence(args : _*)) ++
	    			  ("m" / OMI(premises.length)) ++ (OMV("premises") / Sequence(premises :_ *)) ++
	    			  ("prop" / prop)
	    val i = new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
	    TranslationController.clearLocusVarContext()
	    TranslationController.addSourceRef(i, s)
	    TranslationController.add(i)
	  }
	}
	
}