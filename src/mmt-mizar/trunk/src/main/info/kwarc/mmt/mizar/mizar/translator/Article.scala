package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mizar.reader._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.patterns._
import info.kwarc.mmt.api.objects._
import objects.Conversions._
import info.kwarc.mmt.lf.Arrow
import info.kwarc.mmt.lf.Lambda



object ArticleTranslator {
	def translateArticle(art : MizArticle) {		
		var el : List[MizAny] = art.elems.reverse
		el.map(transArtElems)
	}
	
	import TranslationController.makeConstant
	
	private def transArtElems(e : MizAny) : Unit = {
		e match {	
			case j : MizJustifiedTheorem => 
			  translateJustifiedTheorem(j)
			  TranslationController.theorems += 1
			case m : MizModeMeansDef => 
			  DefinitionTranslator.translateModeMeansDef(m)
			  TranslationController.defs += 1
			case m : MizModeIsDef => 
			  DefinitionTranslator.translateModeIsDef(m)	
			  TranslationController.defs += 1
			case p : MizPredMeansDef =>
			  DefinitionTranslator.translatePredMeansDef(p)
			  TranslationController.defs += 1
			case p : MizPredIsDef => 
			  DefinitionTranslator.translatePredIsDef(p)
			  TranslationController.defs += 1
			case f : MizFuncMeansDef =>
			  DefinitionTranslator.translateFuncMeansDef(f)
			  TranslationController.defs += 1
			case f : MizFuncIsDef => 
			  DefinitionTranslator.translateFuncIsDef(f)
			  TranslationController.defs += 1
			case a : MizAttrMeansDef => 
			  DefinitionTranslator.translateAttrMeansDef(a)
			  TranslationController.defs += 1
			case a : MizAttrIsDef => 
			  DefinitionTranslator.translateAttrIsDef(a)
			  TranslationController.defs += 1
			case m : MizExpMode => 
			  DefinitionTranslator.translateExpMode(m)
			  TranslationController.defs += 1
			case s : MizStructDef => 
			  DefinitionTranslator.translateStructDef(s)
			  TranslationController.defs += 1
			case n : MizNotation => 
			  translateNotation(n)
			  TranslationController.notations += 1
			case r : MizRegistration => 
			  SchemeRegTranslator.translateRegistration(r)
			  TranslationController.regs += 1
			case s : MizSchemeDef => 
			  SchemeRegTranslator.translateScheme(s)
			  TranslationController.schemes += 1
			case l : MizLemma => translateLemma(l)
			case s : MizSet => translateSet(s)
			case c : MizConsider => translateConsider(c)
			case r : MizReconsider => translateReconsider(r)
			case n : MizNow => translateNow(n)
			case i : MizIterEquality => translateIterEquality(i)
			case f : MizDefFunc => translateDefFunc(f)
			case p : MizDefPred => translateDefPred(p)
			case _ => println(e)
		}
	}
	
	def translateDefFunc(f : MizDefFunc) = {
	  val name = TranslationController.addGlobalConst(f.nr, "DF")
	  val tp = Arrow(f.argTypes.map(x => Mizar.any), Mizar.any)
	  
	  val con = f.argTypes map {t => 
	    val name = TranslationController.getFreeVar()
	      TranslationController.addLocusVarBinder(OMV(name))
	    val tp = TypeTranslator.translateTyp(t)
	    VarDecl(name, Some(tp), None)
	  }
	  val df = Lambda(Context(con :_*), TypeTranslator.translateTerm(f.term))
	  
	  val c = makeConstant(name, tp, df)
	  TranslationController.add(c)	  	  
	}
	
	def translateDefPred(p : MizDefPred) = {
	  val name = TranslationController.addGlobalConst(p.nr, "DP")
	  val tp = Arrow(p.argTypes.map(x => Mizar.any), Mizar.any)
	  
	  val con = p.argTypes map {t => 
	    val name = TranslationController.getFreeVar()
	      TranslationController.addLocusVarBinder(OMV(name))
	    val tp = TypeTranslator.translateTyp(t)
	    VarDecl(name, Some(tp), None)
	  }
	  val df = Lambda(Context(con :_*), PropositionTranslator.translateFormula(p.form))
	  
	  val c = makeConstant(name, tp, df)
	  TranslationController.add(c)
	}
	
	def translateIterEquality(i : MizIterEquality) = {
	  val name = TranslationController.getLmName(i.nr)
	  TranslationController.addGlobalProp(i.nr, name)
	  val terms = (i.term :: i.iterSteps.map(_.term)).map(TypeTranslator.translateTerm)
	  val form = Mizar.apply(Mizar.constant("eq"), terms :_*)
	  val tp = Mizar.proof(form)
	  val c = makeConstant(name, form)
	  TranslationController.add(c)	  
	}
	
	
	def translateNow(n : MizNow) = {
	  val form = PropositionTranslator.translateFormula(n.blockThesis.form) //TODO add justification
	  val name = TranslationController.getLmName(n.nr)
	  TranslationController.addGlobalProp(n.nr, name)
	  val c = makeConstant(name, form)
	  TranslationController.add(c)
	}
	
	def translateSet(s : MizSet) = {
	  val tm = TypeTranslator.translateTerm(s.term)
	  val tp = TypeTranslator.translateTyp(s.typ)
	  val name = TranslationController.addGlobalConst(s.constnr, "C")
	  val c = makeConstant(name, tp, tm)
	  TranslationController.add(c) 
	}
	
	def translateConsider(c : MizConsider) = {
	  val startnr = c.constnr
	  val ex_prop = PropositionTranslator.translateProposition(c.prop)
	  val ex_c = makeConstant(LocalName("ex_prop_" + startnr), ex_prop)
	  TranslationController.add(ex_c)
	  val typs = c.typs.map(TypeTranslator.translateTyp)
	  
	  var i : Int = 0
	  while (i < typs.length) {
	    val name = TranslationController.addGlobalConst(startnr + i, "C")
	    println(name)
	    val const = makeConstant(name, typs(i))
	    TranslationController.add(const)
	    i += 1
	  }
	  
	  val props = c.props.map(PropositionTranslator.translateProposition)
	  i = 0
	  while (i < props.length) {
	    val name = "prop_" + startnr + "_" + i
       val const = makeConstant(name, props(i))
	    TranslationController.add(const)
	    i += 1
	  }
	   
	}
	
	def translateReconsider(r : MizReconsider) = {
	  val startnr = r.constnr
	  val tms = r.terms.map(p => (TypeTranslator.translateTyp(p._1),TypeTranslator.translateTerm(p._2)))
	  val prop = PropositionTranslator.translateProposition(r.prop)
	  
	  var i : Int = 0
	  while (i < tms.length) {
	    val name = TranslationController.addGlobalConst(startnr + i, "C")
       val const = makeConstant(name, tms(i)._1, tms(i)._2)
	    TranslationController.add(const)
	    
	    val propname = name + "_prop"
	    val c_prop = makeConstant(LocalName(propname), prop)
	    TranslationController.add(c_prop)
	    i += 1
	  }
	}
	
	def translateNotation(n : MizNotation) = {
		val tp = n.antonymic match  {
			case true => Mizar.not(MMTResolve(n.constrAid, n.kind, n.constrAbsnr))
			case false => MMTResolve(n.constrAid, n.kind, n.constrAbsnr)
		}
		val name = "N" + n.kind + n.nr 
		val nt = makeConstant(LocalName(name), tp)
		TranslationController.addSourceRef(nt, n)
		TranslationController.add(nt)
	}
	
	def translateJustifiedTheorem(j : MizJustifiedTheorem) {
	    val name = "T" + j.nr
	    TranslationController.addGlobalProp(j.prop.nr, name)
	    println(name)
		val tp = Mizar.compact(Mizar.proof(PropositionTranslator.translateProposition(j.prop)))
		val df = Mizar.compact(JustificationTranslator.translateJustification(j.just))
		
		val jt = makeConstant(LocalName(name), tp, df)
		
	    TranslationController.addSourceRef(jt, j)
		TranslationController.add(jt)
	}
	
	def translateLemma(l : MizLemma) = {
	  val name = TranslationController.getLmName(l.prop.nr)
	  TranslationController.addGlobalProp(l.prop.nr, name)
	  val matches = ("prop" / PropositionTranslator.translateProposition(l.prop))
	  val pattern = artPatterns.Lemma
	  val i = new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
	  TranslationController.addSourceRef(i, l)
	  TranslationController.add(i)
	}
	
	/*
	def translateModeMeansDef(m : MizModeMeansDef) {
		val argsDef = MMTDefArgs(m.args.length, "M")
		val defName = m.name match {
			case Some(name) => name
			case None => m.aid + "_" + "M" + "_" + m.absnr
		}
		
		m.args.map(x => TranslationController.addLocusVarBinder(x._1))
		
		//args		
		var argCons = new Constant(OMMOD(TranslationController.currentTheory), LocalName(defName), Some(argsDef), None, null)
		val path = TranslationController.currentTheory ? defName
		TranslationController.add(argCons)
		TranslationController.addRetTerm(path)

		//meaning
		val propName = "means_" + defName
		val args = m.args.map(x => (x._1,TypeTranslator.translateTyp(x._2)))
		val prop = Mizar.compact(MMTDefMeans(args, m.cases.map(x => (PropositionTranslator.translateFormula(x._1) -> PropositionTranslator.translateFormula(x._2))), m.form match {case None => None case Some(mform) => PropositionTranslator.translateFormula(mform)}))
		val funcProp = new Constant(OMMOD(TranslationController.currentTheory), LocalName(propName), Some(prop), None, null)
		TranslationController.add(funcProp)
		
		TranslationController.clearLocusVarContext()

	}
	
	def translatePredMeansDef(p : MizPredMeansDef) {
		val argsDef = MMTDefArgs(p.args.length, "R")
		val defName = p.name match {
			case Some(name) => name
			case None => p.aid + "_" + "R" + "_" + p.absnr
		}
		
		p.args.map(x => TranslationController.addLocusVarBinder(x._1))

		//args		
		var argCons = new Constant(OMMOD(TranslationController.currentTheory), LocalName(defName), Some(argsDef), None, null)
		val path = TranslationController.currentTheory ? defName
		//println(argCons.toString)
		TranslationController.add(argCons)
		
		//meaning
		val propName = "means_" + defName
		val args = p.args.map(x => (x._1,TypeTranslator.translateTyp(x._2)))
		val prop = Mizar.compact(MMTDefMeans(args, PropositionTranslator.translateFormula(p.form)))
		val funcProp = new Constant(OMMOD(TranslationController.currentTheory), LocalName(propName), Some(prop), None, null)
		//println(funcProp.toNode)
		TranslationController.add(funcProp)
		//println("---got here ---")
		TranslationController.clearLocusVarContext()

	}
	
	
	def translateFuncIsDef(f : MizFuncIsDef) {
		val argsDef = MMTDefArgs(f.args.length, "K")
		val defName = f.name match {
			case Some(name) => name
			case None => f.aid + "_" + "K" + "_" + f.absnr
		}
		
		f.args.map(x => TranslationController.addLocusVarBinder(x._1))
		//args
		var argCons = new Constant(OMMOD(TranslationController.currentTheory), LocalName(defName), Some(argsDef), None, null)
		val path = TranslationController.currentTheory ? defName
		TranslationController.add(argCons)
		
		TranslationController.addRetTerm(path)
		
		//retType
		val typesName = "be_" + defName
		val args = f.args.map(x => (x._1,TypeTranslator.translateTyp(x._2)))
		val argTypes = MMTDefTypes(args, TypeTranslator.translateTyp(f.retType), path)
		val typeCons = new Constant(OMMOD(TranslationController.currentTheory), LocalName(typesName), Some(argTypes), None, null)
		TranslationController.add(typeCons)
		//meaning
		val propName = "means_" + defName
		val prop = Mizar.compact(MMTDefMeans(args, TypeTranslator.translateTerm(f.term)))
		val funcProp = new Constant(OMMOD(TranslationController.currentTheory), LocalName(propName), Some(prop), None, null)
		TranslationController.add(funcProp)
		
		TranslationController.clearLocusVarContext()		
	}
	
	def translateFuncMeansDef(f : MizFuncMeansDef) {
		val argsDef = MMTDefArgs(f.args.length, "K")
		val defName = f.name match {
			case Some(name) => name
			case None => f.aid + "_" + "K" + "_" + f.absnr
		}
		
		f.args.map(x => TranslationController.addLocusVarBinder(x._1))
		//args
		var argCons = new Constant(OMMOD(TranslationController.currentTheory), LocalName(defName), Some(argsDef), None, null)
		val path = TranslationController.currentTheory ? defName
		TranslationController.add(argCons)
		
		TranslationController.addRetTerm(path)
		
		//retType
		val typesName = "be_" + defName
		val args = f.args.map(x => (x._1,TypeTranslator.translateTyp(x._2)))
		val argTypes = MMTDefTypes(args, TypeTranslator.translateTyp(f.retType), path)
		val typeCons = new Constant(OMMOD(TranslationController.currentTheory), LocalName(typesName), Some(argTypes), None, null)
		TranslationController.add(typeCons)
		//meaning
		val propName = "means_" + defName
		val prop = Mizar.compact(MMTDefMeans(args, PropositionTranslator.translateFormula(f.form)))
		val funcProp = new Constant(OMMOD(TranslationController.currentTheory), LocalName(propName), Some(prop), None, null)
		TranslationController.add(funcProp)
		
		TranslationController.clearLocusVarContext()		
	}

	def translateAttrMeansDef(f : MizAttrMeansDef) {
		if (f.args.length != 1) {
			println("Warning in mizar.translator.ArticleTranslator translateFuncAttrDef: wrong number of arguments for attribute definition - " + f.args.length )
		}
		val argType = TypeTranslator.translateTyp(f.args.head._2)
		val argsDef = MMTDefAttrArgs(argType)		
		val defName = f.name match {
			case Some(name) => name
			case None => f.aid + "_" + "V" + "_" + f.absnr
		}
		
		f.args.map(x => TranslationController.addLocusVarBinder(x._1))
		//args
		var argCons = new Constant(OMMOD(TranslationController.currentTheory), LocalName(defName), Some(argsDef), None, null)
		val path = TranslationController.currentTheory ? defName
		TranslationController.add(argCons)
		
		//TranslationController.addRetTerm(path)
		
		//retType
		val typesName = "be_" + defName
		val argTypes = MMTDefAttrTypes(argType, TypeTranslator.translateTyp(f.retType), path)
		val typeCons = new Constant(OMMOD(TranslationController.currentTheory), LocalName(typesName), Some(argTypes), None, null)
		TranslationController.add(typeCons)
		
		//meaning
		val propName = "means_" + defName
		val prop = Mizar.compact(MMTDefAttrMeans((f.args.head._1 -> argType), path, PropositionTranslator.translateFormula(f.form)))
		val funcProp = new Constant(OMMOD(TranslationController.currentTheory), LocalName(propName), Some(prop), None, null)
		TranslationController.add(funcProp)
		
		TranslationController.clearLocusVarContext()		
	}
	*/
}