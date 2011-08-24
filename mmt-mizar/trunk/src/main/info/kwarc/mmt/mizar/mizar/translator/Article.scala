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



object ArticleTranslator {
  
	
	def translateArticle(art : MizArticle) {		
		var el : List[MizAny] = art.elems.reverse
		el.map(transArtElems)
	}
	
	private def transArtElems(e : MizAny) : Unit = {
		e match {	
			case j : MizJustifiedTheorem => translateJustifiedTheorem(j)
			//case d : MizDefinition => translateDefinition(d)
			case m : MizModeMeansDef =>DefinitionTranslator.translateModeMeansDef(m)
				TranslationController.mm += 1
			case m : MizModeIsDef =>DefinitionTranslator.translateModeIsDef(m)	
				TranslationController.mi += 1
			case p : MizPredMeansDef =>DefinitionTranslator.translatePredMeansDef(p)
				TranslationController.pm += 1
			case p : MizPredIsDef =>DefinitionTranslator.translatePredIsDef(p)
				TranslationController.pi += 1
			case f : MizFuncMeansDef => DefinitionTranslator.translateFuncMeansDef(f)
				TranslationController.fm += 1
			case f : MizFuncIsDef => DefinitionTranslator.translateFuncIsDef(f)
				TranslationController.fi += 1
			case a : MizAttrMeansDef => DefinitionTranslator.translateAttrMeansDef(a)
				TranslationController.am += 1
			case a : MizAttrIsDef => DefinitionTranslator.translateAttrIsDef(a)
				TranslationController.ai += 1
			case s : MizStructDef => DefinitionTranslator.translateStructDef(s)
			case n : MizNotation => translateNotation(n)
			case r : MizRegistration => SchemeRegTranslator.translateRegistration(r)
			case s : MizSchemeDef => SchemeRegTranslator.translateScheme(s)
			case l : MizLemma => translateLemma(l)
			case s : MizSet => translateSet(s)
			case c : MizConsider => translateConsider(c)
			case r : MizReconsider => translateReconsider(r)
			case _ => println(e)
		}
	}
	
	def translateSet(s : MizSet) = {
	  val tm = TypeTranslator.translateTerm(s.term)
	  val tp = TypeTranslator.translateTyp(s.typ)
	  val name = "const_" + s.constnr
	  val c = new Constant(OMMOD(TranslationController.currentTheory), LocalName(name), Some(tp), Some(tm), Individual(None))
	  TranslationController.add(c)
	}
	
	def translateConsider(c : MizConsider) = {
	  val startnr = c.constnr
	  val ex_prop = PropositionTranslator.translateProposition(c.prop)
	  val ex_c = new Constant(OMMOD(TranslationController.currentTheory), LocalName("ex_prop" + startnr), Some(ex_prop), None, Individual(None))
	  TranslationController.add(ex_c)
	  val typs = c.typs.map(TypeTranslator.translateTyp)
	  val props = c.props.map(PropositionTranslator.translateProposition)

	  var i : Int = 0
	  while (i < typs.length) {
	    val name = "const_" + (startnr + i).toString
	    val const = new Constant(OMMOD(TranslationController.currentTheory), LocalName(name), Some(typs(i)), None, Individual(None))
	    TranslationController.add(const)
	    i += 1
	  }
	  
	  
	  i = 0
	  while (i < props.length) {
	    val name = "prop_" + startnr + "_" + i
	    val const = new Constant(OMMOD(TranslationController.currentTheory), LocalName(name), Some(props(i)), None, Individual(None))
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
	    val name = "const_" + (startnr + i).toString
	    val c = new Constant(OMMOD(TranslationController.currentTheory), LocalName(name), Some(tms(i)._1), Some(tms(i)._2), Individual(None))
	    TranslationController.add(c)
	    
	    val propname = name + "_prop"
	    val c_prop = new Constant(OMMOD(TranslationController.currentTheory), LocalName(propname), Some(prop), None, Individual(None))
	    TranslationController.add(c_prop)
	    i += 1
	  }
	}
	
	def translateNotation(n : MizNotation) = {
		val tp = n.antonymic match  {
			case true => OMA(Mizar.constant("not"), MMTResolve(n.constrAid, n.kind, n.constrAbsnr) :: Nil )
			case false => MMTResolve(n.constrAid, n.kind, n.constrAbsnr)
		}
		val name = n.aid + "_" + n.kind + "_notation_" + n.relnr 
		val nt = new Constant(OMMOD(TranslationController.currentTheory), LocalName(name), Some(tp), None,Individual(None) )
		TranslationController.add(nt)
	}
	
	def translateJustifiedTheorem(j : MizJustifiedTheorem) {
		val tp = Mizar.compact(OMA(Mizar.constant("proof"), PropositionTranslator.translateProposition(j.prop) :: Nil))
		
		val jt = new Constant(OMMOD(TranslationController.currentTheory), LocalName(j.name), Some(tp), None, Individual(None))
		
		TranslationController.add(jt)
	}
	
	def translateLemma(l : MizLemma) {
	  val name = "lemma_" + l.prop.nr
	  val matches = ("prop" / PropositionTranslator.translateProposition(l.prop))
	  val pattern = artPatterns.Lemma
	  val i = new Instance(OMMOD(TranslationController.currentTheory), LocalName(name), pattern.home.toMPath ? pattern.name, matches)
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