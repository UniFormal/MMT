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
import info.kwarc.mmt.api.objects._


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
			case m : MizModeIsDef =>DefinitionTranslator.translateModeIsDef(m)			
			case p : MizPredMeansDef =>DefinitionTranslator.translatePredMeansDef(p)
			case p : MizPredIsDef =>DefinitionTranslator.translatePredIsDef(p)
			case f : MizFuncMeansDef => DefinitionTranslator.translateFuncMeansDef(f)
			case f : MizFuncIsDef => DefinitionTranslator.translateFuncIsDef(f)
			case a : MizAttrMeansDef => DefinitionTranslator.translateAttrMeansDef(a)
			case a : MizAttrIsDef => DefinitionTranslator.translateAttrIsDef(a)
			case s : MizStructDef => DefinitionTranslator.translateStructDef(s)

			case n : MizNotation => 
			println(n)
			translateNotation(n)
			case _ => println(e)
		}
	}
	
	def translateNotation(n : MizNotation) = {
		val tp = n.antonymic match  {
			case true => OMA(Mizar.constant("not"), OMID(MMTUtils.getPath(n.constrAid, n.constrName)) :: Nil )
			case false => OMID(MMTUtils.getPath(n.constrAid, n.constrName))
		}
		val nt = new Constant(OMMOD(TranslationController.currentTheory), LocalName(n.name), Some(tp), None, null)
		//println(nt)
		TranslationController.add(nt)
	}
	
	def translateJustifiedTheorem(j : MizJustifiedTheorem) {
		val tp = Mizar.compact(OMA(Mizar.constant("proof"), PropositionTranslator.translateProposition(j.prop) :: Nil))
		
		val jt = new Constant(OMMOD(TranslationController.currentTheory), LocalName(j.name), Some(tp), None, null)
		
		//println(jt.toString)
		TranslationController.add(jt)
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