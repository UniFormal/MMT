package mizar.translator

import mizar.objects._
import mizar.reader._
import mmt.objects._
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
		
		TranslationController.currentAid = art.title
		
		val path = new DPath(new xml.URI(art.title))
		val d = new Document(path)
		TranslationController.add(d)
		TranslationController.currentDocument = d.path
		
		val th = new DeclaredTheory(d.path, LocalPath(art.title :: Nil), Some(Mizar.MizarTh)) //None		
		
		TranslationController.add(th)
		//TranslationController.add(PlainImport(Mizar.MizarTh, th.path))
		TranslationController.add(PlainInclude(Mizar.HiddenTh, th.path))
		TranslationController.currentTheory  = th.path
		
		var el : List[MizAny] = art.elems.reverse
		el.map(transArtElems)
	}
	
	private def transArtElems(e : MizAny) : Unit = {
		e match {	
			case j : MizJustifiedTheorem => translateJustifiedTheorem(j)
			//case d : MizDefinition => translateDefinition(d)
			case m : MizModeMeansDef => translateModeMeansDef(m)
			case p : MizPredMeansDef => translatePredMeansDef(p)
			case f : MizFuncMeansDef => translateFuncMeansDef(f)
			case _ => println(e)
		}
	}
	
	def translateJustifiedTheorem(j : MizJustifiedTheorem) {
		val tp = Mizar.compact(OMA(Mizar.constant("proof"), PropositionTranslator.translateProposition(j.prop) :: Nil))
		
		val jt = new Constant(OMMOD(TranslationController.currentTheory), LocalName(j.name), Some(tp), None, null)
		
		TranslationController.add(jt)
	}
	
	
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
		
		//meaning
		val propName = "means_" + defName
		val args = m.args.map(x => (x._1,TypeTranslator.translateTyp(x._2)))
		val prop = Mizar.compact(MMTDefMeans(args, PropositionTranslator.translateFormula(m.form)))
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
		TranslationController.add(argCons)
		
		//meaning
		val propName = "means_" + defName
		val args = p.args.map(x => (x._1,TypeTranslator.translateTyp(x._2)))
		val prop = Mizar.compact(MMTDefMeans(args, PropositionTranslator.translateFormula(p.form)))
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
		//println(argCons)
		val path = TranslationController.currentTheory ? defName
		TranslationController.add(argCons)
		
		TranslationController.addRetTerm(path)
		
		//retType
		val typesName = "be_" + defName
		val args = f.args.map(x => (x._1,TypeTranslator.translateTyp(x._2)))
		val argTypes = MMTDefTypes(args, TypeTranslator.translateTyp(f.retType), path)
		val typeCons = new Constant(OMMOD(TranslationController.currentTheory), LocalName(typesName), Some(argTypes), None, null)
		//println(typeCons)
		TranslationController.add(typeCons)
		//meaning
		val propName = "means_" + defName
		val prop = Mizar.compact(MMTDefMeans(args, PropositionTranslator.translateFormula(f.form)))
		val funcProp = new Constant(OMMOD(TranslationController.currentTheory), LocalName(propName), Some(prop), None, null)
		//println(funcProp.toNode)
		//println("-------")
		//println(TranslationController.controller.get(TranslationController.currentTheory))
		//println("-------")
		TranslationController.add(funcProp)
		
		TranslationController.clearLocusVarContext()
		
	}

	/*
	def translateDefinition(d : MizDefinition) {
		val argsDef = MMTDefArgs(d.argTypes.length, d.kind)
		val defName = d.aid + "_" + d.kind + "_" + d.relnr
		
		//args
		val argCons = new Constant(TranslationController.currentTheory, LocalPath(defName :: Nil), Some(argsDef), None, null, None)
		val path = TranslationController.currentTheory ? defName		
		TranslationController.add(argCons)
		//types (for functions)
		d.retType match {
			case Some(rt) => {
				val typesName = "be_" + defName
				val dt : List[MizTyp] = d.argTypes
				val argTypes = MMTDefTypes(dt.map(TypeTranslator.translateTyp), TypeTranslator.translateTyp(rt), path)
				val typeCons = new Constant(TranslationController.currentTheory, LocalPath(typesName :: Nil), Some(argTypes), None, null, None)
				TranslationController.add(typeCons)
			}
			case None => None
		}
		
		//prop
		val propName = "means_" + defName
		val prop = Mizar.compact(MMTDefMeaning(d.argTypes.map(TypeTranslator.translateTyp), PropositionTranslator.translateProposition(d.prop)))
		val funcProp = new Constant(TranslationController.currentTheory, LocalPath(propName :: Nil), Some(prop), None, null, None)
		TranslationController.add(funcProp)
	}
	*/
}