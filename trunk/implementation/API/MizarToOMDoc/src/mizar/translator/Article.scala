package mizar.translator

import mizar.objects._
import mizar.reader._
import mmt.objects._
import jomdoc._
import jomdoc.documents._
import jomdoc.utils._
import jomdoc.frontend._
import jomdoc.symbols._
import jomdoc.libraries._
import jomdoc.modules._
import jomdoc.objects._


object ArticleTranslator {
	
	def translateArticle(art : MizArticle) {
		val path = new DPath(new xml.URI(art.title))
		val d = new Document(path)
		TranslationController.add(d)
		TranslationController.currentDocument = d.path
		
		val th = new Theory(d.path, LocalPath(art.title :: Nil), Some(Mizar.MizarTh)) //None
		TranslationController.add(th)
		TranslationController.currentTheory  = th.path
		
		var el : List[MizAny] = art.elems
		el.map(transArtElems)
	}
	
	private def transArtElems(e : MizAny) : Unit = {
		e match {	
			case j : MizJustifiedTheorem => translateJustifiedTheorem(j)
			case d : MizDefinition => translateDefinition(d)
			case _ => println(e)
		}
	}
	
	def translateJustifiedTheorem(j : MizJustifiedTheorem) {
		val tp = Mizar.compact(OMA(Mizar.constant("proof"), PropositionTranslator.translateProposition(j.prop) :: Nil))
		
		val jt = new Constant(TranslationController.currentTheory, LocalPath(j.name :: Nil), Some(tp), None, null, None)
		//println(jt)
		TranslationController.add(jt)
	}
	
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
}