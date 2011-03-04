package mizar.translator

import mizar.objects._
import mizar.reader._
import mmt.objects._
import jomdoc.objects._

object PropositionTranslator {
	def translateFormula(form : MizFormula) : Term = {
		form match {
			case f : MizNot => MMTNot(translateFormula(f.formula)) 
			case f : MizAnd => MMTAnd(f.formulas.map(translateFormula))
			case f : MizFor => {
				val vName = TranslationController.addVarBinder()
				val tm = MMTFor(vName, TypeTranslator.translateTyp(f.typ), translateFormula(f.formula))
				TranslationController.clearVarBinder()
				tm
			}
			case f : MizPred => MMTPred(f.aid, f.absnr, f.kind, f.terms.map(TypeTranslator.translateTerm))
			case f : MizPrivPred =>  {
				TranslationController.addConstants(f.terms.map(TypeTranslator.translateTerm))
				val tm = translateFormula(form)
				TranslationController.removeConstants(f.terms.length)
				tm
			}
			case f : MizIs => MMTIs(TypeTranslator.translateTerm(f.term), TypeTranslator.translateTyp(f.typ))
			case _ => {
				OMV("error in PropositionTranslator -> TranslateFormula -> case _")
			}
		}
	}
	
	def translateProposition(prop : MizProposition) : Term = {
		translateFormula(prop.form)
	}
}