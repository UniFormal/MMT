package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mizar.reader._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.lf._
import info.kwarc.mmt.lfs._

object PropositionTranslator {
	def translateFormula(form : MizFormula) : Term = {
		form match {
			case f : MizNot => MMTNot(translateFormula(f.formula)) 
			case f : MizAnd => MMTAnd(f.formulas.map(translateFormula))
			case f : MizFor => {
				val vName = TranslationController.addVarBinder(f.varName)
				val tm = MMTFor(vName, TypeTranslator.translateTyp(f.typ), translateFormula(f.formula))
				TranslationController.clearVarBinder()
				tm
			}
			case f : MizExists => {
				val vName = TranslationController.addVarBinder(f.varName)
				val tm = MMTExists(vName, TypeTranslator.translateTyp(f.typ), translateFormula(f.formula))
				TranslationController.clearVarBinder()
				tm
			}
			case f : MizPred => MMTPred(f.aid, f.absnr, f.kind, f.terms.map(TypeTranslator.translateTerm))
			case f : MizSchemePred => f.terms.length match {
			  case 0 => Index(OMV("x"), OMI(f.nr))
			  case _ => Mizar.apply(Index(OMV("x"), OMI(f.nr)), f.terms.map(TypeTranslator.translateTerm) : _*)
			}
			case f : MizPrivPred =>  {
				val form = translateFormula(f.formula)
				form
			}
			case f : MizIs => MMTIs(TypeTranslator.translateTerm(f.term), TypeTranslator.translateTyp(f.typ))
			case f : MizVerum => Mizar.constant("true")
			case f : MizErrorFrm => Mizar.constant("false") //TODO check
			case _ => {
				throw ImplementationError("error in PropositionTranslator -> TranslateFormula -> case _" + form.toString)
			}
		}
	}
	
	def translateProposition(prop : MizProposition) : Term = {
	  translateFormula(prop.form)	
	}
}