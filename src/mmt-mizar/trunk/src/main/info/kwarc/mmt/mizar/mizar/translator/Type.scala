package mizar.translator

import mizar.objects._
import mizar.reader._
import mmt.objects._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._

object TypeTranslator {
	def translateTyp(t : MizTyp) : Term = {
		MMTType(t.name, t.aid, t.kind, t.absnr)
	}
	
	def translateTerm(term : MizTerm) : Term = {
		term match {
			case t : MizVar => TranslationController.resolveVar(t.nr) 
			case t : MizConst =>  TranslationController.resolveConst(t.nr)
			case t : MizFunc => OMA(OMID(MMTUtils.getPath(t.aid, t.kind, t.absnr)), t.args.map(translateTerm).toList)
			case t : MizLocusVar => TranslationController.resolveLocusVar(t.nr)
			case _ => OMV("Error in TypeTranslator -> translateTerm -> case _" + term.toString)
		}
	}
}