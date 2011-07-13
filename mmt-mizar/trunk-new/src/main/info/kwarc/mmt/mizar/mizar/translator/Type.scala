package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mizar.reader._
import info.kwarc.mmt.mizar.mmt.objects._
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
			case t : MizFunc => OMA(OMID(MMTUtils.getPath(t.aid, t.name)), t.args.map(translateTerm).toList)
			case t : MizLocusVar => TranslationController.resolveLocusVar(t.nr)
			case t : MizChoice => OMA(Mizar.constant("set"), translateTyp(t.typ) :: Nil) //TODO replace set with choice
			case t : MizFraenkel => OMA(Mizar.constant("set"), Nil) //TODO 
			case t : MizNum => OMI(t.nr)
			case _ => OMV("Error in TypeTranslator -> translateTerm -> case _" + term.toString)
		}
	}
	
	def translateAdjective(adj : MizAdjective) : Term = {
		MMTAttribute(adj.name, adj.value) 
	}
	
	def translateCluster(c : MizCluster) : Term = {
		MMTCluster(c.adjs.map(x => MMTAttribute(x.name, x.value)))
	}
}