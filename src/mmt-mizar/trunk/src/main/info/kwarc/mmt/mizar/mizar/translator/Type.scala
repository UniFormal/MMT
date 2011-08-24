package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mizar.reader._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._

object TypeTranslator {
	def translateTyp(t : MizTyp) : Term = {
		MMTResolve(t.aid, t.kind, t.absnr)
	}
	
	def translateTerm(term : MizTerm) : Term = {
		term match {
			case t : MizVar => TranslationController.resolveVar(t.nr) 
			case t : MizConst =>  OMID(MMTUtils.getPath(TranslationController.currentAid, "const_" + t.nr))
			case t : MizFunc => MMTFunc(MMTResolve(t.aid, t.kind, t.absnr), t.args.map(translateTerm).toList) 
			case t : MizLocusVar => TranslationController.resolveLocusVar(t.nr)
			case t : MizChoice => OMA(Mizar.constant("set"), translateTyp(t.typ) :: Nil) //TODO replace set with choice
			case t : MizFraenkel => OMA(Mizar.constant("set"), t.types.map(translateTyp)) //TODO 
			case t : MizNum => OMI(t.nr)
			case t : MizPrivFunc => translateTerm(t.term)
			case _ => OMV("Error in TypeTranslator -> translateTerm -> case _" + term.toString)
		}
	}
	
	def translateAdjective(adj : MizAdjective) : Term = {
		MMTAttribute(adj.aid, adj.kind, adj.absnr, adj.value) 
	}
	
	def translateCluster(c : MizCluster) : Option[Term] = {
		MMTCluster(c.adjs.map(translateAdjective(_)))
	}
	
}