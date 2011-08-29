package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mizar.reader._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.lf._

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
			case t : MizChoice => Mizar.choice(translateTyp(t.typ))
			case t : MizFraenkel => 
			  val argTypes = t.types.map(translateTyp)
			  val args = argTypes.map(x => (x,TranslationController.addVarBinder(None)))
			  val term = translateTerm(t.term)
			  val form = PropositionTranslator.translateFormula(t.form)
			  val v = TranslationController.getFreeVar()
			  args.map(x => TranslationController.clearVarBinder())
			  args.foldRight[(Term,Term)](form -> Mizar.fraenkel(v,Mizar.set,Mizar.eq(OMV(v),term),term))((p,r) => (Lambda(p._2,p._1,r._1) -> Mizar.fraenkel(p._2,p._1,r._1,r._2)))._2
			  
			case t : MizNum => OMI(t.nr)
			case t : MizPrivFunc => translateTerm(t.term)
			case _ => OMV("Error in TypeTranslator -> translateTerm -> case _" + term.toString)
		}
	}
	
	def translateAdjective(adj : MizAdjective) : Term = {
		MMTAttribute(adj.aid, adj.kind, adj.absnr, adj.value) 
	}
	
	def translateCluster(c : MizCluster) : Term = {
		MMTCluster(c.adjs.map(translateAdjective(_)))
	}
	
}