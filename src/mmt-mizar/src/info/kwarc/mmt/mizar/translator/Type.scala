package info.kwarc.mmt.mizar.translator

import info.kwarc.mmt.mizar.objects._
import info.kwarc.mmt.mizar.reader._
import info.kwarc.mmt.mizar.mmtwrappers._
import MizSeq._

import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.lf._

object TypeTranslator {
  def translateTyp(t : MizTyp) : Term = {
    val tpfunc = MMTResolve(t.aid, t.kind, t.absnr)
    val tms = t.terms.map(translateTerm)
    val tp = tms.length match {
      case 0 => tpfunc
      case _ => Mizar.apply(tpfunc, tms : _*)
    }
    val mizCluster = t.clusters(0)
    mizCluster.adjs match {
      case Nil => tp
      case _ => 
        val cluster = translateCluster(mizCluster)
        Mizar.adjective(cluster,tp)
    }
  }

  
	def translateTerm(term : MizTerm) : Term = {
		term match {
			case t : MizVar => TranslationController.resolveVar(t.nr) 
			case t : MizConst =>  TranslationController.resolveConst(t.nr)
			case t : MizConstFunc => t.args.length match {
			  case 0 => Mizar.apply(OMID(MMTUtils.getPath("qvar","constFunc")), OMV("f" + t.nr.toString))
			  case _ => Mizar.apply(Mizar.apply(OMID(MMTUtils.getPath("qvar","constFunc")), OMV("f" + t.nr.toString)), t.args.map(translateTerm) : _*)
			}
			case t : MizFunc => MMTFunc(MMTResolve(t.aid, t.kind, t.absnr), t.args.map(translateTerm).toList) 
			case t : MizSchemeFunc => t.args.length match {
			  case 0 => Index(OMV("x"), OMI(t.nr - 1))
			  case _ => Mizar.apply(Index(OMV("x"), OMI(t.nr - 1)), t.args.map(TypeTranslator.translateTerm) : _*)
			}

			case t : MizLocusVar => TranslationController.resolveLocusVar(t.nr)
			case t : MizChoice => Mizar.choice(translateTyp(t.typ))
			case t : MizFraenkel => 
			  val argTypes = t.types.map(translateTyp)
			  val args = argTypes.map(x => (x,TranslationController.addVarBinder(None)))
			  val term = translateTerm(t.term)
			  val form = PropositionTranslator.translateFormula(t.form)
			  val v = TranslationController.getFreeVar()
			  args.map(x => TranslationController.clearVarBinder())
			  args.foldRight[(Term,Term)](form -> Mizar.fraenkel(v,Mizar.set,Mizar.eq(OMV(v),term),term))((p,r) => (Lambda(LocalName(p._2), p._1,r._1) -> Mizar.fraenkel(p._2,p._1,r._1,r._2)))._2		  
			case t : MizNum => Mizar.num(t.nr)
			case t : MizPrivFunc => translateTerm(t.term)
			case _ => throw ImplementationError("Error in TypeTranslator -> translateTerm -> case _" + term.toString)
		}
	}
	
	def translateAdjective(adj : MizAdjective) : Term = {
		MMTAttribute(adj.aid, adj.kind, adj.absnr, adj.value) 
	}
	
	def translateCluster(c : MizCluster) :Term = {
		MMTCluster(c.adjs.map(translateAdjective(_)))
	}
	
}