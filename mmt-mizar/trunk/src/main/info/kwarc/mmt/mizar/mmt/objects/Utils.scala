package info.kwarc.mmt.mizar.mmt.objects

import info.kwarc.mmt.mizar.mizar.translator.TranslationController
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.lf._


object Mizar {
	val baseURI = URI("http", "oaff.mathweb.org") / "mml" 

	val MizarTh = DPath(URI("http", "latin.omdoc.org") / "foundations"/ "mizar") ? "mizar-curry"
	val MizarPatternsTh = DPath(URI("http", "latin.omdoc.org") / "foundations"/ "mizar") ? "mizar-patterns"
	val HiddenTh = DPath(URI("http", "latin.omdoc.org") / "foundations" / "mizar") ? "HIDDEN"
	//val TarskiTh = DPath(utils.mmt.baseURI.resolve("set_theories/mizar/mizar_tarski.omdoc")) ? "Tarski"
	
	def constant(name : String) : Term = {
		name match {
			case "set" => OMID(HiddenTh ? name)
			case "=" => OMID(HiddenTh ? "==") //due to twelf constant naming limitations
			case "<>" => OMID(HiddenTh ? "<>")
			case _ => OMID(MizarTh ? name)
		}
		
	}
	
	def compact(t : Term) : Term = {
		t
	}
	
	def prop : Term = constant("prop")
	def any : Term = constant("any")
	def tp : Term = constant("tp")
	def set = constant("set")
	
	def is(t1 : Term, t2 : Term) = OMA(constant("is"), List(t1,t2))
	def be(t1 : Term, t2 : Term) = OMA(constant("be"), List(t1,t2))
	
	def and(tms : SeqItem*) : Term = OMA(constant("and"), tms.toList)

	def or(tms : SeqItem*) : Term = OMA(constant("or"), tms.toList)
	
	def implies(tm1 : SeqItem, tm2 : SeqItem) : Term = OMA(constant("implies"), List(tm1, tm2))
	
	def not(tm : SeqItem) : Term = OMA(constant("not"), List(tm))
	
	def proof(t : Term) = OMA(constant("proof"), List(t))
	
	def eq(t1 : Term, t2 : Term) = OMA(constant("eq"), List(t1,t2))
	
	def exists(v : String, tp : Term, prop : Term) = 
	  OMBIND(OMA(Mizar.constant("ex"), List(tp)),Context(TermVarDecl(v, Some(Mizar.any), None)),prop)
	
	def forall(v : String, tp : Term, prop : Term) = 
	  OMBIND(OMA(Mizar.constant("for"), List(tp)),Context(TermVarDecl(v, Some(Mizar.any), None)), prop)
	
	def attr(t : Term) = OMA(Mizar.constant("attr"), List(t))
	  
	def adjective(cluster : Term, typ : Term) = OMA(Mizar.constant("adjective"), List(typ,cluster))
	def cluster(a1 : Term, a2 : Term) = OMA(Mizar.constant("cluster"), List(a1, a2)) 
	def choice(tp : Term) = OMA(Mizar.constant("choice"), List(tp))
	def fraenkel(v : String, t : Term, p : Term, f : Term) = 
	  OMA(Mizar.constant("fraenkel"), List(t,Lambda(v,Mizar.any, p),Lambda(v,Mizar.any, f)))
	
}



object MMTUtils {
	
	def getTheoryPath(aid : String) : MPath = {
	  if (aid == TranslationController.currentAid)
	    TranslationController.currentTheory
	  else aid match {
	  	case "HIDDEN" => Mizar.HiddenTh
	  	case _ =>  DPath(Mizar.baseURI) ? aid
	  }
	}
	
	def getPath(aid : String, kind : String, absnr : Int) : GlobalName = {
		getTheoryPath(aid) ? (aid + "_" + kind+ "_" + absnr.toString)
		
	}
	def getPath(aid : String, name : String) : GlobalName = {
		getTheoryPath(aid) ? name
	}
	
	def getPath(aid : String, name : List[String]) : GlobalName = {
	  getTheoryPath(aid) ? LocalName(name.map(NamedStep)) 
	}
	
	def args(argNr : String, body : Term) : Term = {
	  OMA(LF.arrow,
	      List(SeqSubst(Mizar.any,"i",SeqUpTo(OMV(argNr))),
	      body ))
	}
	
	def args(name : String, argNr : String, body : Term) : Term = {
	  Pi("x", SeqSubst(Mizar.any,"i",SeqUpTo(OMV(argNr))), body)
	}
	
	def argTypes(args : String, types : String, argNr : String, body : Term) : Term = {
	  	     OMA(LF.arrow, 
	  	         List(SeqSubst(Mizar.be(Index(SeqVar(args), OMV("i")),Index(SeqVar(types),OMV("i"))),"i", SeqUpTo(OMV(argNr))),
	         	 body))
	}
	
	def argTypes(name : String, args : String, types : String, argNr : String, body : Term) : Term = {
	  	     Pi(name, 
	  	        SeqSubst(Mizar.be(Index(SeqVar(args), OMV("i")),Index(SeqVar(types),OMV("i"))),"i", SeqUpTo(OMV(argNr))),
	         	 body)
	}
	
	def nullAttribute = Lambda("z",Mizar.any, Mizar.constant("true"))
	
}
