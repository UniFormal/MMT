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
import info.kwarc.mmt.lf._
import info.kwarc.mmt.lfs._


object Mizar {
	val mmlBase = URI("http", "oaff.mathweb.org") / "MML" 

	private val mizarBase =  DPath(URI("http", "latin.omdoc.org") / "foundations"/ "mizar")
	val MizarTh = mizarBase ? "mizar-curry"
	val MizarPatternsTh = mizarBase ? "mizar-patterns"
	val HiddenTh = mizarBase ? "HIDDEN"
    val MizarInformal = mizarBase ? "mizar-informal"
	
    def by : Term = OMID(MizarInformal ? "by")
    def from : Term = OMID(MizarInformal ? "from")
    
    
	def constant(name : String) : Term = {
		name match {
			case "M1" => OMID(HiddenTh ? name)
			case "R1" => OMID(HiddenTh ? name)
			case "R2" => OMID(HiddenTh ? name)
			case _ => OMID(MizarTh ? name)
		}
		
	}
	
	def compact(t : Term) : Term = {
		t
	}
	
	def apply(f : Term, args : Term*) = args.length match {
	  case 0 => f
	  case _ => ApplySpine(f, args : _*)
	}
	
	def prop : Term = constant("prop")
	def any : Term = constant("any")
	def tp : Term = constant("tp")
	def set = constant("M1")
	
	def is(t1 : Term, t2 : Term) = apply(constant("is"), t1, t2)
	def be(t1 : Term, t2 : Term) = apply(constant("be"), t1, t2)
	
	def and(tms : Term*) : Term = apply(constant("and"), tms : _*)

	def or(tms : Term*) : Term = apply(constant("or"), tms : _*)
	
	def implies(tm1 : Term, tm2 : Term) : Term = apply(constant("implies"), tm1, tm2)
	
	def not(tm : Term) : Term = apply(constant("not"), tm)
	
	def proof(t : Term) = apply(constant("proof"), t)
	
	def eq(t1 : Term, t2 : Term) = apply(constant("R1"), t1, t2)
	
	def exists(v : String, tp : Term, prop : Term) = 
	  not(OMBIND(apply(Mizar.constant("for"), tp),Context(VarDecl(LocalName(v), Some(Mizar.any), None)), not(prop)))
	
	def forall(v : String, tp : Term, prop : Term) = 
	  OMBIND(apply(Mizar.constant("for"), tp),Context(VarDecl(LocalName(v), Some(Mizar.any), None)), prop)
	
	def attr(t : Term) = apply(Mizar.constant("attr"), t)
	  
	def adjective(cluster : Term, typ : Term) = apply(Mizar.constant("adjective"), typ, cluster)
	def cluster(a1 : Term, a2 : Term) = apply(Mizar.constant("cluster"), a1, a2) 
	def choice(tp : Term) = apply(Mizar.constant("choice"), tp)
	def fraenkel(v : String, t : Term, p : Term, f : Term) = 
	  apply(Mizar.constant("fraenkel"), t, Lambda(LocalName(v), Mizar.any, p), Lambda(LocalName(v), Mizar.any, f))
}



object MMTUtils {
  
	def getTheoryPath(aid : String) : MPath = {
	  if (aid == TranslationController.currentAid)
	    TranslationController.currentTheory
	  else aid match {
	  	case "HIDDEN" => Mizar.HiddenTh
	  	case _ =>  DPath(Mizar.mmlBase / TranslationController.currentVersion.toString) ? aid
	  }
	}
	
	def getPath(aid : String, kind : String, absnr : Int) : GlobalName = {
		getTheoryPath(aid) ? (aid + "_" + kind+ "_" + absnr.toString)
		
	}
	
	def getPath(aid : String, name : String) : GlobalName = {
		getTheoryPath(aid) ? name
	}
	
	def getPath(aid : String, name : LocalName) : GlobalName = {
		getTheoryPath(aid) ? name
	}
	
	def getPath(aid : String, name : List[String]) : GlobalName = {
	  getTheoryPath(aid) ? LocalName(name.map(SimpleStep)) 
	}
	
	def args(argNr : String, body : Term) : Term = {
	  Arrow(SeqMap(Mizar.any, LocalName("i") ,OMV(argNr)), body)
	}
	
	def args(name : String, argNr : String, body : Term) : Term = {
	  Pi(LocalName("x"), SeqMap(Mizar.any, LocalName("i"), OMV(argNr)), body)
	}
	
	def args(name : String, argNr : Int, body : Term) : Term = {
			Pi(LocalName("x"), SeqMap(Mizar.any, LocalName("i"), OMI(argNr)), body)
	}
	
	def tmparr(t1 : Term, t2 : Term) : Term = Apply(Mizar.constant("TeST"),Apply(t1, t2))
	
	def argTypes(args : String, types : String, argNr : String, body : Term) : Term = {
	  	     Arrow( 
	  	         SeqMap(Mizar.be(Index(OMV(args), OMV("i")),Index(OMV(types),OMV("i"))), LocalName("i"), OMV(argNr)),
	         	 body)
	}
	
	def argTypes(args : String, types : List[Term], argNr : Int, body : Term) : Term = {
	  	     tmparr( 
	  	         SeqMap(Mizar.be(Index(OMV(args), OMV("i")),Index(Sequence(types : _*),OMV("i"))), LocalName("i"), OMI(argNr)),
	         	 body)
	}
	
	
	def argTypes(name : String, args : String, types : String, argNr : String, body : Term) : Term = {
	  	     Pi(LocalName(name), 
	  	        SeqMap(Mizar.be(Index(OMV(args), OMV("i")),Index(OMV(types),OMV("i"))), LocalName("i"), OMV(argNr)),
	         	 body)
	}
	
	
	def argTypes(name : String, args : String, types : List[Term], argNr : Int, body : Term) : Term = types.length match {
	  case 0 => body
	  case _ => Pi(LocalName(name), 
	  	        SeqMap(Mizar.be(Index(OMV("x"), OMV("i")),Index(Sequence(types : _*),OMV("i"))), LocalName("i"), OMI(argNr)),
	         	 body)
	}
	
	def nullAttribute = Lambda(LocalName("z"), Mizar.any, Mizar.constant("true"))
}
