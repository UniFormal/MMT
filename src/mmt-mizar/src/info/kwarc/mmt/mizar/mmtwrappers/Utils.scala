package info.kwarc.mmt.mizar.mmtwrappers

import info.kwarc.mmt.mizar.translator.TranslationController
import MizSeq._

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import notations._
import info.kwarc.mmt.lf._


object Mizar {
	val mmlBase = utils.URI("http", "oaff.mathweb.org") / "MML"
  val mathHubBase = "http://gl.mathhub.info/Mizar/MML/blob/master"
	private val mizarBase =  DPath(utils.URI("http", "latin.omdoc.org") / "foundations"/ "mizar")
	val MizarTh = mizarBase ? "Mizar"
	val MizarPatternsTh = mizarBase ? "MizarPatterns"
	
	val HiddenTh = mizarBase ? "HIDDEN"
  //TODO
	val MizarInformal = mizarBase ? "mizar-informal"
	
  def by : Term = OMID(MizarInformal ? "by")
  def from : Term = OMID(MizarInformal ? "from")
    
	def constantName(name : String) : GlobalName = {
		name match {
  		case "set" => HiddenTh ? name
	  	case "eq" => MizarTh ? name //officially in Hidden but in our case its in the Mizar base theory
			case "in" => HiddenTh ? name
			case _ => MizarTh ? name
		}
	}
	def constant(name : String) : Term = OMID(constantName(name))
	
	def compact(t : Term) : Term = {
		t
	}
	
	def apply(f : Term, args : Term*) = ApplyGeneral(f, args.toList)
	
	def prop : Term = constant("prop")
	def any : Term = constant("any")
	def tp : Term = constant("tp")
	def set = constant("set")
	
	def is(t1 : Term, t2 : Term) = apply(constant("is"), t1, t2)
	def be(t1 : Term, t2 : Term) = apply(constant("be"), t1, t2)
	
	def trueCon = constant("true")

	def falseCon = constant("false")
	
	def and(tms : List[Term]) : Term = apply(constant("and"), OMI(tms.length) :: tms :_*)
	  //apply(constant("and"), OMI(tms.length), apply(Sequence, tms :_*))
	def or(tms : List[Term]) : Term = apply(constant("or"), OMI(tms.length) :: tms :_*)
	
	// Special function for 'and' and 'or' applied to an sequence (e.g. Ellipsis or sequence variable)
	def seqConn(connective : String, length : Term, seq : Term) : Term = 
	  apply(constant(connective), length, seq)

	
	def implies(tm1 : Term, tm2 : Term) : Term = apply(constant("implies"), tm1, tm2)
	
	def not(tm : Term) : Term = apply(constant("not"), tm)
	
	def proof(t : Term) = apply(constant("proof"), t)
	
	def eq(t1 : Term, t2 : Term) = apply(constant("eq"), t1, t2)
	
	def exists(v : String, tp : Term, prop : Term) = 
	  ApplySpine(Mizar.constant("ex"), tp, Lambda(LocalName(v), Mizar.any, prop))	
	def forall(v : String, tp : Term, prop : Term) = 
	  ApplySpine(Mizar.constant("for"), tp, Lambda(LocalName(v), Mizar.any, prop))
	  
//	  OMBIND(apply(Mizar.constant("for"), tp),Context(VarDecl(LocalName(v), Some(Mizar.any), None, None)), prop)
	
	def attr(t : Term) = apply(Mizar.constant("attr"), t)
	def adjective(cluster : Term, typ : Term) = apply(Mizar.constant("adjective"), typ, cluster)
	def cluster(a1 : Term, a2 : Term) = apply(Mizar.constant("cluster"), a1, a2) 
	def choice(tp : Term) = apply(Mizar.constant("choice"), tp)
	def fraenkel(v : String, t : Term, p : Term, f : Term) = 
	  apply(Mizar.constant("fraenkel"), t, Lambda(LocalName(v), Mizar.any, p), Lambda(LocalName(v), Mizar.any, f))
	  
  
	val numRT = new uom.RepresentedRealizedType(any, uom.StandardInt)
	def num(i: Int) = numRT(i)
}

object MMTUtils {
    val mainPatternName = OMV.anonymous 
  
	def getTheoryPath(aid : String) : MPath = {
	  if (aid == TranslationController.currentAid)
	    TranslationController.currentTheory
	  else aid match {
	  	case "HIDDEN" => Mizar.HiddenTh
	  	case _ =>  DPath(Mizar.mmlBase) ? aid
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
	
	//Lambda args
	
	def LamArgs(name: String, argNr : Int, body : Term) : Term = {
	  Lambda(LocalName("x"), Ellipsis(OMI(argNr), LocalName("i"), Mizar.any), body)
	}
	
	
	def Lam(name : String, tp : Term, body : Term) : Term = {
	  Lambda(LocalName(name), tp, body)
	}
	
	// Pi args
	def PiArgs(argNr : String, body : Term) : Term = {
	  Arrow(Ellipsis(OMV(argNr), LocalName("i"), Mizar.any), body)
	}
	
	
	def PiArgs(name : String, argNr : String, body : Term) : Term = {
	  Pi(LocalName("x"), Ellipsis(OMV(argNr), LocalName("i"), Mizar.any), body)
	}

	
	def PiArgs(name : String, argNr : Int, body : Term) : Term = {
			Pi(LocalName("x"), Ellipsis(OMI(argNr), LocalName("i"), Mizar.any), body)
	}
	
	def PiArgTypes(args : String, types : String, argNr : String, body : Term) : Term = {
	  	     Arrow( 
	  	         Ellipsis(OMV(argNr), LocalName("i"), Mizar.be(Index(OMV(args), OMV("i")),Index(OMV(types),OMV("i")))),
	         	 body)
	}
	
	def PiArgTypes(args : String, types : List[Term], argNr : Int, body : Term) : Term = {
	  	     Arrow( 
	  	         Ellipsis(OMI(argNr), LocalName("i"), Mizar.be(Index(OMV(args), OMV("i")), Index(Sequence(types : _*), OMV("i")))),
	         	 body)
	}
	
	
	def PiArgTypes(name : String, args : String, types : String, argNr : String, body : Term) : Term = {
	  	     Pi(LocalName(name), 
	  	        Ellipsis(OMV(argNr), LocalName("i"), Mizar.be(Index(OMV(args), OMV("i")),Index(OMV(types),OMV("i")))),
	         	 body)
	}
	
	def PiArgTypes(name : String, args : String, types : List[Term], argNr : Int, body : Term) : Term = types.length match {
	  case 0 => body
	  case _ => Pi(LocalName(name), 
	  	        Ellipsis(OMI(argNr), LocalName("i"), Mizar.be(Index(OMV("x"), OMV("i")), Index(Sequence(types : _*), OMV("i")))),
	         	 body)
	}
		
	def nullAttribute = Lambda(LocalName("z"), Mizar.any, Mizar.constant("true"))
}
