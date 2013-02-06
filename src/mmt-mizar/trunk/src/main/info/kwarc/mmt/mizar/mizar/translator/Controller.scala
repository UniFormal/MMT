package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._
import scala.collection.mutable.ArrayStack
import info.kwarc.mmt.lf._
import scala.collection._


object TranslationController {
	
    var controller = {
       val c = new frontend.Controller
//       c.setFileReport(File("mizar.log"))
//       c.setCheckNone //c.setFoundChecker(new libraries.DefaultFoundation(controller.report))
       c
    }
    
    var query : Boolean = false
	  
	//new frontend.Controller(libraries.NullChecker, new FileReport(new java.io.File("mizar.log")))
	
    //set during translation
    var currentBase : String = null
	var currentVersion : Int = -1
    var currentAid : String = null
    
    def localPath = LocalPath(currentAid :: Nil)
	def currentDocument : DPath = DPath(Mizar.mmlBase / currentVersion.toString)
	def currentTheory : MPath = currentDocument ? localPath
	def currentSource = currentBase + "/source/" + currentVersion.toInt + "/" + currentAid + "/miz"
	
	var anonConstNr = 0
	var defs = 0
	var theorems = 0
	var notations = 0
	var regs = 0
	var schemes = 0
	
	var varContext : ArrayStack[Term] = new ArrayStack
	var locusVarContext : ArrayStack[Term] = new ArrayStack
	// consider/set/reconsider everywhere and let in proofs
	var constContext : mutable.HashMap[Int,Term] = mutable.HashMap()
	//deftheorems, lemmas, assume and others 
	var propContext : mutable.HashMap[Int,Term] = mutable.HashMap()
	
	def clear() = {
     constContext = mutable.HashMap()
     propContext = mutable.HashMap()
     controller.clear
     anonConstNr = 0
    }
    
    def getLmName(nrO : Option[Int]) : String = nrO match {
      case Some(nr) => 
        "Lm" + nr
      case None => 
        anonConstNr += 1
        "AnonLm" + anonConstNr
    }
	
	
	def addSourceRef(mmtEl : metadata.HasMetaData, mizEl : MizAny) = mizEl.sreg match {
      case None => None
      case Some(sregion) => 
        val start = parser.SourcePosition(-1, sregion.start.line, sregion.start.col)
        val end = parser.SourcePosition(-1, sregion.end.line, sregion.end.col)
        val ref = parser.SourceRef(URI(currentSource), parser.SourceRegion(start, end))
        mmtEl.metadata.add(metadata.Link(parser.SourceRef.metaDataKey, ref.toURI))
    }
	
	def add(e : StructuralElement) {
		controller.add(e)
	}
	
	def resolveVar(nr : Int) : Term = {
	    varContext(varContext.length - nr) 
	}
		
	def resolveLocusVar(nr : Int) : Term = {
	  assert (locusVarContext.length >= nr,"TranslationController.resolveLocusVar " +
	  		"for number " + nr +" which is over the size of context " + locusVarContext)
	  locusVarContext(locusVarContext.length - nr)
	}
	
	def addLocalProp(nrO : Option[Int]) : LocalName = nrO match {
	  case Some(nr) =>
	    val name = LocalName("p" + nr)
	    propContext(nr) = OMV(name)
	    name
      case _ => LocalName.Anon
	}
	
	def addGlobalProp(nrO : Option[Int], sName : String) = nrO match {
	  case Some(nr) =>
	    val name = LocalName(sName)
	    propContext(nr) = OMID(MMTUtils.getPath(TranslationController.currentAid, name))
	  case _ => None
	}
	
	def resolveProp(nr : Int) : Term = try {
	  propContext(nr)
	} catch {
	  case _ : Throwable => 
	    println(propContext)
	    throw new java.lang.Error("propContext lookup failed for " + nr)
	}
	
	def addGlobalConst(nr : Int, kind : String) : LocalName = {
	  val name = LocalName(kind + nr)
	  constContext(nr) = OMID(MMTUtils.getPath(TranslationController.currentAid, name))
	  name
	}
	
	def addLocalConst(nr : Int) : LocalName = {
	  val name = LocalName("c" + nr)
	  constContext(nr) = OMV(name)
	  name
	}
	
	def resolveConst(nr : Int) : Term = {
	  if (query) {
	    Mizar.apply(OMID(MMTUtils.getPath("qvar","const")), OMV("c" + nr.toString))	    
	  } else {
		constContext(nr)
	  }
	}
	
	def addQVarBinder() = {
	  val name = "x" + varContext.length
	  varContext.push(Mizar.apply(OMID(DPath(Mizar.mmlBase) ? "qvar" ? "qvar"), OMV(name)))
	}
	
	def addVarBinder(n : Option[String]) : String = n match {
		case Some(x) => 
			varContext.push(OMV(x))
			x			
		case None =>
			val name = "x" + varContext.length
			varContext.push(OMV(name))
			name
	}
	
	def clearConstContext() = {
	  constContext = mutable.HashMap()
	}
	
	def clearVarBinder() = {
		varContext.pop()
	}
	
	def clearVarContext() = {
		varContext = new ArrayStack()
	}
	
	def addLocusVarBinder(tm : Term) : Unit = {
		locusVarContext.push(tm)
	}
	
	def addRetTerm(path: GlobalName) = {
	    locusVarContext.length match {
	      case 0 => locusVarContext.push(OMID(path))
	      case _ => locusVarContext.push(Mizar.apply(OMID(path), locusVarContext : _*))
	    }
	}
	
	def clearLocusVarBinder() {
		locusVarContext.pop()
	}

	def clearLocusVarContext() {
		locusVarContext = new ArrayStack()
	}
		
	def getFreeVar() : String = {
		var i : Int = 0
		val totalContext = varContext.toList ::: locusVarContext.toList
		while (totalContext.contains(OMV("x" + i))) {
			i = i + 1
		}
		"x" + i
	}
	
	def makeConstant(n: LocalName, t: Term) =
      new Constant(OMMOD(currentTheory), n, None, Some(t), None, None, None)
   def makeConstant(n: LocalName, t: Term, d: Term) =
      new Constant(OMMOD(currentTheory), n, None, Some(t), Some(d), None, None)
}