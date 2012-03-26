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


object TranslationController {
	
    val controller = {
       val c = new frontend.Controller
       c.setFileReport(File("mizar.log"))
       c.setCheckNone //c.setFoundChecker(new libraries.DefaultFoundation(controller.report))
       c
    }
    
    var query : Boolean = false
	  
	
	//new frontend.Controller(libraries.NullChecker, new FileReport(new java.io.File("mizar.log")))
	var currentAid : String = null
	var currentDocument : DPath = null
	var currentTheory : MPath = null
	
	var defs = 0
	var theorems = 0
	var notations = 0
	var regs = 0
	var schemes = 0
	
	var varContext : ArrayStack[Term] = new ArrayStack
	var locusVarContext : ArrayStack[Term] = new ArrayStack
	
	def add(e : StructuralElement) {
		controller.add(e)
	}
	
	def resolveVar(nr : Int) : Term = {
	    varContext(varContext.length - nr) 
	}
		
	def resolveLocusVar(nr : Int) : Term = {
		locusVarContext(locusVarContext.length - nr)
		//Index(SeqVar("y"), OMI(nr - 1))
	}
	
	def resolveConst(nr : Int) : Term = {
	  if (query) {
	    OMA(OMID(MMTUtils.getPath("qvar","const")), List(OMV("c" + nr.toString)))
	  } else {
	    OMID(MMTUtils.getPath(TranslationController.currentAid, "C" + nr))
	  }
	}
	
	def addQVarBinder() = {
	  val name = "x" + varContext.length
	  varContext.push(OMA(OMID(DPath(Mizar.baseURI) ? "qvar" ? "qvar"),List(OMV(name))))
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
	      case _ => locusVarContext.push(OMA(OMID(path), locusVarContext.toList))
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
}