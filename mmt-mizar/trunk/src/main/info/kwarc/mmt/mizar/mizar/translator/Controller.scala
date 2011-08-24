package info.kwarc.mmt.mizar.mizar.translator

import info.kwarc.mmt.mizar.mizar.objects._
import info.kwarc.mmt.mizar.mmt.objects._
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._


object TranslationController {
	val controller  =  new frontend.Controller( new libraries.FoundChecker(libraries.DefaultFoundation), new FileReport(new java.io.File("mizar.log")))
	var currentAid : String = null
	var currentDocument : DPath = null
	var currentTheory : MPath = null
	
	var pm = 0
	var pi = 0
	var fm = 0
	var fi = 0
	var mm = 0
	var mi = 0
	var am = 0
	var ai = 0
	
	var varContext : List[Term] = Nil
	var locusVarContext : List[Term] = Nil
	
	def add(e : StructuralElement) {
		controller.add(e)
	}
	
	def resolveVar(nr : Int) : Term = {
		varContext(nr - 1) 
	}
		
	def resolveLocusVar(nr : Int) : Term = {
		locusVarContext(nr - 1)
	}
	
	
	def addVarBinder(n : Option[String]) : String = n match {
		case Some(x) => 
			varContext = OMV(x) :: varContext
			x			
		case None =>
			val name = "x" + varContext.length
			varContext = OMV(name) :: varContext
			name
	}
	
	def clearVarBinder() = {
		varContext = varContext.tail
	}
	
	def addLocusVarBinder(n : Option[String]) : String = n match {
		case Some(x) =>
			locusVarContext =  locusVarContext :+ OMV(x)
			x
		case None =>
			val name = "x" + locusVarContext.length
			locusVarContext = locusVarContext :+ OMV(name)
			name
	}
	
	def addRetTerm(path: GlobalName) = {
		locusVarContext = locusVarContext :+ OMA(OMID(path), locusVarContext)
	}
	
	def clearLocusVarBinder() {
		locusVarContext = locusVarContext.tail
	}

	def clearLocusVarContext() {
		locusVarContext = Nil
	}
		
	def getFreeVar() : String = {
		var i : Int = 0
		val totalContext = varContext ::: locusVarContext
		while (totalContext.contains(OMV("x" + i))) {
			i = i + 1
		}
		"x" + i
	}
}