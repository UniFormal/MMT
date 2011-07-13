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
	
	var varContext : List[Term] = Nil
	var constContext : List[Term] = Nil
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
	
	def resolveConst(nr : Int) : Term = {
		constContext(nr - 1)
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
	
	def addConstBinder() : String = {
		val name = "c" + constContext.length
		constContext = OMV(name) :: constContext
		name
	}
	
	def addConstants(terms : List[Term]) : Unit = {
		constContext = terms ::: constContext
	}
	
	def removeConstants(n : Int) : Unit = {
		constContext = constContext.slice(n,constContext.length)
	}
	
	def getFreeVar() : String = {
		var i : Int = 0
		val totalContext = varContext ::: locusVarContext ::: constContext
		while (totalContext.contains(OMV("x" + i))) {
			i = i + 1
		}
		"x" + i
	}
}