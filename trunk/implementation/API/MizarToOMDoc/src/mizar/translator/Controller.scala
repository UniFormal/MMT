package mizar.translator

import mizar.objects._
import mmt.objects._
import jomdoc._
import jomdoc.documents._
import jomdoc.utils._
import jomdoc.frontend._
import jomdoc.symbols._
import jomdoc.libraries._
import jomdoc.modules._
import jomdoc.objects._


object TranslationController {
	val controller  =  new frontend.Controller(NullChecker, new FileReport(new java.io.File("mizar.log")))
	var currentDocument : DPath = null
	var currentTheory : MPath = null
	
	var varContext : List[Term] = Nil
	var constContext : List[Term] = Nil
	
	def add(e : StructuralElement) {
		controller.add(e)
	}
	
	def resolveVar(nr : Int) : Term = {
			varContext(nr - 1) 
	}
	
	def resolveConst(nr : Int) : Term = {
		constContext(nr - 1)
	}
	
	def addVarBinder() : String = {
		val name = "x" + varContext.length
		varContext = OMV(name) :: varContext
		name
	}
	
	def clearVarBinder() = {
		varContext = varContext.tail
	}
	
	def addConstBinder() : String = {
		val name = "c" + constContext.length
		varContext = OMV(name) :: constContext
		name
	}
	
	def addConstants(terms : List[Term]) : Unit = {
		constContext = terms ::: varContext
	}
	
	def removeConstants(n : Int) : Unit = {
		constContext = constContext.slice(n,constContext.length)
	}
}