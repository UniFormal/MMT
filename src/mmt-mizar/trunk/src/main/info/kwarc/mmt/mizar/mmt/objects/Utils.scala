package mmt.objects

import mizar.translator.TranslationController
import info.kwarc.mmt.api._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.utils._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.symbols._
import info.kwarc.mmt.api.libraries._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.objects._


object Mizar {
	val MizarTh = DPath(utils.mmt.baseURI.resolve("set_theories/mizar/mizar-curry.omdoc")) ? "Mizar-Curry"
	val HiddenTh = DPath(utils.mmt.baseURI.resolve("set_theories/mizar/HIDDEN.omdoc")) ? "HIDDEN"
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
}

object MMTUtils {
	
	
	def getFilePath(aid : String) : DPath =  {
		if (aid == TranslationController.currentAid) 
			TranslationController.currentDocument
		else 
			DPath(utils.mmt.baseURI.resolve("set_theories/mizar/" + aid + ".omdoc"))
	}
	def getTheoryPath(aid : String) : MPath = getFilePath(aid : String) ? aid
	def getPath(aid : String, kind : String, absnr : Int) : GlobalName = {
		getTheoryPath(aid) ? (aid + "_" + kind+ "_" + absnr.toString)
		
	}
	def getPath(aid : String, name : String) : GlobalName = {
		getTheoryPath(aid) ? name
	}
}
