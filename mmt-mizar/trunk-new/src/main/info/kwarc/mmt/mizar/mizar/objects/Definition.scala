package info.kwarc.mmt.mizar.mizar.objects

/**
 * 	objects.Definition
 * 	Contains classes for handling Mizar Definitions	
 * 	@author Mihnea Iancu
 *  @version 10/01/2011
 */


//utils

class XMLDefinition(val aid : String, val kind : String, val nr : Int, val argTypes: List[MizTyp], val retType : Option[MizTyp]) extends MizAny {
	var prop : MizProposition = null
	var premises : List[MizLet] = Nil
	
	def setProp(p : MizProposition) {
		prop = p
	}
	
	def setPremises(p : List[MizLet]) = {
		premises = p
	}
	
	def resolveArgs() : Option[List[(Option[String],MizTyp)]] = {
		val namedVars = premises.flatMap(x => x.types)
		if (namedVars.length == argTypes.length) {
			val names = namedVars.map(l => ParsingController.resolveVar(l.vid))
			Some(names.zip(argTypes))			
		} else {
			None
		}
	}
}


class XMLRedefinition( aid : String,  kind : String, nr : Int, argTypes : List[MizTyp], retType : Option[MizTyp], val constraid : String, val constrabsnr : Int) 
	extends XMLDefinition(aid, kind, nr, argTypes, retType)

class XMLConstructor(val aid : String, val kind : String, val nr : Int, val argTypes: List[MizTyp], val retType: Option[MizTyp]) extends MizAny {
	
}

class XMLDefTheorem(val kind : String, val nr : Int, val constrkind : String, val constrnr : Int, val prop : MizProposition)

class XMLDefiniens(val aid : String, val nr : Int, val constraid : String, val constrkind : String, val absconstrnr : Int)

class XMLIsDefiniens(aid : String, nr : Int, constraid : String, constrkind : String, absconstrnr : Int, val cases : List[(MizTerm, MizFormula)], val term : Option[MizTerm]) 
	extends XMLDefiniens(aid,nr, constraid, constrkind, absconstrnr)

class XMLMeansDefiniens( aid : String,  nr : Int,  constraid : String,  constrkind : String,  absconstrnr : Int, val cases : List[(MizFormula, MizFormula)], val form : Option[MizFormula]) 
	extends XMLDefiniens(aid,nr, constraid, constrkind, absconstrnr)

trait XMLDefMeaning

class XMLIsDefMeaning(val term: MizTerm) extends XMLDefMeaning
class XMLMeansDefMeaning(val form : MizFormula) extends XMLDefMeaning

