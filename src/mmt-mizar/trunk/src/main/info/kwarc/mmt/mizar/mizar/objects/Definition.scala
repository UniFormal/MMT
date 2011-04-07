package mizar.objects

/**
 * 	objects.Definition
 * 	Contains classes for handling Mizar Definitions	
 * 	@author Mihnea Iancu
 *  @version 10/01/2011
 */




//utils

class XMLDefinition(val aid : String, val kind : String, val relnr : Int, val symbolnr : Option[Int], val argTypes: List[MizTyp], val retType : Option[MizTyp], val premises : List[MizLet]) extends MizAny {
	var prop : MizProposition = null
	def setProp(p : MizProposition) {
		prop = p
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

class XMLConstructor(val aid : String, val kind : String, val relnr : Int, val argTypes: List[MizTyp], val retType: Option[MizTyp]) extends MizAny {
	var symbolnr : Option[Int] = None
	def addSymbolNr(n : Int) = {
		symbolnr = Some(n)
	}
}

class XMLDefTheorem(val kind : String, val nr : Int, val constrkind : String, val constrnr : Int, val prop : MizProposition)

trait XMLDefiniens

class XMLIsDefiniens(val aid : String, val nr : Int, val constrkind : String, val constrnr : Int, val term : MizTerm) extends XMLDefiniens

class XMLMeansDefiniens(val aid : String, val nr : Int, val constrkind : String, val constrnr : Int, val form : MizFormula) extends XMLDefiniens

trait XMLDefMeaning

class XMLIsDefMeaning(val term: MizTerm) extends XMLDefMeaning
class XMLMeansDefMeaning(val form : MizFormula) extends XMLDefMeaning

