package mizar.objects
/**
 * 	objects.Definition
 * 	Contains classes for handling Mizar Definitions	
 * 	@author Mihnea Iancu
 *  @version 10/01/2011
 */


class MizDefinition(val aid : String, val kind : String, val relnr : Int, val argTypes: List[MizTyp], val retType : Option[MizTyp], val premises : List[MizLet]) extends MizAny {
	var prop : MizProposition = null
	def setProp(p : MizProposition) {
		prop = p
	}
}


class MizConstructor(val aid : String, val kind : String, val relnr : Int, val argTypes: List[MizTyp], val retType: Option[MizTyp]) extends MizAny

