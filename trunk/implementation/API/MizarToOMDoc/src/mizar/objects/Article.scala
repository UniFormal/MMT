package mizar.objects

class MizArticle(val title : String, var elems : List[MizAny]) {
	def addElem(e : MizAny) {
		elems = e :: elems
	}
	
	def setElems(e : List[MizAny]) {
		elems = e // ::: elems
	}
	
}
class MizJustifiedTheorem(val name : String, val prop : MizProposition) extends MizAny

class MizDefinitionBlock(val defs : List[MizDefinition])
class MizDefTheorem(val kind : String, val nr : Int, val constrkind : String, val constrnr : Int, val prop : MizProposition) extends MizAny
class MizDefiniens() extends MizAny//TODO