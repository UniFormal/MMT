package mizar.objects

class MizArticle(val title : String, var elems : List[MizAny]) {
	def addElem(e : MizAny) {
		elems = e :: elems
	}
	
	def setElems(e : List[MizAny]) {
		elems = e 
	}
	
}

class MizJustifiedTheorem(val name : String, val prop : MizProposition) extends MizAny

class XMLDefinitionBlock(val defs : List[XMLDefinition]) extends MizAny



/**
 * Definitions with structural info
 */
trait MizDefinition extends MizAny
//type (means/is)
trait MizIsDef extends MizDefinition
trait MizMeansDef extends MizDefinition

//Kind (Mode, Functor, Predicate)
trait MizModeDef extends MizDefinition
trait MizFuncDef extends MizDefinition
trait MizPredDef extends MizDefinition

//Def Classes
class MizModeIsDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String],MizTyp)], val retType : Option[MizTyp], val term : MizTerm) extends MizIsDef with MizModeDef
class MizFuncIsDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String],MizTyp)], val retType : MizTyp, val term : MizTerm) extends MizIsDef with MizFuncDef
class MizPredIsDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String],MizTyp)], val term : MizTerm) extends MizIsDef with MizPredDef

class MizModeMeansDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String],MizTyp)], val retType : Option[MizTyp], val form : MizFormula) extends MizMeansDef with MizModeDef 
class MizFuncMeansDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String],MizTyp)], val retType : MizTyp, val form : MizFormula) extends MizMeansDef with MizFuncDef
class MizPredMeansDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String],MizTyp)], val form : MizFormula) extends MizMeansDef with MizPredDef
