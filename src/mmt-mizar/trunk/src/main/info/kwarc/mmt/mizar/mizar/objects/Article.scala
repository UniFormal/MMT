package info.kwarc.mmt.mizar.mizar.objects

class MizArticle(val title : String, var elems : List[MizAny]) {
	def addElem(e : MizAny) {
		elems = e :: elems
	}
	
	def setElems(e : List[MizAny]) {
		elems = e 
	}
	
}

class MizNotation(val name : String, val aid : String, val kind : String, val relnr : Int, val constrAid : String, val constrAbsnr : Int, val antonymic : Boolean) extends MizAny
class MizJustifiedTheorem(val aid : String, val nr : Int, val prop : MizProposition) extends MizAny
class MizLemma(val prop : MizProposition) extends MizAny
class MizSet(val nr : Int, val constnr : Int, val term : MizTerm, val typ : MizTyp) extends MizAny
class MizConsider(val nr : Int, val constnr : Int, val prop : MizProposition, val typs : List[MizTyp], val props : List[MizProposition]) extends MizAny
class MizReconsider(val nr : Int, val constnr : Int, val terms: List[(MizTyp,MizTerm)], val prop : MizProposition) extends MizAny

class XMLDefinitionBlock(val defs : List[XMLDefinition])



/**
 * Definitions with structural info
 */
trait MizDefinition extends MizAny

trait MizRedefinition extends MizDefinition
//type (means/is)
trait MizIsDef extends MizDefinition
trait MizMeansDef extends MizDefinition

//Kind (Mode, Functor, Predicate)
trait MizModeDef extends MizDefinition
trait MizFuncDef extends MizDefinition
trait MizPredDef extends MizDefinition
trait MizAttrDef extends MizDefinition

//Def Classes
class MizModeIsDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String],MizTyp)], val retType : Option[MizTyp], val cases : List[(MizTerm,MizFormula)], val term : Option[MizTerm]) extends MizIsDef with MizModeDef
class MizFuncIsDef(val name : Option[String], val aid : String, val kind : String, val absnr : Int, val args : List[(Option[String],MizTyp)], val retType : MizTyp, val cases : List[(MizTerm,MizFormula)], val term : Option[MizTerm]) extends MizIsDef with MizFuncDef
class MizPredIsDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String],MizTyp)],val cases : List[(MizTerm,MizFormula)], val term : Option[MizTerm]) extends MizIsDef with MizPredDef
class MizAttrIsDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String],MizTyp)], val retType : MizTyp, val cases : List[(MizTerm,MizFormula)], val term : Option[MizTerm]) extends MizIsDef with MizAttrDef

class MizModeMeansDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String],MizTyp)], val retType : Option[MizTyp], val cases : List[(MizFormula,MizFormula)], val form : Option[MizFormula]) extends MizMeansDef with MizModeDef 
class MizFuncMeansDef(val name : Option[String], val aid : String, val kind : String,  val absnr : Int, val args : List[(Option[String],MizTyp)], val retType : MizTyp, val cases : List[(MizFormula,MizFormula)], val form : Option[MizFormula]) extends MizMeansDef with MizFuncDef
class MizPredMeansDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String],MizTyp)], val cases : List[(MizFormula,MizFormula)], val form : Option[MizFormula]) extends MizMeansDef with MizPredDef
class MizAttrMeansDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String],MizTyp)], val retType : MizTyp, val cases : List[(MizFormula,MizFormula)], val form : Option[MizFormula]) extends MizMeansDef with MizAttrDef

//StructDef

class MizSelector(val name : Option[String], val aid : String, val absnr : Int, val typ : Option[MizTyp])
class MizStructDef(val name : Option[String], val aid : String, val absnr : Int, val args : List[(Option[String], MizTyp)], val imports : List[MizTyp], val selectors: List[MizSelector]) extends MizAny


//ReDef Classes
class MizModeIsRedef(val name : Option[String], val aid : String, val absnr : Int, val redefaid : String, val absredefnr : Int, val args : List[(Option[String],MizTyp)], val retType : Option[MizTyp],val cases : List[(MizTerm,MizFormula)], val term : Option[MizTerm]) extends MizRedefinition with MizIsDef with MizModeDef
class MizFuncIsRedef(val name : Option[String], val aid : String, val kind : String, val absnr : Int, val redefaid : String, val absredefnr : Int, val args : List[(Option[String],MizTyp)], val retType : MizTyp,val cases : List[(MizTerm,MizFormula)], val term : Option[MizTerm]) extends MizRedefinition with MizIsDef with MizFuncDef
class MizPredIsRedef(val name : Option[String], val aid : String, val absnr : Int, val redefaid : String, val absredefnr : Int, val args : List[(Option[String],MizTyp)],val cases : List[(MizTerm,MizFormula)], val term : Option[MizTerm]) extends MizRedefinition with MizIsDef with MizPredDef
class MizAttrIsRedef(val name : Option[String], val aid : String, val absnr : Int, val redefaid : String, val absredefnr : Int, val args : List[(Option[String],MizTyp)], val retType : MizTyp,val cases : List[(MizTerm,MizFormula)], val term : Option[MizTerm]) extends MizRedefinition with MizIsDef with MizAttrDef

class MizModeMeansRedef(val name : Option[String], val aid : String, val absnr : Int, val redefaid : String, val absredefnr : Int, val args : List[(Option[String],MizTyp)], val retType : Option[MizTyp], val cases : List[(MizFormula,MizFormula)], val form : Option[MizFormula]) extends MizRedefinition with MizMeansDef with MizModeDef 
class MizFuncMeansRedef(val name : Option[String], val aid : String, val kind : String, val absnr : Int, val redefaid : String, val absredefnr : Int, val args : List[(Option[String],MizTyp)], val retType : MizTyp, val cases : List[(MizFormula,MizFormula)], val form : Option[MizFormula]) extends MizRedefinition with MizMeansDef with MizFuncDef
class MizPredMeansRedef(val name : Option[String], val aid : String, val absnr : Int, val redefaid : String, val absredefnr : Int, val args : List[(Option[String],MizTyp)], val cases : List[(MizFormula,MizFormula)], val form : Option[MizFormula]) extends MizRedefinition with MizMeansDef with MizPredDef
class MizAttrMeansRedef(val name : Option[String], val aid : String, val absnr : Int, val redefaid : String, val absredefnr : Int, val args : List[(Option[String],MizTyp)], val retType : MizTyp, val cases : List[(MizFormula,MizFormula)], val form : Option[MizFormula]) extends MizRedefinition with MizMeansDef with MizAttrDef
