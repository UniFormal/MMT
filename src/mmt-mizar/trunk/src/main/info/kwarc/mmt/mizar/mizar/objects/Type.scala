package info.kwarc.mmt.mizar.mizar.objects
/**
 * 	objects.Type
 * 	Contains classes for handling Mizar Types and Terms.	
 * 	@author Mihnea Iancu
 *  @version 09/03/2011
 */

/**
 * Class For Terms
 */
abstract class MizTerm extends MizAny

/**
 * Classes for each different kind of Term available in Mizar
 */
class MizVar(val nr : Int) extends MizTerm
class MizLocusVar(val nr : Int) extends MizTerm
class MizFreeVar(val nr : Int) extends MizTerm
class MizLambdaVar(val nr : Int) extends MizTerm
class MizConst(val nr : Int, val vid : Int) extends MizTerm
class MizInfConst(val nr : Int) extends MizTerm
class MizConstFunc(val nr : Int, val args : List[MizTerm]) extends MizTerm

class MizNum(val nr : Int) extends MizTerm
class MizFunc(val aid : String, val kind : String, val absnr : Int, val args : List[MizTerm]) extends MizTerm
class MizSchemeFunc(val aid : String, val kind : String, val nr : Int, val schemeNr : Int, val args : List[MizTerm]) extends MizTerm
class MizPrivFunc(val nr : Int, val term : MizTerm, val args : List[MizTerm]) extends MizTerm
class MizFraenkel(val types : List[MizTyp], val term : MizTerm, val form : MizFormula) extends MizTerm
class MizQuaTrm(val term : MizTerm, val typ : MizTyp) extends MizTerm
class MizIt() extends MizTerm
class MizChoice(val typ : MizTyp) extends MizTerm
class MizErrorTrm() extends MizTerm

/**
 * Class for handling Mizar Adjectives
 */
class MizAdjective(val aid : String, val kind : String, val absnr : Int, val pid : String, val value : Boolean, val terms : List[MizTerm])

/**
 * Class for handling Mizar Clusters
 */
class MizCluster(val adjs : List[MizAdjective])

/**
 * Class for handling Mizar Types
 */
class MizTyp(val aid : String, val kind : String, val absnr: Int, val vid : String, val clusters : List[MizCluster], val terms: List[MizTerm], var name : Option[String]) {
	def setName(n : Option[String]) {
		name = n
	}
	
	def ==(that : MizTyp) = this.aid == that.aid && 
					        this.kind == that.kind && 
					        this.absnr == that.absnr && 
					        this.vid == that.vid &&
					        this.clusters.length == that.clusters.length &&
					        this.terms.length == that.terms.length
					        
	def !=(that : MizTyp) = !(==(that))				        
}


