package info.kwarc.mmt.mizar.mizar.objects
/**
 * 	objects.Proposition
 * 	Contains classes for handling Mizar Propositions and Formulas	
 * 	@author Mihnea Iancu
 *  @version 10/03/2011
 */

/**
 * Class For Formulas
 */
abstract class MizFormula extends MizAny

/**
 * Classes for each different kind of Formula available in Mizar
 */
class MizNot(val formula : MizFormula) extends MizFormula
class MizAnd(val formulas : List[MizFormula]) extends MizFormula
class MizFor(val varName : Option[String], val typ : MizTyp, val formula : MizFormula) extends MizFormula
class MizExists(val varName : Option[String], val typ : MizTyp, val formula : MizFormula) extends MizFormula
class MizPred(val aid : String, val kind : String, val absnr : Int, val terms : List[MizTerm]) extends MizFormula 
class MizSchemePred(val aid : String, val kind : String, val nr : Int, val schemeNr : Int, val terms : List[MizTerm]) extends MizFormula
class MizPrivPred(val nr : Int, val terms : List[MizTerm], val formula : MizFormula) extends MizFormula
class MizIs(val term : MizTerm, val typ : MizTyp) extends MizFormula
class MizVerum() extends MizFormula
class MizErrorFrm() extends MizFormula

/**
 * Class For Propositions
 */
class MizProposition(val nr : Option[Int], val propnr : Int, val form : MizFormula) extends MizAny