package mizar.objects
/**
 * 	objects.Proposition
 * 	Contains classes for handling Mizar Propositions and Formulas	
 * 	@author Mihnea Iancu
 *  @version 10/01/2011
 */

/**
 * Class For Formulas
 */
abstract class MizFormula

/**
 * Classes for each different kind of Formula available in Mizar
 */
class MizNot(val formula : MizFormula) extends MizFormula
class MizAnd(val formulas : List[MizFormula]) extends MizFormula
class MizFor(val typ : MizTyp, val formula : MizFormula) extends MizFormula
class MizPred(val aid : String, val kind : String, val absnr : Int, val terms : List[MizTerm]) extends MizFormula
class MizPrivPred(val nr : Int, val terms : List[MizTerm], val formula : MizFormula) extends MizFormula
class MizIs(val term : MizTerm, val typ : MizTyp) extends MizFormula
class MizVerum() extends MizFormula
class MizErrorFrm() extends MizFormula
class ErrorFrm() extends MizFormula

/**
 * Class For Propositions
 */
class MizProposition(val nr : Int, val form : MizFormula)