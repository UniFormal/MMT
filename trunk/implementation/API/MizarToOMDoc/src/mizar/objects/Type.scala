package mizar.objects 
/**
 * 	objects.Type
 * 	Contains classes for handling Mizar Types and Terms.	
 * 	@author Mihnea Iancu
 *  @version 10/01/2011
 */

import mizar.objects._

/**
 * Class For Terms
 */
abstract class MizTerm

/**
 * Classes for each different kind of Term available in Mizar
 */
class MizVar(val nr : Int) extends MizTerm
class MizLocusVar(val nr : Int) extends MizTerm
class MizFreeVar(val nr : Int) extends MizTerm
class MizLambdaVar(val nr : Int) extends MizTerm
class MizConst(val nr : Int, val vid : Int) extends MizTerm
class MizInfConst(val nr : Int) extends MizTerm
class MizNum(val nr : Int) extends MizTerm
class MizFunc(val aid : String, val kind : String, val absnr : Int, val args : List[MizTerm]) extends MizTerm
class MizPrivFunc(val nr : Int, val term : MizTerm, val args : List[MizTerm]) extends MizTerm
class MizFraenkel(val types : List[MizTyp], val term : MizTerm, val form : MizFormula) extends MizTerm
class MizQuaTrm(val term : MizTerm, val typ : MizTyp) extends MizTerm
class MizIt() extends MizTerm
class MizChoice(val typ : MizTyp) extends MizTerm
class MizErrorTrm() extends MizTerm

/**
 * Class for handling Mizar Adjectives
 */
class MizAdjective(nr : Int, value : Boolean, absnr : Int, aid : String, kind : String, pid : Int, terms : List[MizTerm])

/**
 * Class for handling Mizar Clusters
 */
class MizCluster(adjs : List[MizAdjective])

/**
 * Class for handling Mizar Types
 */
class MizTyp(val aid : String, val kind : String, val absnr: Int, val clusters : List[MizCluster], val terms: List[MizTerm])

/**
 * Class for Refs
 */
class MizRef(val nr : Int, val kind : String, val aid : String, val absnr : Int)