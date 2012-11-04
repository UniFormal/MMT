package info.kwarc.mmt.mizar.mizar.objects

/**
 * 	objects.Reasoning
 * 	Contains classes for handling Mizar Reasoning	
 * 	@author Mihnea Iancu
 *  @version 09/10/2012
 */

trait MizJustification extends MizAny

//inference
trait MizInference extends MizJustification
class MizBy(val refs : List[MizRef]) extends MizInference
class MizFrom(val refs : List[MizRef]) extends MizInference
abstract class MizRef
class MizLocalRef(val nr : Int) extends MizRef
class MizGlobalRef(val nr : Int, val kind : String, val aid : String, val absnr : Int) extends MizRef
class MizErrorInf() extends MizInference

//skipped proof
class MizSkippedProof() extends MizJustification

//proof
class MizProof(val reasoning : MizReasoning) extends MizJustification
class MizReasoning(val proofSteps : List[MizProofItem]) extends MizAny
trait MizProofItem extends MizAny
class MizCaseReasoning() extends MizProofItem//TODO
trait MizSkeletonItem extends MizProofItem //Skeleton Items are items that modify the thesis
trait MizAuxiliaryItem extends MizProofItem // Auxiliary items are items that do not modify the thesis

//Skeleton Items
class MizLet(val nr : Int, val types : List[MizTyp]) extends MizSkeletonItem
class MizConclusion(val jp : MizJustifiedProposition) extends MizSkeletonItem //TODO
class MizAssume(val props : List[MizProposition]) extends MizSkeletonItem
class MizGiven(val nr : Int, val exSt : MizProposition, val types : List[MizTyp], val props : List[MizProposition]) extends MizSkeletonItem
class MizTake(val term : MizTerm) extends MizSkeletonItem
class MizTakeAsVar(val nr : Int, val typ : MizTyp, val term : MizTerm) extends MizSkeletonItem 

//AuxiliaryItems
class MizSet(val nr : Int, val constnr : Int, val term : MizTerm, val typ : MizTyp) extends MizAuxiliaryItem
class MizConsider(val nr : Int, val constnr : Int, val prop : MizProposition, val just : MizJustification,
    val typs : List[MizTyp], val props : List[MizProposition]) extends MizAuxiliaryItem
class MizReconsider(val nr : Int, val constnr : Int, val terms: List[(MizTyp,MizTerm)], 
    val prop : MizProposition, val just : MizJustification) extends MizAuxiliaryItem
class MizDefFunc(val nr : Int, val argTypes : List[MizTyp], val term : MizTerm, val typ : MizTyp) extends MizAuxiliaryItem
class MizDefPred(val nr : Int, val argTypes : List[MizTyp], val form : MizFormula) extends MizAuxiliaryItem

trait MizJustifiedProposition extends MizAuxiliaryItem
class MizNow(val nr : Option[Int], val reasoning : MizReasoning, val blockThesis : MizBlockThesis) extends MizJustifiedProposition
class MizIterEquality(val nr : Option[Int], val term : MizTerm, val iterSteps : List[MizIterStep]) extends MizJustifiedProposition
class MizIterStep(val term : MizTerm, val inf : MizInference) 
class MizPropWithJust(val prop : MizProposition, val just : MizJustification) extends MizJustifiedProposition

class MizThesis(val form : MizFormula)
class MizBlockThesis(val theses : List[MizThesis], val form : MizFormula)
