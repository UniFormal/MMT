package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import frontend._
import modules._
import symbols._
import patterns._
import objects._
import utils.MyList.fromList
import collection.immutable.{HashSet, HashMap}

/**  */
class StepBasedElaborator(objectSimplifier: ObjectSimplifier) extends Simplifier(objectSimplifier) {
   var steps: List[ElaborationStep] = Nil
   def apply(se: StructuralElement) {
      steps.foreach {_.apply(se)(this, controller)}
   }
}

/** takes a StructuralElement and produces further StructuralElement that are the result of elaborating the former */
abstract class ElaborationStep {
  /**
   * @param e the StructuralElement that is elaborated
   * @param cont a function that is applied to each produced StructuralElement
   */
   def apply(e: StructuralElement)(implicit elab: StepBasedElaborator, controller: Controller)
}
