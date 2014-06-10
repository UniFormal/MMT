package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._
import frontend._
import parser._
import archives._
import ontology._

/** type of continuation functions passed to an [[ObjectChecker]] to report dependencies */
trait RelationHandler {
   def apply(r: RelationalElement)
}

object RelationHandler {
   /** does nothing */
   def ignore = new RelationHandler {def apply(r: RelationalElement) {}}
}

/**
 * checks objects
 * 
 * see also [[Checker]]
 */
trait ObjectChecker extends Extension {
   def apply(cu: CheckingUnit)(implicit errorCont: ErrorHandler, relCont: RelationHandler)
}

object ObjectChecker {
   /** does nothing */
   def ignore = new ObjectChecker {
      def apply(cu: CheckingUnit)(implicit errorCont: ErrorHandler, relCont: RelationHandler) {}
   }
}

/**
 * checks structural elements
 * 
 * see also [[Checker]]
 */
trait StructureChecker extends Extension {
   /**
    * checks a StructuralElement
    * @param e the element to check
    */
   def apply(e : StructuralElement)(implicit errorCont: ErrorHandler, relCont: RelationHandler)
   /** checks a StructuralElement, given by its URI */
   def apply(p: Path)(implicit errorCont: ErrorHandler, relCont: RelationHandler) {
      apply(controller.get(p))
   }
}


/**
 * the designated super class for all checkers
 */
abstract class Checker(val objectLevel: ObjectChecker) extends StructureChecker with ObjectChecker with LeveledExtension {
  /** relegates to objectChecker */
  def apply(cu: CheckingUnit)(implicit errorCont: ErrorHandler, relCont: RelationHandler) =
     objectLevel(cu)
}

abstract class Interpreter(parser: Parser, checker: Checker) extends Importer {
   def importDocument(bf: BuildTask, seCont: documents.Document => Unit) {
      
   }
}