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

class CheckingEnvironment(val errorCont: ErrorHandler, val reCont: RelationHandler)


/**
 * checks objects
 * 
 * see also [[Checker]]
 */
trait ObjectChecker extends Extension {
   /**
    * @param cu the checking unit to check
    * @param rules rules to use during checking
    * @param env continuation functions
    */
   def apply(cu: CheckingUnit, rules: RuleSet)(implicit env: CheckingEnvironment)
}

object ObjectChecker {
   /** does nothing */
   def ignore = new ObjectChecker {
      def apply(cu: CheckingUnit, rules: RuleSet)(implicit env: CheckingEnvironment) {}
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
   def apply(e : StructuralElement)(implicit env: CheckingEnvironment)
   /** checks a StructuralElement, given by its URI */
   def apply(p: Path)(implicit env: CheckingEnvironment) {
      apply(controller.get(p))
   }
}


/**
 * the designated super class for all checkers
 */
abstract class Checker(val objectLevel: ObjectChecker) extends StructureChecker with ObjectChecker with LeveledExtension {
  /** relegates to objectChecker */
  def apply(cu: CheckingUnit, rules: RuleSet)(implicit env: CheckingEnvironment) =
     objectLevel(cu, rules)
}

// unused
abstract class Interpreter(parser: Parser, checker: Checker) extends Importer {
   def importDocument(bf: BuildTask, seCont: documents.Document => Unit) {
      
   }
}