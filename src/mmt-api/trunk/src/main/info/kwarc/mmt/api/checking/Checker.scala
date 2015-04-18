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

/**
 * checks structural elements
 * 
 * see also [[Checker]]
 */
trait StructureChecker extends FormatBasedExtension {
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

/** trivial checkers that do nothing */
object NullChecker {
   object objects extends ObjectChecker {
      def apply(cu: CheckingUnit, rules: RuleSet)(implicit env: CheckingEnvironment) {}
   }
   object structure extends Checker(objects) {
      val id = "null"
      def apply(e : StructuralElement)(implicit env: CheckingEnvironment) {}
   }
}

/**
 * the designated super class for all checkers
 */
abstract class Checker(val objectLevel: ObjectChecker) extends StructureChecker with ObjectChecker with LeveledExtension {
  val id : String
  def isApplicable(s: String) = s == id
  /** relegates to objectChecker */
  def apply(cu: CheckingUnit, rules: RuleSet)(implicit env: CheckingEnvironment) =
     objectLevel(cu, rules)
}

