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

class CheckingEnvironment(val errorCont: ErrorHandler, val reCont: RelationHandler, val task: MMTTask)


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
   def apply(cu: CheckingUnit, rules: RuleSet)(implicit env: CheckingEnvironment): CheckingResult
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
   class Objects extends ObjectChecker {
      def apply(cu: CheckingUnit, rules: RuleSet)(implicit env: CheckingEnvironment) = {
         CheckingResult(false, None, cu.judgement.wfo)
      }
   }
   class Structure extends Checker(new Objects) {
      override def init(c: Controller){super.init(c); objectLevel.init(c)}
      val id = "null"
      def apply(e : StructuralElement)(implicit env: CheckingEnvironment) {}
      def checkElement(e : StructuralElement, cont : Option[Context] = None)(implicit ce: CheckingEnvironment) {}
      def checkElementEnd(e: ContainerElement[_], cont : Option[Context] = None)(implicit ce: CheckingEnvironment) {}
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
  //TODO these may have to be added here eventually, currently not needed
  //def checkElement(e : StructuralElement, cont : Option[Context] = None)(implicit ce: CheckingEnvironment): Unit
  //def checkElementEnd(e: ContainerElement[_], cont : Option[Context] = None)(implicit ce: CheckingEnvironment): Unit
}

