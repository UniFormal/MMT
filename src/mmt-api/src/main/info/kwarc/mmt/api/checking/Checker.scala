package info.kwarc.mmt.api.checking

import info.kwarc.mmt.api._
import objects._
import frontend._
import parser._
import archives._
import ontology._

/** type of continuation functions passed to an [[ObjectChecker]] to report dependencies */
trait RelationHandler {
   def apply(r: RelationalElement): Unit
}

object RelationHandler {
   /** does nothing */
   def ignore = new RelationHandler {def apply(r: RelationalElement): Unit = {}}
}

case class CheckingEnvironment(simplifier: uom.Simplifier, errorCont: ErrorHandler, reCont: RelationHandler, task: MMTTask) {
  def simpEnv = new uom.SimplificationEnvironment(false, errorCont, task)
}

/**
 * checks objects
 *
 * see also [[Checker]]
 */
trait ObjectChecker extends Extension {
  def lookup = controller.globalLookup
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
 *
 * INVARIANTS: apply(se) must be equivalent to
 * - for ContainerElement's: applyElementBegin(se) + se.getPrimitiveDeclarations.foreach(apply) + applyElementEnd(se)
 * - for other elements: apply(se) must be equivalent to applyElementBegin(se)
 *
 * That way all calls to the [[StructureParserContinuations]] together check the entire element.
 */
trait StructureChecker extends FormatBasedExtension {
   /** checks the entire StructuralElement */
   def apply(e : StructuralElement)(implicit env: CheckingEnvironment): Unit
   /** checks the header of a StructuralElement, i.e., everything except for its body */
   def applyElementBegin(e : StructuralElement)(implicit ce: CheckingEnvironment): Unit
   /** checks the end of a StructuralElement (e.g., global conditions like totality of a view) */
   def applyElementEnd(e: ContainerElement[_])(implicit ce: CheckingEnvironment): Unit

  /** checks a StructuralElement, given by its URI */
   def apply(p: Path)(implicit env: CheckingEnvironment): Unit = {
      apply(controller.get(p))
   }
}

/** trivial checkers that do nothing */
object NullChecker {
   class Objects extends ObjectChecker {
      def apply(cu: CheckingUnit, rules: RuleSet)(implicit env: CheckingEnvironment) = {
        val tm = cu.judgement.wfo match {
           case t: Term => t
           case c: Context => Context.AsTerm(c)
           case _ => throw ImplementationError("cannot check this object")
        }
        CheckingResult(false, None, tm)
      }
   }
   class Structure extends Checker(new Objects) {
      override def init(c: Controller): Unit = {super.init(c); objectLevel.init(c)}
      val id = "null"
      def apply(e : StructuralElement)(implicit env: CheckingEnvironment): Unit = {}
      def applyElementBegin(e : StructuralElement)(implicit ce: CheckingEnvironment): Unit = {}
      def applyElementEnd(e: ContainerElement[_])(implicit ce: CheckingEnvironment): Unit = {}
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

