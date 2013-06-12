package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import parser._

/**
 * A Presenter transforms MMT content into presentation
 */
abstract class Presenter extends frontend.Extension {
   def isApplicable(format: String): Boolean
   def apply(e : StructuralElement, rh : RenderingHandler)
   def apply(o: Obj, rh: RenderingHandler)
   def asString(e : StructuralElement): String = {
      val sb = new StringBuilder
      apply(e, sb)
      sb.get
   }
   def asString(o: Obj): String = {
      val sb = new StringBuilder
      apply(o, sb)
      sb.get
   }
   /** transforms into pragmatic form and tries to retrieve a notation
    *  
    *  if the term but not the pragmatic form has a notation, the strict form is retained 
    */
   protected def getNotation(t: Term) : (Term, List[Position], Option[TextNotation]) = {
      //TODO: try (lib.preImage(p) flatMap (q => getDefault(NotationKey(Some(q), key.role)))
      def tryTerm(t: Term): Option[TextNotation] = t match {
         case ComplexTerm(p, args, vars, scs) =>
            controller.get(p) match {
               case c: symbols.Constant => c.not
               case p: patterns.Pattern => p.not
               case _ => None
            }
         case _ => None
      }
      val (tP, posP) = controller.pragmatic.pragmaticHeadWithPositions(t)
      tryTerm(tP) match {
         case Some(n) => (tP, posP, Some(n))
         case None    => tryTerm(t) match {
            case Some(n) => (t, Position.positions(t), Some(n))
            case None => (tP, posP, None)
         }
      }
   }   
}

/** helper object */
object Presenter {
   /**
    * a utility function that decides whether to place brackets
    * @param outerPrecedence the precedence of the outer notation
    * @param delimitation delimitation status of an argument within the outer notation
    *   -1: left-open argument
    *   0: delimited argument
    *   1: right-open argument
    * @param innerNotation the notation used to render the argument
    * @return information on whether brackets should be placed
    *   < 0: brackets to be avoided
    *   = 0: brackets optional
    *   > 0: brackets needed
    */
    /* TODO bracketing improvements
    * - independent of precedence, often no brackets are needed when recursing
    *     - from the left argument of a left-open notation into a right-closed notation
    *     - from a middle argument into a left- and right-closed notation
    *     - the right argument of a right-open notation into a left-closed notation
    * - when multiple arguments occur without delimiter, brackets are usually needed
    * - generally, omitting brackets may screw up parsing 
    */
   def bracket(outerPrecedence: Precedence, delimitation: Int, innerNotation: TextNotation) : Int = {
      val innerPrecedence = innerNotation.precedence
      if (outerPrecedence == Precedence.neginfinite || innerPrecedence == Precedence.infinite)
         -1
      else {
         val yes = delimitation match {
            //the = case puts brackets into x * (y / z) if * and / have the same precedence
            case 1 => outerPrecedence >= innerPrecedence && innerNotation.isLeftOpen
            case 0 => outerPrecedence >= innerPrecedence && (innerNotation.isLeftOpen || innerNotation.isRightOpen)
            case -1 => outerPrecedence >= innerPrecedence && innerNotation.isRightOpen
            case _ => throw ImplementationError("illegal position")
         }
         if (yes) 1 else 0
      }
   }
}

/**
 * A Presenter that returns text based on the toString method
 * 
 * This Presenter can be used without initialization.
 */
object TextPresenter extends Presenter {
   def isApplicable(format: String) = format == "text"
   def apply(c : StructuralElement, rh : RenderingHandler) {
      rh(c.toString)
   }
   def apply(o: Obj, rh: RenderingHandler) {
      rh(o.toString)
   }
}

/**
 * A Presenter that returns XML based on the toNode method
 * 
 * This Presenter can be used without initialization.
 */
object OMDocPresenter extends Presenter {
   def isApplicable(format: String) = format == "xml"
   private val pp = new scala.xml.PrettyPrinter(100, 2)
   def apply(c : StructuralElement, rh : RenderingHandler) {
      val sb = new scala.collection.mutable.StringBuilder
      pp.format(c.toNode, sb)
      rh(sb.result)
   }
   def apply(o: Obj, rh: RenderingHandler) {
      val sb = new scala.collection.mutable.StringBuilder
      pp.format(o.toNode, sb)
      rh(sb.result)
   }
}
