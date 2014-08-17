package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import frontend._
import objects._
import objects.Conversions._
import notations._
import documents._
import modules._
import archives._

/**
 * the type of presenters that can handle objects
 *  
 * see also [Presenter]]
 */
trait ObjectPresenter extends Extension {
   def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler)
   def asString(o: Obj, origin: Option[CPath] = None): String = {
      val sb = new StringBuilder
      apply(o, origin)(sb)
      sb.get
   }
}

/**
 * the type of presenters that can handle structural elements
 *  
 * see also [Presenter]]
 */
trait StructurePresenter extends Extension {
   def apply(e : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler)
   def asString(e : StructuralElement): String = {
      val sb = new StringBuilder
      apply(e)(sb)
      sb.get
   }
}

/**
 * the designated super class of all presenters
 */
abstract class Presenter(val objectLevel: ObjectPresenter)
   extends archives.Exporter with StructurePresenter with ObjectPresenter with LeveledExtension {

  /** relegates to objectPresenter */
  def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) = objectLevel(o, origin)
   
  def outDim = Dim("export", "presentation", key)

  def isApplicable(format: String): Boolean
  
  def exportDocument(doc : documents.Document, bf: BuildTask) = apply(doc, true)(rh)
  def exportTheory(thy : DeclaredTheory, bf: BuildTask) = apply(thy, true)(rh)
  def exportView(view : DeclaredView, bf: BuildTask) = apply(view, true)(rh)
  /** does nothing */
  def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {}
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
            case 1 => outerPrecedence >= innerPrecedence && (innerNotation.isLeftOpen || innerNotation.arity.numNormalArgs > 1)
            case 0 => outerPrecedence >= innerPrecedence && (innerNotation.isLeftOpen || innerNotation.isRightOpen)
            case -1 => outerPrecedence >= innerPrecedence && (innerNotation.isRightOpen || innerNotation.arity.numNormalArgs > 1)
            case _ => throw ImplementationError("illegal position")
         }
         if (yes) 1 else 0
      }
   }
   
   def getNotation(controller: frontend.Controller, p: ContentPath, twoDim: Boolean) : Option[TextNotation] = {
      val notC = controller.globalLookup.getO(p) flatMap {
         case c: symbols.Constant => if (c.notC.isDefined) Some(c.notC) else None
         case p: patterns.Pattern => if (p.notC.isDefined) Some(p.notC) else None
         case _ => None
      }
      notC.flatMap(n => if (twoDim) n.getPresent else n.getParse)
   }
}

/**
 * A Presenter that returns text based on the toString method
 * 
 * This Presenter can be used without initialization.
 */
object ObjectTextPresenter extends ObjectPresenter {
   def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) {
      rh(o.toString)
   }
}

/**
 * A Presenter that returns text based on the toString method
 * 
 * This Presenter can be used without initialization.
 */
object TextPresenter extends Presenter(ObjectTextPresenter) {
   val key = "present-text"
   override def outExt = "txt"
   def isApplicable(format: String) = format == "text"
   def apply(c : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) {
      rh(c.toString)
   }
}

/**
 * A Presenter that returns XML based on the toNode method, i.e., OpenMath
 * 
 * This Presenter can be used without initialization.
 */
object OpenMathPresenter extends ObjectPresenter {
   private val pp = new scala.xml.PrettyPrinter(100, 2)
   def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) {
      val sb = new scala.collection.mutable.StringBuilder
      pp.format(o.toNode, sb)
      rh(sb.result)
   }
}

/**
 * A Presenter that returns XML based on the toNode method, i.e., OMDoc
 * 
 * This Presenter can be used without initialization.
 */
object OMDocPresenter extends Presenter(OpenMathPresenter) {
   val key = "present-omdoc"
   override def outExt = "omdoc"
   def isApplicable(format: String) = format == "xml"
   private val pp = new scala.xml.PrettyPrinter(100, 2)
   def apply(c : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) {
      val sb = new scala.collection.mutable.StringBuilder
      pp.format(c.toNode, sb)
      rh(sb.result)
   }
}