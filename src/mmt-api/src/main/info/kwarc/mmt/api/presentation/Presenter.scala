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
   def asXML(o: Obj, origin: Option[CPath] = None): scala.xml.Node = {
      val s = asString(o, origin)
      scala.xml.XML.loadString(s)
   }
}

/**
 * the type of presenters that can handle structural elements
 *
 * see also [[Presenter]]
 */
trait StructurePresenter extends Exporter {
   /**
    * @param e the element to present
    * @param standalone if true, include appropriate header and footer
    * @param rh output stream
    */
   def apply(e : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler)
   def asString(e : StructuralElement): String = {
      val sb = new StringBuilder
      apply(e)(sb)
      sb.get
   }

   def exportDocument(doc : documents.Document, bf: BuildTask) = apply(doc, true)(rh)
   def exportTheory(thy : DeclaredTheory, bf: BuildTask) = apply(thy, true)(rh)
   def exportView(view : DeclaredView, bf: BuildTask) = apply(view, true)(rh)
   /** does nothing */
   def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]) {}
}

/**
 * the designated super class of all presenters
 *
 * The format for which a Presenter is applicable is the same as the key used to run it as an exporter.
 */
abstract class Presenter(val objectLevel: ObjectPresenter)
   extends StructurePresenter with ObjectPresenter with LeveledExtension {

  /** relegates to objectPresenter */
  def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) = objectLevel(o, origin)

  override def outDim = Dim("export", "presentation", key)
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
    *   these cases are handled below, but some heuristic fine-tuning is necessary
    * - when multiple arguments occur without delimiter, brackets are usually needed
    * - generally, omitting brackets may screw up parsing
    */
   def bracket(outerPrecedence: Precedence, delimitation: Int, innerNotation: TextNotation) : Int = {
      val innerPrecedence = innerNotation.precedence
      if (outerPrecedence == Precedence.neginfinite || innerPrecedence == Precedence.infinite)
         // brackets explicitly prevented
         -1
      else if (!innerNotation.isLeftOpen && !innerNotation.isRightOpen)
         // inner notations brings its own brackets
         -1
      else {
         val yes = delimitation match {
            //the = case puts brackets into x * (y / z) if * and / have the same precedence
            case 1 => outerPrecedence >= innerPrecedence && (innerNotation.isLeftOpen) // || innerNotation.arity.numNormalArgs > 1) // the commented-out part does not seem to make sense but it is kept for inspiration
            case 0 => outerPrecedence >= innerPrecedence && (innerNotation.isLeftOpen || innerNotation.isRightOpen)
            case -1 => outerPrecedence >= innerPrecedence && (innerNotation.isRightOpen) // || innerNotation.arity.numNormalArgs > 1)
            case _ => throw ImplementationError("illegal position")
         }
         if (yes) 1 else 0
      }
   }

   def getNotations(controller: frontend.Controller, p: ContentPath, twoDim: Boolean) : List[TextNotation] = {
      val notC = controller.globalLookup.getO(p) flatMap {
         case c: symbols.Constant => if (c.notC.isDefined) Some(c.notC) else None
         case d: symbols.DerivedDeclaration => None //TODO
         case _ => None
      }
      val dim = if (twoDim) 2 else 1
      notC.map(n => n.getNotations(Some(dim), None)).getOrElse(Nil)
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
   override def isApplicable(format: String) = format == "text"
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
   def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler) {
      rh << o.toOBJNode.toString
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
   override def isApplicable(format: String) = format == "xml"
   def apply(c : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) {
      rh << c.toNode.toString
   }
}
