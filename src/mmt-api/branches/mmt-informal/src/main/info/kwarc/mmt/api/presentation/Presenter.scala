package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import objects._
import objects.Conversions._
import parser._
import documents._
import modules._
import archives._

trait StructurePresenter {
   def apply(e : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler)
   def asString(e : StructuralElement): String = {
      val sb = new StringBuilder
      apply(e)(sb)
      sb.get
   }
}

trait ObjectPresenter {
   def apply(o: Obj)(implicit rh : RenderingHandler)
   def asString(o: Obj): String = {
      val sb = new StringBuilder
      apply(o)(sb)
      sb.get
   }
}

/**
 * A Presenter transforms MMT content into presentation
 */
trait Presenter extends archives.Exporter with StructurePresenter with ObjectPresenter {  
  def isApplicable(format: String): Boolean
  
  /** applied to each leaf document (i.e., .omdoc file) */
  def exportDocument(doc : documents.Document, bf: archives.BuildTask) = apply(doc, true)(rh)
  /** applied to each theory */
  def exportTheory(thy : DeclaredTheory, bf: BuildFile) = apply(thy, true)(rh)
  /** applied to each view */
  def exportView(view : DeclaredView, bf: BuildFile) = apply(view, true)(rh)
  /** applied to every namespace
   *  @param dpath the namespace
   *  @param namespaces the sub-namespace in this namespace
   *  @param modules the modules in this namespace
   */
  def exportNamespace(dpath: DPath, bd: BuildDir, namespaces: List[(BuildDir,DPath)], modules: List[(BuildFile,MPath)]) {
    //nothing to do
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
   
   private def getNotation(controller: frontend.Controller, p: ContentPath) : Option[NotationContainer] = {
      controller.globalLookup.getO(p) flatMap {
         case c: symbols.Constant => if (c.notC.isDefined) Some(c.notC) else None
         case p: patterns.Pattern => if (p.notC.isDefined) Some(p.notC) else None
         case _ => None
      }
   }
   
   /** transforms into pragmatic form and tries to retrieve a notation
    *  
    *  if the term but not the pragmatic form has a notation, the strict form is retained
    *  
    *  @param parsing if true return a parsing notation even if a presentation notation is available 
    */
   def getNotation(controller: frontend.Controller, o: Obj) : (Obj, List[Position], Option[NotationContainer]) = {
      //TODO: try (lib.preImage(p) flatMap (q => getDefault(NotationKey(Some(q), key.role)))
      def tryTerm(t: Term) = t match {
         case ComplexTerm(p, sub, vars, args) => getNotation(controller, p)
         case OMID(p) => getNotation(controller, p)
         case _ => None
      }
      o match {
         case t: Term =>
            val (tP, _, posP) = controller.pragmatic.pragmaticHeadWithInfo(t)
            tryTerm(tP) match {
               case Some(n) => (tP, posP, Some(n))
               case None    => tryTerm(t) match {
                  case Some(n) => (t, Position.positions(t), Some(n))
                  case None => (tP, posP, None)
               }
            }
         case _ =>
            (o, Position.positions(o), None)
      }
   }   
}

/**
 * A Presenter that returns text based on the toString method
 * 
 * This Presenter can be used without initialization.
 */
object TextPresenter extends Presenter {
   val key = "present-text"
   val outDim = Dim("export", "presentation", "text")
   override def outExt = "txt"
   def isApplicable(format: String) = format == "text"
   def apply(c : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) {
      rh(c.toString)
   }
   def apply(o: Obj)(implicit rh : RenderingHandler) {
      rh(o.toString)
   }
}

/**
 * A Presenter that returns XML based on the toNode method
 * 
 * This Presenter can be used without initialization.
 */
object OMDocPresenter extends Presenter {
  val key = "present-omdoc"
  val outDim = Dim("export", "presentation", "omdoc")
  override def outExt = "omdoc"
  def isApplicable(format: String) = format == "xml"
   private val pp = new scala.xml.PrettyPrinter(100, 2)
   def apply(c : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler) {
      val sb = new scala.collection.mutable.StringBuilder
      pp.format(c.toNode, sb)
      //rh(sb.result)
      rh(c.toNode) //must be type node, otherwise XML rh will escape it
   }
   def apply(o: Obj)(implicit rh : RenderingHandler) {
      val sb = new scala.collection.mutable.StringBuilder
      pp.format(o.toNode, sb)
      //rh(sb.result)
      rh(o.toNode)
   }
}
