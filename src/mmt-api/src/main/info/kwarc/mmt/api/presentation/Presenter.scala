package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.modules._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.objects._
import info.kwarc.mmt.api.symbols._

/**
  * Presents [[Obj MMT objects]], in particular [[Term]]s.
  *
  * @example ''objectPresenter.asString(someObject)'' to present a single [[Obj object]]
  *          to a string.
  *          Use ''apply()'' with a [[StringBuilder]] when presenting multiple objects in
  *          a row for efficiency reasons.
  *
  * @see [[Presenter]]
  */
trait ObjectPresenter extends Extension {
   def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler): Unit
   def asString(o: Obj, origin: Option[CPath] = None): String = {
      val sb = new StringBuilder
      apply(o, origin)(sb)
      sb.get
   }
   def asXML(o: Obj, origin: Option[CPath] = None): scala.xml.Node = {
      val s = asString(o, origin)
      try {
         scala.xml.XML.loadString(s)
      } catch {
         case t : Throwable =>
          print("")
          throw t
      }
   }
}

/**
  * Presents [[StructuralElement]]s to some output syntax.
  *
  * @example ''structurePresenter.asString(someObject)'' to present a single
  *          [[StructuralElement]] to a string.
  *          Use ''apply()'' with a [[StringBuilder]] when presenting multiple objects in
  *          a row for efficiency reasons.
  *
  * @see [[Presenter]]
  */
trait StructurePresenter extends Exporter {
   /**
    * @param e the element to present
    * @param standalone if true, include appropriate header and footer
    * @param rh output stream
    */
   def apply(e : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler): Unit
   def asString(e : StructuralElement): String = {
      val sb = new StringBuilder
      apply(e)(sb)
      sb.get
   }

   def exportDocument(doc : documents.Document, bf: BuildTask): Unit = apply(doc, standalone = true)(rh)
   def exportTheory(thy : Theory, bf: BuildTask): Unit = apply(thy, standalone = true)(rh)
   def exportView(view : View, bf: BuildTask): Unit = apply(view, standalone = true)(rh)
   override def exportDerivedModule(dm: DerivedModule, bf: BuildTask): Unit = apply(dm, standalone = true)(rh)
   /** does nothing */
   def exportNamespace(dpath: DPath, bd: BuildTask, namespaces: List[BuildTask], modules: List[BuildTask]): Unit = {}
}

/**
  * Presents MMT knowledge items to some output syntax.
  *
  * Combining a [[StructurePresenter]] and an [[ObjectPresenter]], it can present
  * both [[StructuralElement]]s and [[Obj objects]].
  *
  * @see [[MMTSyntaxPresenter]], of which a singleton instance is exposed via
  *      [[Controller.presenter]]
  *
  * @example ''presenter.asString(someObject)'' to present a single
  *          [[StructuralElement]] to a string.
  *          Use ''apply()'' with a [[StringBuilder]] when presenting multiple objects in
  *          a row for efficiency reasons.
  *
  * This class is the designated super class of all presenters.
  *
  * The format for which a Presenter is applicable is the same as the key used to run it as an exporter.
  *
  * @param objectLevel An [[ObjectPresenter]] to which all calls to the implemented
  *                    [[ObjectPresenter]] trait are delegated to.
  */
abstract class Presenter(val objectLevel: ObjectPresenter)
   extends StructurePresenter with ObjectPresenter with LeveledExtension {

  def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler): Unit = objectLevel(o, origin)

  override def outDim: Dim = Dim("export", "presentation", key)
}

/**
  * delimitation status of an argument within the outer notation
  * @param position
  *  -1: left-open argument
  *   0: delimited argument
  *   1: right-open argument
  * @param next next marker, if any
  */
case class Delimitation(position: Int, next: Option[Marker])

object Presenter {
   /**
    * a utility function that decides whether to place brackets
    * @param outerPrecedence the precedence of the outer notation
    * @param delimitation delimitation status of the argument within the outer notation
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
    * - suppress brackets for left/right association
    * - when multiple arguments occur without delimiter, brackets are usually needed
    * - generally, omitting brackets may screw up parsing
    */
   def bracket(outerPrecedence: Precedence, delimitation: Delimitation, innerNotation: TextNotation) : Int = {
      val innerPrecedence = innerNotation.precedence
      if (outerPrecedence == Precedence.neginfinite || innerPrecedence == Precedence.infinite) {
         // brackets explicitly prevented
         -1
      } else if (!innerNotation.isLeftOpen && !innerNotation.isRightOpen) {
         // inner notations brings its own brackets
         -1
      } else {
         val rightAssocDelimFollowsInOuter = delimitation.next match {
           case Some(d: Delim) => !d.associatesToLeft
           case _ => false
         }
        val rightAssocDelimStartsInInner = innerNotation.pullsFromLeft
         val yes = delimitation.position match {
            case 1 =>
              //the = case puts brackets into x * (y / z) if * and / have the same precedence
              //the exception for right-associativity avoids brackets in a => (b => c)
              (outerPrecedence > innerPrecedence && innerNotation.isLeftOpen) ||
              (outerPrecedence == innerPrecedence && innerNotation.isLeftOpen && !rightAssocDelimStartsInInner)
            case 0 =>
              outerPrecedence >= innerPrecedence && (innerNotation.isLeftOpen || innerNotation.isRightOpen)
            case -1 =>
              //the = case omits brackets in (x * y) / z if * and / have the same precedence
              //the exception for right-associativity adds brackets in (a => b) => c)
              (outerPrecedence > innerPrecedence && innerNotation.isRightOpen) ||
              (outerPrecedence == innerPrecedence && innerNotation.isRightOpen && rightAssocDelimFollowsInOuter)
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
  * Presents [[Obj objects]] by means of calling their [[Obj.toString]] method.
  *
  * @example ''ObjectTextPresenter.asString(obj)''
  */
object ObjectTextPresenter extends ObjectPresenter {
   def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler): Unit = {
      rh(o.toString)
   }
}

/**
  * Presents MMT knowledge items by means of calling their ''toString'' method.
  *
  * @example ''TextPresenter.asString(item)''
  */
object TextPresenter extends Presenter(ObjectTextPresenter) {
   val key = "present-text"
   override def outExt = "txt"
   override def isApplicable(format: String): Boolean = format == "text"
   def apply(c : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler): Unit = rh(c.toString)
}

/**
  * Presents [[Obj objects]] to OMDoc XML by means of calling their [[Obj.toNode]] method.
  *
  * @example ''OpenMathPresenter.asString(obj)''
  */
object OpenMathPresenter extends ObjectPresenter {
   def apply(o: Obj, origin: Option[CPath])(implicit rh : RenderingHandler): Unit = {
      rh << o.toOBJNode.toString
   }
}

/**
  * Presents MMT knowledge items to OMDoc XML by means of calling their ''toNode'' method.
  *
  * @example ''OMDocPresenter.asString(item)''
  */
object OMDocPresenter extends Presenter(OpenMathPresenter) {
   val key = "present-omdoc"
   override def outExt = "omdoc"
   override def isApplicable(format: String): Boolean = format == "xml"
   def apply(c : StructuralElement, standalone: Boolean = false)(implicit rh : RenderingHandler): Unit = {
      rh << c.toNode.toString
   }
}
