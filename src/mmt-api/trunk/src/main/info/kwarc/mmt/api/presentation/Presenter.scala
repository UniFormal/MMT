package info.kwarc.mmt.api.presentation
import info.kwarc.mmt.api._
import objects._

/**
 * A Presenter transforms MMT content into presentation
 */
abstract class Presenter extends frontend.Extension {
   def isApplicable(format: String): Boolean
   def apply(c : StructuralElement, rh : RenderingHandler)
   def apply(o: Obj, rh: RenderingHandler)
}

/**
 * A Presenter that returns text based on the toString method
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