package info.kwarc.mmt.latex
import info.kwarc.mmt.api._
import objects._
import presentation._

class LatexPresenter extends Presenter {
   def isApplicable(format: String) = format == "latex"
   def apply(c : StructuralElement, rh : RenderingHandler) {
      rh(c.toString)
   }
   def apply(o: Obj, rh: RenderingHandler) {
      rh(o.toString)
   }

}