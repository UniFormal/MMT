package info.kwarc.mmt.api.presentation

import info.kwarc.mmt.api._
import frontend._
import objects._

class MathMLPresenter(val controller: Controller) extends NotationBasedPresenter {
   def getNotation(o: Obj) = {
      val (oP, pos, ncOpt) = Presenter.getNotation(controller, o)
      (oP, pos, ncOpt.flatMap(_.getPresent))
   }
   override def doIdentifier(p: ContentPath)(implicit rh : RenderingHandler) {
      val s = p match {
         case OMMOD(m) % name => name.toPath  //not parsable if there are name clashes 
         case _ => p.toPath
      }
      val n = <mo jobad:xref={p.toPath}>{s}</mo>
      rh(n)
   }
   override def doVariable(n: LocalName)(implicit rh : RenderingHandler) {
      val node = <mi>{n.toPath}</mi>
      rh(node)
   }
   override def doOperator(s: String)(implicit rh : RenderingHandler) {
      val n = <mo>{s}</mo>
      rh(n)
   }
   override def doDelimiter(p: GlobalName, d: parser.Delimiter)(implicit rh : RenderingHandler) {
      val n = <mo jobad:xref={p.toPath}>{d.text}</mo>
      rh(n)
   }
   override def doSpace(level: Int)(implicit rh : RenderingHandler) {
      val n = <mspace/>
      rh(n)
   }
}