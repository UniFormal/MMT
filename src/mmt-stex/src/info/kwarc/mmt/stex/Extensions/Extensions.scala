package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{GlobalName, StructuralElement}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.STeXServer
import info.kwarc.mmt.stex.xhtml.{HTMLNode, HTMLRule}

trait STeXExtension extends Extension {
  lazy val server = controller.extman.get(classOf[STeXServer]).head
  def rules : List[HTMLRule] = Nil
  def serverReturn(request: ServerRequest): Option[ServerResponse] = None
  def doHeader(head : HTMLNode,body : HTMLNode) : Unit = {}
  //def checkingRules : List[PartialFunction[(StructuralElement,SemanticParsingState), StructuralElement]] = Nil
  // def translators : List[Translator] = Nil
}