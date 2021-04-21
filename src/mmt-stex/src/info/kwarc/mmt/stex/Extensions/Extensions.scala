package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{GlobalName, StructuralElement}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.objects.Term
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.{STeXServer, SemanticParsingState}
import info.kwarc.mmt.stex.xhtml.{XHTMLNode, XHTMLParsingState}


abstract class Translator(val language : String) {
  def applicable(tm : Term) : Boolean
  def translate(tm : Term) : (Term,List[GlobalName])
}

trait STeXExtension extends Extension {
  lazy val server = controller.extman.get(classOf[STeXServer]).head
  def xhtmlRules : List[PartialFunction[(XHTMLNode,XHTMLParsingState), Unit]] = Nil
  def serverReturn(request: ServerRequest): Option[ServerResponse] = None
  def doHeader(head : XHTMLNode,body : XHTMLNode) : Unit = {}
  def checkingRules : List[PartialFunction[(StructuralElement,SemanticParsingState), StructuralElement]] = Nil
  // def translators : List[Translator] = Nil
}