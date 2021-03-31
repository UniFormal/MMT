package info.kwarc.mmt.stex.Extensions

import info.kwarc.mmt.api.{CPath, DefComponent, GlobalName, MPath, NamespaceMap, Path, StructuralElement, TypeComponent}
import info.kwarc.mmt.api.frontend.Extension
import info.kwarc.mmt.api.modules.Theory
import info.kwarc.mmt.api.notations.TextNotation
import info.kwarc.mmt.api.objects.{Free, FreeOrAny, OMA, OMAorAny, OMID, OMMOD, OMS, Obj, Term, VarDecl}
import info.kwarc.mmt.api.ontology.{Binary, CustomBinary, RelationalElement, RelationalExtractor, RelationalReader, Unary}
import info.kwarc.mmt.api.refactoring.AcrossLibraryTranslator
import info.kwarc.mmt.api.symbols.{RuleConstant, RuleConstantInterpreter}
import info.kwarc.mmt.api.utils.{MMTSystem, XMLEscaping}
import info.kwarc.mmt.api.web.{ServerRequest, ServerResponse}
import info.kwarc.mmt.stex.features.TheoremFeature
import info.kwarc.mmt.stex.xhtml.{HasHeadSymbol, PreConstant, PreDocument, PreElement, PreFeature, PreParent, PreRuleConstant, PreStructure, PreTheory, ToScript, XHTML, XHTMLArityComponent, XHTMLComponent, XHTMLDecl, XHTMLDefComponent, XHTMLMacroNameComponent, XHTMLModule, XHTMLNode, XHTMLNotationComponent, XHTMLNotationFragment, XHTMLOMDoc, XHTMLPrecedence, XHTMLStexArg, XHTMLTerm, XHTMLText, XHTMLTypeComponent}
import info.kwarc.mmt.stex.{STeX, STeXServer}

import scala.util.Try
import scala.xml.{Elem, Node}

abstract class Translator(val language : String) {
  def applicable(tm : Term) : Boolean
  def translate(tm : Term) : (Term,List[GlobalName])
}

trait STeXExtension extends Extension {
  lazy val server = controller.extman.get(classOf[STeXServer]).head
  def xhtmlRules : List[PartialFunction[Node,XHTMLNode]] = Nil
  def serverReturn(request: ServerRequest): Option[ServerResponse] = None
  def doHeader(head : XHTMLNode,body : XHTMLNode) : Unit = {}
  // def translators : List[Translator] = Nil
}