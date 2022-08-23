package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import objects._
import parser._
import presentation._
import checking._

import scala.xml._

/** completely unknown elements  */
class UnknownOpaqueElement(val parent: DPath, val format: String, val raw: NodeSeq) extends OpaqueElement

/** a fallback/default parser for completely unknown elements  */
class DefaultOpaqueElementInterpreter extends OpaqueElementInterpreter with OpaqueTextParser with OpaqueChecker with OpaqueHTMLPresenter with OpaqueTextPresenter {
   type OE = UnknownOpaqueElement

   def format = "unknown"

   def fromNode(parent: DPath, nsMap: NamespaceMap, nodes: NodeSeq) = new UnknownOpaqueElement(parent, format, nodes)

   def fromString(oP: ObjectParser, parent: DPath, pu: ParsingUnit)(implicit eh: ErrorHandler): OE = {
      val elem = scala.xml.Text(pu.term)
      new UnknownOpaqueElement(parent, format, elem)
   }

   def toString(oP: ObjectPresenter, oe: OpaqueElement)(implicit rh: RenderingHandler): Unit = {
     rh(oe.raw.text)
   }
   
   def check(oC: ObjectChecker, context: Context, rules: RuleSet, oe : OpaqueElement)(implicit ce: CheckingEnvironment): Unit = {}

   def toHTML(oP: ObjectPresenter, oe: OpaqueElement)(implicit rh : RenderingHandler): Unit = {
      rh(<pre>{oe.raw.text}</pre>)
   }
}
