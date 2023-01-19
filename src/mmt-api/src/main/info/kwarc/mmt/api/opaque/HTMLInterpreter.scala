package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import objects._
import parser._
import presentation._
import checking._
import symbols._
import utils.xml.attr

import scala.xml._

import OpaqueXML._

/**
 * MMT objects are read from <script type="mmt">OBJ</script> tags in XHTML
 *
 * very rough proof of concept, should be refined a lot
 */
class HTMLInterpreter extends OpaqueElementInterpreter
                         with OpaqueChecker with OpaqueHTMLPresenter {
   type OE = OpaqueXML
   override def logPrefix = "opaque_html"

   def format = "html"
   override def isApplicable(f: String) = super.isApplicable(f) || f == "H"

   def fromNode(parent: DPath, nsMap: NamespaceMap, nodes: NodeSeq): OpaqueXML = {
      val node = if (nodes.length == 1) nodes.head else <div>{nodes}</div>
      var terms: List[TermFragmentInXML] = Nil
      var i = -1
      val nodeM = mapMMTNodes(node) {case MMTNode(format, s) =>
         i += 1
         val tc = TermContainer(s)
         val tf = new TermFragmentInXML(i, format, tc)
         terms ::= tf
         MMTIndex(i)
      }
      new OpaqueXML(parent, format, nodeM, terms.reverse)
   }

   def check(oC: ObjectChecker, context: Context, rules: RuleSet, oe : OpaqueElement)(implicit ce: CheckingEnvironment): Unit = {
      val oh = downcast(oe)
      oh.terms foreach {tf =>
         tf.tc.parsed.foreach {t =>
            val cu = CheckingUnit.byInference(Some(oe.path $ tf.comp), context, t)
            oC(cu, rules)
         }
      }
   }

   def toHTML(oP: ObjectPresenter, oe: OpaqueElement)(implicit rh : RenderingHandler): Unit = {
      val oh = downcast(oe)
      val nP = mapMMTNodes(oh.node) {case MMTIndex(i) =>
         val tf = oh.terms(i)
         tf.tc.get match {
            case Some(t) =>
               XML.loadString(oP.asString(t, None)) // TODO awkward string rendering and reparsing
            case None =>
               <pre>{tf.rawString}</pre>
         }
      }
      rh(nP)
   }
}
