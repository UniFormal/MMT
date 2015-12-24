package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import objects._
import parser._
import presentation._
import checking._
import symbols._
import utils.xml.attr

import scala.xml._

object OpaqueHTML {
   object MMTNode {
      def apply(format: String, s: String) = <script type={"mmt/"+format}>{s}</script>
      def unapply(n: Node) = n match {
         case n: Elem if n.label == "script" && attr(n, "type").startsWith("mmt/") =>
            val format = attr(n, "type").substring(4)
            Some((format,n.text))
         case _ => None
      }
   }
   object MMTIndex {
      def apply(i: Int) = <mmt index={i.toString}/>
      def unapply(n: Node) = n match {
         case n @ <mmt/> => Some(attr(n, "index").toInt)
         case _ => None
      }
   }
   def mapMMTNodes(n: Node)(f: Node => Node): Node = n match {
      case MMTNode(_,_) => f(n)
      case MMTIndex(_) => f(n)
      case n: Elem => n.copy(child = n.child.map(c => mapMMTNodes(c)(f)))
      case n => n
   }
}

import OpaqueHTML._

case class TermFragmentInHTML(index: Int, format: String, tc: TermContainer) {
   def comp = OtherComponent(index.toString)
   def rawString = tc.read.getOrElse("")
}

/** html intermixed with [[Obj]]ects
 *
 * MMT objects are initially provided using <script type="mmt">OBJ</script>.
 * For storage in 'node' they are replaced with <mmt index="i"/> where i is the index of OBJ in 'terms'.  
 */
class OpaqueHTML(val parent: DPath, val node: Node, val terms: List[TermFragmentInHTML]) extends OpaqueElement {
   def format = "html"
   def raw: NodeSeq = mapMMTNodes(node) {case MMTIndex(i) =>
      val tf = terms(i)
      MMTNode(tf.format, tf.rawString)
   }
   override def toString = node.toString
   
   override def getComponents = terms map {tf =>
      DeclarationComponent(tf.comp, tf.tc)
   }
}

/** very rough proof of concept, should be refined a lot */
class HTMLInterpreter extends OpaqueElementInterpreter
                         with OpaqueChecker with OpaqueHTMLPresenter {
   type OE = OpaqueHTML
   override def logPrefix = "opaque_html"
   
   def format = "html"
   override def isApplicable(f: String) = super.isApplicable(f) || f == "H"
   
   def fromNode(parent: DPath, nsMap: NamespaceMap, nodes: NodeSeq): OpaqueHTML = {
      val node = if (nodes.length == 1) nodes.head else <div>{nodes}</div>
      var terms: List[TermFragmentInHTML] = Nil
      var i = -1
      val nodeM = mapMMTNodes(node) {case MMTNode(format, s) =>
         i += 1
         val tc = TermContainer(s)
         val tf = new TermFragmentInHTML(i, format, tc)
         terms ::= tf
         MMTIndex(i)
      }
      new OpaqueHTML(parent, nodeM, terms.reverse)
   }
  
   def check(oC: ObjectChecker, context: Context, oe : OpaqueElement)(implicit ce: CheckingEnvironment) {
      val oh = downcast(oe)
      oh.terms foreach {tc =>
         /*f.tc.parsed.foreach {t =>
            val cu = CheckingUnit.byInference(Some(oe.path $ f.comp), context, t)
            oC(cu, rules) TODO get the rules efficiently from somewhere
         }*/
      }
   }
   
   def toHTML(oP: ObjectPresenter, oe: OpaqueElement)(implicit rh : RenderingHandler) {
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