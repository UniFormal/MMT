package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import objects._
import parser._
import presentation._
import checking._
import symbols._

import scala.xml._

/** html intermixed with [[Obj]]ects in <mmt> elements */
class OpaqueHTML(val parent: DPath, val node: Node, val terms: List[TermContainer]) extends OpaqueElement {
   def format = "html"
   def raw: NodeSeq = node
   override def toString = node.toString
   
   override def getComponents = terms.zipWithIndex map {
      case (tc,i) => DeclarationComponent(OtherComponent(i.toString), tc)
   }
}

/** very rough proof of concept, should be refined a lot */
class HTMLInterpreter extends OpaqueElementInterpreter
                         with OpaqueChecker with OpaqueHTMLPresenter {
   type OE = OpaqueHTML
   override def logPrefix = "opaque_html"
   
   def format = "html"
   override def isApplicable(f: String) = super.isApplicable(f) || f == "H"
   
   protected def mapMathNodes(n: Node)(f: Node => Node): Node = n match {
      case n: Elem if n.label == "mmt" => f(n)
      case n: Elem => n.copy(child = n.child.map(c => mapMathNodes(c)(f)))
      case n => n
   }

   def fromNode(parent: DPath, nsMap: NamespaceMap, nodes: NodeSeq): OpaqueHTML = {
      var terms: List[String] = Nil
      val nodesM = mapMathNodes(nodes.head) {n =>
         val nNew = <mmt index={terms.length.toString}/>
         terms ::= n.text
         nNew
      }
      val tcs = terms.reverseMap {s => TermContainer(s)}
      new OpaqueHTML(parent, nodesM, tcs)
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
      mapMathNodes(oh.node) {n =>
         n match {
            case n @ <mmt/> =>
               val i = utils.xml.attr(n, "index").toInt
               val tc = oh.terms(i)
               tc.get match {
                  case Some(t) => oP(t, None)
                  case None => <pre>{tc.read}</pre>
               }
         }
         n
      }
   }
}