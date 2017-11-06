package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import objects._
import symbols._
import utils.xml.attr

import scala.xml._

/** helper functions */
object OpaqueXML {
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

import OpaqueXML._

/**
 * Opaque content stored in an XML-tree structure with intermixed with [[Obj]]ects
 *
 * MMT objects O are stored in 'node' using pointer <mmt index="i"/> where i is the index of O in 'terms'.  
 */
class OpaqueXML(val parent: DPath, val format: String, val node: Node, val terms: List[TermFragmentInXML]) extends OpaqueElement {
   def raw: NodeSeq = mapMMTNodes(node) {case MMTIndex(i) =>
      val tf = terms(i)
      MMTNode(tf.format, tf.rawString)
   }
   override def toString = node.toString
   
   override def getComponents = terms map {tf =>
      DeclarationComponent(tf.comp, tf.tc)
   }
   
   def compatibilityKey = node
   
   override def compatible(that: StructuralElement) = that match {
      case that: OpaqueXML =>
         this.compatibilityKey == that.compatibilityKey
      case _ => false
   }
}

case class TermFragmentInXML(index: Int, format: String, tc: TermContainer) {
   def comp = OtherComponent("ext-"+index.toString)
   def rawString = tc.read.getOrElse("")
}
