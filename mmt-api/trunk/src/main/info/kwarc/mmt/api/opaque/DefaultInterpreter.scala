package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import presentation._

import scala.xml._

/** completely unknown elements  */
class UnknownOpaqueElement(val parent: DPath, val raw: NodeSeq) extends OpaqueElement {
   def format = "unknown"
}

/** a fallback/default parser for completely unknown elements  */
object DefaultOpaqueElementInterpreter extends OpaqueElementInterpreter[OpaqueElement]
                                          with OpaqueHTMLPresenter[OpaqueElement] {
   def format = "unknown"

   def fromNode(parent: DPath, nsMap: NamespaceMap, nodes: NodeSeq) = new UnknownOpaqueElement(parent, nodes)
   
   def toHTML(oP: ObjectPresenter, oe: OpaqueElement)(implicit rh : RenderingHandler) {
      rh(<pre>{oe.raw}</pre>)
   }
}