package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import symbols._
import objects._
import checking._
import frontend._

abstract class OpaqueElement extends NarrativeElement {
   def format: String
   def raw: String
}

/**
 * an extension that provides (parts of) the meaning of [[OpaqueElement]]s
 */
abstract class OpaqueElementInterpreter[OE <: OpaqueElement] extends Extension {
   /** the format of opaque declaration this can interpret */
   def format : String
   
   def toString(oc: OE): String

   def fromNode(node: scala.xml.Node, base: Path): OE
   def toNode(oc: OpaqueElement): scala.xml.Node
}


trait OpaqueTextParser[OE <: OpaqueElement] {self: OpaqueElementInterpreter[OE] =>
   def fromString(s: String): OE
}

trait OpaqueHTMLPresenter[OE <: OpaqueElement] {self: OpaqueElementInterpreter[OE] =>
   def toHTML(oe: OE): String
}

abstract class UnknownOpaqueElement(val parent: Path, val raw: String) extends OpaqueElement {
   def format = "unknown"
   override def toString = raw
   def toNode = <opaque format="unknown">{raw}</opaque>
}

abstract class DefalultOpaqueElementInterpreter extends OpaqueElementInterpreter[UnknownOpaqueElement] with OpaqueHTMLPresenter[UnknownOpaqueElement] {
   def format = "unknown"
   def toHTML(oe: UnknownOpaqueElement) = s"<pre>${oe.raw}</pre>"
}