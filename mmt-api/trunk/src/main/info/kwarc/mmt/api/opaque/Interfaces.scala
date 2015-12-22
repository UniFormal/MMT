package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import parser._
import presentation._

/** used to extend [[StructureParser]]s */
trait OpaqueTextParser[OE <: OpaqueElement] extends OpaqueElementInterpreter[OE] {
   def fromString(oP: ObjectParser, parent: DPath, pu: ParsingUnit)(implicit eh: ErrorHandler): OE
}

/** used to extend [[HTMLPresenter]]s */
trait OpaqueHTMLPresenter[OE <: OpaqueElement] extends OpaqueElementInterpreter[OE] {
   def toHTML(oP: ObjectPresenter, oe: OE)(implicit rh : RenderingHandler): Unit
   /** auxiliary function that tries to downcast the argument */
   def toHTMLForce(oP: ObjectPresenter, oe: OpaqueElement)(implicit rh : RenderingHandler) {
      toHTML(oP, force(oe))
   }
}