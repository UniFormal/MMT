package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import parser._
import presentation._

/** used to extend [[StructureParser]]s */
trait OpaqueTextParser[OE <: OpaqueElement] {self: OpaqueElementInterpreter[OE] =>
   def fromString(oP: ObjectParser, parent: DPath, pu: ParsingUnit)(implicit eh: ErrorHandler): OE
}

/** used to extend [[HTMLPresenter]]s */
trait OpaqueHTMLPresenter[OE <: OpaqueElement] {self: OpaqueElementInterpreter[OE] =>
   def toHTML(oP: ObjectPresenter, oe: OpaqueText)(implicit rh : RenderingHandler): Unit
}