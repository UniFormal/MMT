package info.kwarc.mmt.api.opaque

import info.kwarc.mmt.api._
import parser._
import presentation._
import checking._
import objects._

/** used to extend [[StructureParser]]s */
trait OpaqueTextParser extends OpaqueElementInterpreter {
   /** @param pu the text to read (does not necessarily represent a [[Term]]) */
   def fromString(oP: ObjectParser, parent: DPath, pu: ParsingUnit)(implicit eh: ErrorHandler): OE
}

/** used to extend the [[StructureChecker]]s */
trait OpaqueChecker extends OpaqueElementInterpreter {
   def check(oC: ObjectChecker, context: Context, rules: RuleSet, oe : OpaqueElement)(implicit ce: CheckingEnvironment): Unit
}

/** used to extend [[HTMLPresenter]]s */
trait OpaqueHTMLPresenter extends OpaqueElementInterpreter {
   def toHTML(oP: ObjectPresenter, oe: OpaqueElement)(implicit rh : RenderingHandler): Unit
}