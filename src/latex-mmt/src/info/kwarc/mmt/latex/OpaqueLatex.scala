package info.kwarc.mmt.latex

import info.kwarc.mmt.api._
import opaque._

import scala.xml._

class LatexInterpreter extends TextInterpreter {
   override def logPrefix = "opaque_latex"
   override val format = "latex"
   override val formatAlias = List("L")
}
