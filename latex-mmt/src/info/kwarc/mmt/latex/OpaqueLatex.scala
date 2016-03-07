package info.kwarc.mmt.latex

import info.kwarc.mmt.api._
import opaque._

import scala.xml._

class LatexInterpreter extends DefaultOpaqueElementInterpreter {
   override def logPrefix = "opaque_latex"
   override def format = "latex"
   override def isApplicable(f: String) = super.isApplicable(f) || f == "L"
}