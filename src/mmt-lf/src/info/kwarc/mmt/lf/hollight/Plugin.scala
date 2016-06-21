package info.kwarc.mmt.lf.hollight

import info.kwarc.mmt._
import api._
import lf._

class Plugin extends frontend.Plugin {
   val theory = HOLLight.foundation
   val dependencies = List("info.kwarc.mmt.lf.Plugin")
   override def start(args: List[String]) {
      val em = controller.extman
   }
}

object HOLLightHOAS extends notations.NestedHOASNotation(HOLLight.hoas, LF.hoas)
