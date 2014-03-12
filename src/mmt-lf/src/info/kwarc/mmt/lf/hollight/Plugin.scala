package info.kwarc.mmt.lf.hollight

import info.kwarc.mmt._
import api._
import frontend._
import lf._

class Plugin extends frontend.Plugin {
   val dependencies = List("info.kwarc.mmt.lf.Plugin")
   override def start(args: List[String]) {
      val em = controller.extman
      em.notationExtensions ::= new notations.NestedHOASNotation(HOLLight.logic, HOLLight.hoas, LF.hoas)
   }
}
