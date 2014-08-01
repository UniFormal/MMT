package info.kwarc.mmt.morphisms

import info.kwarc.mmt._
import info.kwarc.mmt.api._

class MorphismPlugin extends frontend.Plugin {
   val dependencies = Nil
   override def start(args: List[String]) {
      val em = controller.extman
      //em.ruleStore.add(TheoryTypeInhabitable,TheoryTypeUniverse,MorphTypeInhabitable,ComplexTheoryInfer,MorphCheck)
      em.addExtension(new InstanceElaborator)
   }
}