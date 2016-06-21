package info.kwarc.mmt.morphisms

import info.kwarc.mmt._
import info.kwarc.mmt.api._
import objects.ModExp

class MorphismPlugin extends frontend.Plugin {
   val theory = ModExp._path
   val dependencies = Nil
   override def start(args: List[String]) {
      val em = controller.extman
      em.addExtension(new InstanceElaborator)
   }
}
