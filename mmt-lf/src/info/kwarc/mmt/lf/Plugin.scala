package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import frontend._

class Plugin extends frontend.Plugin {
   val theory = LF.theoryPath
   val dependencies = List("info.kwarc.mmt.morphisms.MorphismPlugin")
   override def start(args: List[String]) {
      val em = controller.extman
      // content enhancers
      em.addExtension(new NotationGenerator)
      em.addExtension(new SimplificationRuleGenerator)
      // build targets
      em.addExtension(new ScalaExporter)
      // Twelf parser
      em.addExtension(new TwelfParser)
      // foundation
      em.addExtension(new LFF)
   }
}

object LFHOAS extends notations.HOASNotation(LF.hoas)
