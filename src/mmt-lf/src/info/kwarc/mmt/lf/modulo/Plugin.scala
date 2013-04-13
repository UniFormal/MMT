package info.kwarc.mmt.lf.modulo

import info.kwarc.mmt.api._
import info.kwarc.mmt.lf._

class Plugin extends frontend.Plugin {
   val dependencies = List("info.kwarc.mmt.lf.Plugin")
   override def init(c: frontend.Controller, args: List[String]) {
      super.init(c, args)
      val em = c.extman
      em.ruleStore.add(RewriteTerm)
      em.addExtension("info.kwarc.mmt.lf.modulo.RewriteRuleGenerator", Nil)
      //TODO this should go away
      object HOAS extends pragmatics.HOAS {
         val theory = LFModulo._path
         val apply = Apply.path
         val lambda = Lambda.path
      }
      em.pragmaticStore.add(HOAS)
   }
}