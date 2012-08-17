package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import frontend._

class Plugin extends frontend.Plugin {
   val dependencies = List("info.kwarc.mmt.lf.TypedPlugin")
   def init(c: Controller, args: List[String]) {
      val em = c.extman
      em.ruleStore.add(PiType,PiTerm,ApplyTerm,LambdaTerm,Beta,Extensionality,Initial,Solve,ExpandArrow)
      em.pragmaticStore.add(LFHOAS)
   }
}

class TypedPlugin extends frontend.Plugin {
   val dependencies = Nil
   def init(c: Controller, args: List[String]) {
      val em = c.extman
      em.ruleStore.add(UniverseType,UniverseKind,UnivTerm)
   }
}