package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import frontend._

class Plugin extends frontend.Plugin {
   val dependencies = List("info.kwarc.mmt.lf.TypedPlugin")
   override def init(c: Controller, args: List[String]) {
      super.init(c, args)
      val em = c.extman
      // type reconstruction rules
      em.ruleStore.add(PiType,PiTerm,ApplyTerm,LambdaTerm,Beta,Extensionality,Initial,Solve,ExpandArrow)
      // computation rules
      em.ruleStore.add(UnsafeBeta)
      // proof rules
      em.ruleStore.add(PiIntroRule, PiElimRule, ArrowIntroRule, ArrowElimRule)
      // pragmatic features
      em.pragmaticStore.add(LFHOAS, LFTyping)
   }
}

class TypedPlugin extends frontend.Plugin {
   val dependencies = Nil
   override def init(c: Controller, args: List[String]) {
      super.init(c, args)
      val em = c.extman
      em.ruleStore.add(UniverseType,UniverseKind,UnivTerm)
   }
}

/* A temporary Plugin that provides pragmatic notations for the LF produced by Twelf */
class OldLFPlugin extends frontend.Plugin {
   val dependencies = Nil
   override def init(c: Controller, args: List[String]) {
      super.init(c, args)
      val em = c.extman
      em.pragmaticStore.add(OldLFHOAS)
   }
}
