package info.kwarc.mmt.lf

import info.kwarc.mmt.api._
import frontend._

class TypedPlugin extends frontend.Plugin {
   val dependencies = Nil
   override def start(args: List[String]) {
      val em = controller.extman
      em.ruleStore.add(UniverseType,UniverseKind,UnivTerm)
   }
}

class Plugin extends frontend.Plugin {
   val dependencies = List("info.kwarc.mmt.lf.TypedPlugin")
   override def start(args: List[String]) {
      val em = controller.extman
      // type reconstruction rules
      em.ruleStore.add(PiType,PiTerm,ApplyTerm,LambdaTerm,
            Beta,Extensionality,PiCongruence,LambdaCongruence,
            Solve,ExpandArrow)
      // computation rules
      em.ruleStore.add(UnsafeBeta)
      // proof rules
      em.ruleStore.add(PiIntroRule, PiElimRule, ArrowIntroRule, ArrowElimRule)
      // content enhancers
      em.addExtension(new NotationGenerator)
      em.addExtension(new SimplificationRuleGenerator)
      // build targets
      em.addExtension(new ScalaExporter)
      // pragmatic features
      em.notationExtensions ::= new notations.HOASNotation(LF._path, LF.hoas)
   }
}

class ShallowPolymorphismPlugin extends frontend.Plugin {
   val dependencies = List("info.kwarc.mmt.lf.Plugin")
   override def start(args: List[String]) {
      val em = controller.extman
      em.ruleStore.add(ShallowPolymorphism)
   }
}