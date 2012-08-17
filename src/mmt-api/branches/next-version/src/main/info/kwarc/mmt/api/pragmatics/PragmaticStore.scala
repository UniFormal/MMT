package info.kwarc.mmt.api.pragmatics
import info.kwarc.mmt.api._
import objects._

//experimental

class PragmaticStore {

}

abstract class Feature

class FeatureInstance(feature: Feature, theory: MPath)


abstract class Application extends Feature {
   val apply: GlobalName
   def makeStrict(fun: Term, args: List[Term]) = OMA(OMID(apply), fun :: args)
   def makePragmatic(t: Term) = t match {
      case OMA(OMID(this.apply), fun :: args) => OMA(fun, args)
      case _ => t
   }
}

abstract class Binding extends Application {
   val lambda: GlobalName
   def makeStrict(binder: Term, context: Context, scope: Term) : Term = makeStrict(binder, List(OMBIND(OMID(lambda), context, scope)))
   override def makePragmatic(t: Term) = t match {
      case OMA(OMID(this.apply), List(binder, OMBIND(OMID(this.lambda), context, scope))) => OMBIND(binder, context, scope)
      case OMA(OMID(this.apply), fun :: args) => OMA(fun, args)
      case _ => t
   }
}

//what about the other typing judgements - often there is more than one
abstract class Typing extends Feature {
   val hastype : GlobalName 
   def makeStrict(j: objects.Typing) = Inhabitation(j.stack, OMA(OMID(hastype), List(j.tm, j.tp)))
   def makePragmatic(j: Judgement) = j match {
      case Inhabitation(stack, OMA(OMID(this.hastype), List(tm, tp))) => Typing(stack, tm, tp)
      case _ => j
   }
}

abstract class Equality extends Feature {
   val logrel : MPath 
}