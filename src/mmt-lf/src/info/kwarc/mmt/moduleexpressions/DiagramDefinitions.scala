package info.kwarc.mmt.moduleexpressions

import info.kwarc.mmt.api._
import modules._
import symbols._
import checking._
import objects._
import notations._
import utils._

object DiagramDefinition {
  val feature = "diagram"
}

class DiagramDefinition extends ModuleLevelFeature(DiagramDefinition.feature) {
   def getHeaderNotation = Nil
   
   /**  */
   def check(dm: DerivedModule)(implicit env: ExtendedCheckingEnvironment) {}
   
   override def modules(dm: DerivedModule) = {
     val diag = dm.dfC.normalized.getOrElse {throw LocalError("no definiens found")}
     val ad = diag match {
       case AnonymousDiagramCombinator(ad) => ad
       case df => throw LocalError("definiens not a diagram: " + controller.presenter.asString(df)) // TODO should use proper error handler 
     }
     def labelToTerm(l: LocalName) = OMMOD(dm.path / l) 
     val modules = ad.getElements.mapOrSkip {e =>
       val isNew = e.label match {
         case LocalName(List(SimpleStep(_))) => true
         case _ => false
       }
       if (!isNew) SkipThis()
       val name = dm.name / e.label
       e match {
         case node: DiagramNode =>
           val anonThy = node.theory
           val df = TermContainer(anonThy.toTerm)
           val thy = Theory(dm.parent, name, anonThy.mt, df = df)
           thy
         case arrow: DiagramArrow =>
           val isImplicit = ad.distArrow contains arrow.label 
           val anonMorph = arrow.morphism
           val df = anonMorph.toTerm
           val vw = View(dm.parent, name, labelToTerm(arrow.from), labelToTerm(arrow.to), Some(df), isImplicit)
           vw
       }
     }
     modules
   }
}