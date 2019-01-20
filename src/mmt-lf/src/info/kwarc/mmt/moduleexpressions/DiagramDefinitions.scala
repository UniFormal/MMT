package info.kwarc.mmt.moduleexpressions

import info.kwarc.mmt.api._
import modules._
import symbols._
import checking._
import objects._
import notations._
import utils._

class DiagramDefinitions extends ModuleLevelFeature("diagram") {
   def getHeaderNotation = Nil
   
   /** not sure if this is even called, need to wait for more experience with checking derived modules */
   def check(dm: DerivedModule)(implicit env: ExtendedCheckingEnvironment) {}
   
   override def modules(dm: DerivedModule) = {
     val diag: Term = ??? // need to normalize dm.df
     val ad = diag match {
       case AnonymousDiagramCombinator(ad) => ad
       case _ => throw LocalError("not a diagram") // TODO should use proper error handler 
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