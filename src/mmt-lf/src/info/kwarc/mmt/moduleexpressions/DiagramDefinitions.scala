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
     /* defines the naem of the generated modules */
     def labelToName(l: LocalName) = LocalName(dm.name.toPath + "_" + l.toPath)
     def labelToPath(l: LocalName) = l match {
       case Common.ExistingName(p) => p
       case _ => dm.parent ? labelToName(l)
     }
     val modules = ad.getArrows.mapOrSkip {e =>
       val isNew = e.label match {
         case Common.ExistingName(_) => false
         case _ => true
       }
       if (!isNew) throw SkipThis
       val name = labelToName(e.label)
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
           val vw = View(dm.parent, name, OMMOD(labelToPath(arrow.from.label)), OMMOD(labelToPath(arrow.to.label)), Some(df), isImplicit)
           vw
       }
     }
     modules
   }
}