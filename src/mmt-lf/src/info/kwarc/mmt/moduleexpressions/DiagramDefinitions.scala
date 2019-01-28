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
     var oldNew: List[(LocalName,LocalName)] = Nil
     val modules = ad.getElements.mapOrSkip {e =>
       e.label match {
         case Common.ExistingName(_) => throw SkipThis
         case _ =>
       }
       val path = labelToPath(e.label)
       val name = path.name
       oldNew ::= (e.label,LocalName(path))
       e match {
         case node: DiagramNode =>
           val anonThy = node.theory
           val df = TermContainer(anonThy.toTerm)
           val thy = Theory(dm.parent, name, anonThy.mt, df = df)
           thy
         case arrow: DiagramArrow =>
           val anonMorph = arrow.morphism
           val df = anonMorph.toTerm
           val vw = View(dm.parent, name, OMMOD(labelToPath(arrow.from)), OMMOD(labelToPath(arrow.to)), Some(df), arrow.isImplicit)
           vw
       }
     }
     val adP = ad.relabel(l => utils.listmap(oldNew, l).getOrElse(l))
     dm.dfC.normalized = Some(adP.toTerm)
     modules
   }
}