package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import frontend._
import checking._
import objects._
import symbols._
import modules._

/** simplifies/rewrites objects */
trait ObjectSimplifier extends Extension {self =>
   def apply(obj: Obj, context: Context, rules: RuleSet): obj.ThisType
   
   def toTranslator(rules: RuleSet) = new UniformTranslator {
     def apply(c: Context, t: Term) = self.apply(t, c, rules) 
   }
}

/** simplifies/elaborates structural elements */
trait StructureSimplifier extends Extension {
   /** convenience abbreviation */
   def apply(p: ContentPath) {
     apply(controller.get(p))
   }
   /** flattens and elaborates the dependency closure of this element, typically called on a [[DeclaredTheory]] */
   def apply(se: StructuralElement): Unit
   
   def materialize(context: Context, exp: Term, expandDefs: Boolean, pathOpt: Option[MPath]): Module
   
   def getBody(context: Context, moduleExp: Term): ElementContainer[NamedElement]
}

/**
 * the designated super class of all simplifiers
 */
abstract class Simplifier(val objectLevel: ObjectSimplifier) extends StructureSimplifier with LeveledExtension {
   def apply(obj: Obj, context: Context, rules: RuleSet): obj.ThisType = objectLevel(obj, context, rules)
   def apply(obj: Obj, context: Context): obj.ThisType = {
      val rules = RuleSet.collectRules(controller, context)
      apply(obj, context, rules)
   }
}