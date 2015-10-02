package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import frontend._
import checking._
import objects._
import modules._

/** simplifies/rewrites objects */
trait ObjectSimplifier extends Extension {
   def apply(obj: Obj, context: Context, rules: RuleSet): obj.ThisType
}

/** simplifies/elaborates structural elements */
trait StructureSimplifier extends Extension {
   def apply(se: StructuralElement)
   def flatten(t: DeclaredTheory): Unit
   def materialize(context: Context, exp: Term, expandDefs: Boolean, pathOpt: Option[MPath]): Module
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