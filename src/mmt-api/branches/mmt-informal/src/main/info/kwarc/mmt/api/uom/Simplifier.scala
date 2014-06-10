package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import frontend._
import objects._

/** simplifies/rewrites objects */
trait ObjectSimplifier extends Extension {
   def apply(obj: Obj, scope: Term, context: Context): obj.ThisType
}

/** simplifies/elaborates structural elements */
trait StructureSimplifier extends Extension {
   def apply(se: StructuralElement)
}

/**
 * the designated super class of all simplifiers
 */
abstract class Simplifier(val objectLevel: ObjectSimplifier) extends StructureSimplifier with LeveledExtension {
   def apply(obj: Obj, scope: Term, context: Context): obj.ThisType = objectLevel(obj, scope, context)
}