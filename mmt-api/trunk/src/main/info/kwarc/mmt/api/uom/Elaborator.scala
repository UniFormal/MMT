package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._

/** does nothing for now */
class StructureElaborator(objectSimplifier: ObjectSimplifier) extends Simplifier(objectSimplifier) {
   def apply(se: StructuralElement) {}
}