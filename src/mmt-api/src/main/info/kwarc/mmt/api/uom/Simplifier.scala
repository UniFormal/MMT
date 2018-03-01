package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import frontend._
import checking._
import objects._
import symbols._
import modules._

/** simplifies/rewrites objects */
trait ObjectSimplifier extends Extension {self =>
   /** applies rules to simplify an object */
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
   /** elaborates one element and relevant parts of its dependency closure
    *  
    *  This is typically used for assumed-correct content that is not checked, e.g., content loaded directly from OMDoc
    *  For container elements that are checked incrementally, one may alternatively use applyElementBegin and applyElementEnd
    */
   def apply(se: StructuralElement): Unit
   /** like apply except that for container elements applyElementBegin + apply on every child + applyElementEnd = apply */ 
   def applyElementBegin(se: StructuralElement): Unit
   /** called in conjunction with applyElementBegin */ 
   def applyElementEnd(ce: ContainerElement[_]): Unit

   def materialize(context: Context, exp: Term, pathOpt: Option[MPath], tcOpt: Option[TermContainer]): DeclaredModule

   def getBody(context: Context, moduleExp: Term): ElementContainer[NamedElement]

   def elaborateContext(outer: Context, con: Context) : Context
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
