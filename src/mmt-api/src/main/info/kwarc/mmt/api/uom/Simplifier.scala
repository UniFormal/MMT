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
   def apply(obj: Obj, context: Context, rules: RuleSet, expandDefinitions: Boolean): obj.ThisType
   
   def toTranslator(rules: RuleSet, expDef: Boolean) = new UniformTranslator {
     def apply(c: Context, t: Term) = self.apply(t, c, rules, expDef)
   }
}

class SimplificationEnvironment(val covered: Boolean, val errorCont: ErrorHandler, val task: MMTTask)

object TrivialSimplificationEnvironment extends SimplificationEnvironment(true, ErrorThrower, MMTTask.generic)

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
   def apply(se: StructuralElement) {applyChecked(se)(TrivialSimplificationEnvironment)}
   
   /** like apply but takes an extra argument to handle checking and error reporting */
   def applyChecked(se: StructuralElement)(implicit env: SimplificationEnvironment): Unit
   /** like apply except that for container elements applyElementBegin + apply on every child + applyElementEnd = apply */ 
   def applyElementBegin(se: StructuralElement)(implicit env: SimplificationEnvironment): Unit
   /** called in conjunction with applyElementBegin */ 
   def applyElementEnd(ce: ContainerElement[_])(implicit env: SimplificationEnvironment): Unit

   def materialize(context: Context, exp: Term, pathOpt: Option[MPath], tcOpt: Option[TermContainer]): DeclaredModule

   def getBody(context: Context, moduleExp: Term): ElementContainer[NamedElement]

   def elaborateContext(outer: Context, con: Context) : Context
}

/**
 * the designated super class of all simplifiers
 */
abstract class Simplifier(val objectLevel: ObjectSimplifier) extends StructureSimplifier with LeveledExtension {
   def apply(obj: Obj, context: Context, rules: RuleSet, expDef: Boolean): obj.ThisType = objectLevel(obj, context, rules, expDef)
   def apply(obj: Obj, context: Context, expDef: Boolean = false): obj.ThisType = {
      val rules = RuleSet.collectRules(controller, context)
      apply(obj, context, rules, expDef)
   }
}
