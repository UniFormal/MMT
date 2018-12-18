package info.kwarc.mmt.api.uom

import info.kwarc.mmt.api._
import frontend._
import checking._
import objects._
import symbols._
import modules._

/** simplifies/rewrites objects */
trait ObjectSimplifier extends Extension {self =>
   /** applies rules to simplify an object
    *  @param obj the object to simplify
    *  @param su the simplification unit (obj should be part of this, but the Scala type system can't handle it)
    *  @param rules the rules of that context (provided for efficiency)
    */
   def apply(obj: Obj, su: SimplificationUnit, rules: RuleSet): obj.ThisType
   
   def toTranslator(rules: RuleSet, expDef: Boolean) = new UniformTranslator {
     def apply(c: Context, t: Term) = self.apply(t, SimplificationUnit(c, expDef, true), rules)
   }
}

/** applies rules to simplify an object
 *  @param context the context of the object to simplify
 *  @param expandDefinitions if true, expand all definitions of constant (we speak of *deep* simplification) 
 *  @param fullRecursion if false, only recurse into subexpressions that contribute to head-normalizations 
 */
case class SimplificationUnit(context: Context, expandDefinitions: Boolean, fullRecursion: Boolean,solverO :Option[Solver] = None) extends MMTTask {
  def ++(c: Context) = copy(context = context ++ c)
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
   def apply(se: StructuralElement) {applyChecked(se)(TrivialSimplificationEnvironment)}
   
   /** like apply but takes an extra argument to handle checking and error reporting */
   def applyChecked(se: StructuralElement)(implicit env: SimplificationEnvironment): Unit
   /** like apply except that for container elements applyElementBegin + apply on every child + applyElementEnd = apply */ 
   def applyElementBegin(se: StructuralElement)(implicit env: SimplificationEnvironment): Unit
   /** called in conjunction with applyElementBegin */ 
   def applyElementEnd(ce: ContainerElement[_])(implicit env: SimplificationEnvironment): Unit

   def materialize(context: Context, exp: Term, pathOpt: Option[MPath], tcOpt: Option[TermContainer]): Module

   def getBody(context: Context, moduleExp: Term): ElementContainer[NamedElement]

   def elaborateContext(outer: Context, con: Context) : Context
}

/**
 * the designated super class of all simplifiers
 */
abstract class Simplifier(val objectLevel: ObjectSimplifier) extends StructureSimplifier with LeveledExtension {
   def apply(obj: Obj, su: SimplificationUnit, rules: RuleSet): obj.ThisType = objectLevel(obj, su, rules)
   def apply(obj: Obj, su: SimplificationUnit): obj.ThisType = {
      val rules = RuleSet.collectRules(controller, su.context)
      apply(obj, su, rules)
   }
   
   /** legacy interface */
   @deprecated("use other apply methods instead", "")
   def apply(obj: Obj, context: Context, expDef: Boolean = false): obj.ThisType = apply(obj, SimplificationUnit(context, expDef, true))
   @deprecated("use other apply methods instead", "")
   def apply(obj: Obj, context: Context, rules: RuleSet, expDef: Boolean): obj.ThisType = apply(obj, SimplificationUnit(context, expDef, true), rules)
}

class SimplificationEnvironment(val covered: Boolean, val errorCont: ErrorHandler, val task: MMTTask)

object TrivialSimplificationEnvironment extends SimplificationEnvironment(true, ErrorThrower, MMTTask.generic)