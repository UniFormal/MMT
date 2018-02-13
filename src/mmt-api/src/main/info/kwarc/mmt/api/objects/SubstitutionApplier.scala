package info.kwarc.mmt.api.objects

import info.kwarc.mmt.api._

/** A SubstitutionApplier is an abstraction for the substitution application function.
 *
 * Its purpose is to permit optimized versions of substitution that share the traversal and capture-avoidance code.
 *
 * A SubstitutionApplier is carried along as an implicit argument of Obj.^^, which implements the shared parts.
 */
abstract class SubstitutionApplier {
   /** the case for complex terms
    *
    *  When using Obj.^^, this is not called on symbols, variables, and literals.
    */
    def apply(o: Obj, sub: Substitution): o.ThisType
}

/**
 * the default SubstitutionApplier defined in the usual way
 *
 * This is used by Obj.^
 */
object PlainSubstitutionApplier extends SubstitutionApplier {
   def apply(o: Obj, sub: Substitution): o.ThisType = o.substitute(sub)(this)
}

/**
 * A SubstitutionApplier that only recurses if the subobject has a free variable
 * that is non-trivially affected by the substitution.
 */
object SmartSubstitutionApplier extends SubstitutionApplier {
   def apply(o: Obj, sub: Substitution): o.ThisType =
      if (o.freeVars.exists(f => sub.exists(s => s.name == f && s.target != OMV(f)))) o.substitute(sub)(this)
      else o
}

/**
 * like SmartSubstitutionApplier but also preserves structure sharing
 */
// TODO currently not used, needs testing
object StructureSharingSubstitutionApplier extends SubstitutionApplier {
   /**
    * stores a client property (sub,oS) in every object o for which o ^ sub == oS
    */
   private object SubstitutionResult extends TermProperty[(Substitution,Obj)](utils.mmt.baseURI / "clientProperties" / "sub" / "subresult")
   def apply(o: Obj, sub: Substitution): o.ThisType =
      if (o.freeVars.exists(f => sub.exists(s => s.name == f && s.target != OMV(f)))) {
         SubstitutionResult.get(o) match {
            case Some((s,oS)) if s == sub => oS.asInstanceOf[o.ThisType] //cast always succeeds for values put by this object
            case _ =>
               val oS = o.substitute(sub)(this)
               SubstitutionResult.put(o,(sub,oS))
               oS
         }
      } else o
}

/**
 * Like SmartSubstitutionApplier, but remembers previous applications.
 */
class MemoizedSubstitutionApplier extends SubstitutionApplier {
   private val memory = new scala.collection.mutable.HashMap[(Int,Int), Obj]
   def apply(o: Obj, sub: Substitution): o.ThisType =
      memory.get((o.hash, sub.hash)) match {
         case Some(result) => result.asInstanceOf[o.ThisType]
         case None =>
            if (o.freeVars.exists(f => sub.exists(s => s.name == f && s.target != OMV(f)))) {
               val oS = o.substitute(sub)(this)
               memory((o.hash,sub.hash)) = oS
               oS
            } else
               o
      }
}
