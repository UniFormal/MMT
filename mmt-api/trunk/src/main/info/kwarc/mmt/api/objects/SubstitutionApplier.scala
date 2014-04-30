package info.kwarc.mmt.api.objects

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
      if (o.freeVars.exists(f => sub.subs.exists(s => s.name == f && s.target != OMV(f)))) o.substitute(sub)(this)
      else o
}

/** 
 *  Like SmartSubstitutionApplier, but remembers previous applications. 
 */
class MemoizedSubstitutionApplier extends SubstitutionApplier {
   private val memory = new scala.collection.mutable.HashMap[(Int,Int), Obj]
   def apply(o: Obj, sub: Substitution): o.ThisType =
      memory.get((o.hash, sub.hash)) match {
         case Some(result) => result.asInstanceOf[o.ThisType]
         case None => 
            if (o.freeVars.exists(f => sub.subs.exists(s => s.name == f && s.target != OMV(f)))) {
               val oS = o.substitute(sub)(this)
               memory((o.hash,sub.hash)) = oS
               oS
            } else
               o
      }
}