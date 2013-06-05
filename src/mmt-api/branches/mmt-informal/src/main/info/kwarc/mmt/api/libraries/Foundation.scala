package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import objects._
import libraries._
import frontend._


import scala.collection.mutable.HashSet

abstract class FoundationLookup {
   val lup : Lookup
   def getType(p: GlobalName) : Option[Term]
   def getDefiniens(p: GlobalName) : Option[Term]
}

class PlainLookup(val lup: Lookup) extends FoundationLookup {
   def getType(p: GlobalName) : Option[Term] = {
      lup.getConstant(p).tp
   }
   def getDefiniens(p: GlobalName) : Option[Term] = {
      lup.getConstant(p).df
   }
}

class TracedLookup(val lup: Lookup) extends FoundationLookup {
   private val trace = new HashSet[CPath]
   def getType(p: GlobalName) : Option[Term] = {
      trace += CPath(p, TypeComponent)
      lup.getConstant(p).tp
   }
   def getDefiniens(p: GlobalName) : Option[Term] = {
      trace += CPath(p, DefComponent)
      lup.getConstant(p).df
   }
   def getTrace: HashSet[CPath] = trace
}

/** MMT Foundation: provides oracles for typing and equality. Concrete foundations are registered as plugins and maintained by @frontend.ExtensionManager
 * Concrete foundations must have a constructor that takes no arguments, which will be called after plugin registration */
abstract class Foundation extends Extension {
   val foundTheory : MPath
   
   def tracedTyping(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit lib : Lookup) : HashSet[CPath] = {
      val fl = new TracedLookup(lib)
      typing(tm, tp, G)(fl) match {
        case false => throw InvalidObject(tp.getOrElse(OMHID), "type checking failed")
        case true => fl.getTrace
      }
   }

   def tracedEquality(tm1 : Term, tm2 : Term)(implicit lib : Lookup) : HashSet[CPath] = {
      val fl = new TracedLookup(lib)
      equality(tm1, tm2)(fl)
      fl.getTrace
   }
   
   /** typing judgement */
   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit fl: FoundationLookup) : Boolean
   /** equality judgement */
   def equality(tm1 : Term, tm2 : Term)(implicit fl : FoundationLookup) : Boolean
   /** type inference */
   def inference(tm: Term, context: Context)(implicit lup: Lookup) : Term
}

/** Foundation where typing is always true and equality is identity */
class DefaultFoundation extends Foundation {
   val foundTheory = utils.mmt.mmtcd 
   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit fl : FoundationLookup) : Boolean =
      true
      //tm.isEmpty || tp.isEmpty || tp == Some(OMHID())
   def equality(tm1 : Term, tm2 : Term)(implicit fl : FoundationLookup) : Boolean = 
      tm1 == tm2
   def inference(tm: Term, context: Context)(implicit lib: Lookup) : Term = OMHID //TODO: questionable choice here, better introduce a special untyped foundation
}
