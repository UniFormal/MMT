package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import objects._
import frontend._

/** MMT Foundation: provides oracles for typing and equality. Concrete foundations are registered as plugins and maintained by @frontend.ExtensionManager
 * Concrete foundations must have a constructor that takes no arguments, which will be called after plugin registration */
abstract class Foundation {
   protected var report : Report = null
   /** called after registration of the plugin
    *  @param params user parameters (passed on the shell) */
   def init(r: Report, params: List[String] = Nil) {
      report = r
   }
   val foundTheory : MPath
   /** typing judgement */
   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit lib : Lookup) : Boolean
   /** equality judgement */
   def equality(tm1 : Term, tm2 : Term)(implicit lib : Lookup) : Boolean
   /** type inference */
   def infer(tm: Term, context: Context)(implicit lib: Lookup) : Term
}

/** Foundation where typing is always true and equality is identity */
class DefaultFoundation extends Foundation {
   val foundTheory = utils.mmt.mmtcd 
   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit lib : Lookup) : Boolean =
      true
      //tm.isEmpty || tp.isEmpty || tp == Some(OMHID())
   def equality(tm1 : Term, tm2 : Term)(implicit lib : Lookup) : Boolean = 
      tm1 == tm2
   def infer(tm: Term, context: Context)(implicit lib: Lookup) : Term = OMHID //TODO: questionable choice here, better introduce a special untyped foundation
}

