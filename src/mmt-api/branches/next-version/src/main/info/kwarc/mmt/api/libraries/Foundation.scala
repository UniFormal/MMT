package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import objects._
import frontend._

abstract class Foundation {
   protected var report : Report = NullReport
   def init(r: Report, params: List[String] = Nil) : Foundation = {report = r; this}
   val foundTheory : MPath
   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit lib : Lookup) : Boolean
   def equality(tm1 : Term, tm2 : Term)(implicit lib : Lookup) : Boolean
   def infer(tm: Term, context: Context)(implicit lib: Lookup) : Term
}

class DefaultFoundation extends Foundation {
   val foundTheory = utils.mmt.mmtcd 
   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit lib : Lookup) : Boolean =
      true
      //tm.isEmpty || tp.isEmpty || tp == Some(OMHID())
   def equality(tm1 : Term, tm2 : Term)(implicit lib : Lookup) : Boolean = 
      tm1 == tm2
   def infer(tm: Term, context: Context)(implicit lib: Lookup) : Term = OMHID //TODO: questionable choice here, better introduce a special untyped foundation
}

