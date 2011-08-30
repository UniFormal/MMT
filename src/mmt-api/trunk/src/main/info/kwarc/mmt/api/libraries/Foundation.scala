package info.kwarc.mmt.api.libraries
import info.kwarc.mmt.api._
import objects._
import frontend._

abstract class Foundation(report: Report) {
   val foundTheory : MPath
   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit lib : Lookup) : Boolean
   def equality(tm1 : Term, tm2 : Term)(implicit lib : Lookup) : Boolean
}

class DefaultFoundation(report: Report) extends Foundation(report) {
   val foundTheory = utils.mmt.mmtcd 
   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit lib : Lookup) : Boolean =
      true
      //tm.isEmpty || tp.isEmpty || tp == Some(OMHID())
   def equality(tm1 : Term, tm2 : Term)(implicit lib : Lookup) : Boolean = 
      tm1 == tm2
}

