package info.kwarc.mmt.api.libraries
import jomdoc._
import jomdoc.objects._

abstract class Foundation {
   def applicable(m : MPath) : Boolean
   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit lib : Lookup) : Boolean
   def equality(tm1 : Term, tm2 : Term)(implicit lib : Lookup) : Boolean
}

object DefaultFoundation extends Foundation {
   def applicable(m : MPath) : Boolean = true
   def typing(tm : Option[Term], tp : Option[Term], G : Context = Context())(implicit lib : Lookup) : Boolean =
      true
      //tm.isEmpty || tp.isEmpty || tp == Some(OMHID())
   def equality(tm1 : Term, tm2 : Term)(implicit lib : Lookup) : Boolean = 
      tm1 == tm2
}

