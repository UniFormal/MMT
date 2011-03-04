package org.omdoc.cds.unsorted.uom.omdoc
import jomdoc._
import jomdoc.objects._

class lists {
   private val base = DPath(new xml.URI("http://cds.omdoc.org/unsorted/uom.omdoc"))
   val elem = OMS(base ? "elem")
   val list = OMS(base ? "list")
   val nil  = OMS(base ? "nil")
   val cons = OMS(base ? "cons")
   val append = OMS(base ? "append")
   
   // UOM start http://cds.omdoc.org/unsorted/uom.omdoc?lists?append_*
   def append_*(l: Term, m: Term) : Term = {
     null
   }
   // UOM end
}

class lists_ext {
    private val base = DPath(new xml.URI("http://cds.omdoc.org/unsorted/uom.omdoc"))
    
    val lists = new lists
    val append_many = OMS(base ? "append_many")

    // UOM start http://cds.omdoc.org/unsorted/uom.omdoc?lists_ext?append_*
    def append_many_*(l: Term*) : Term = {
        l.toList match {
            case lists.nil => lists.nil
            case OMA(lists.cons, List(hd)) => hd
            case OMA(lists.cons, hd :: tl) => lists.append_*(hd, append_many_*(tl))
         }
    }
   // UOM end
}
