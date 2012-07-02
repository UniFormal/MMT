package org.omdoc.cds.unsorted.uom.omdoc
import jomdoc._
import jomdoc.objects._

class lists {
  private val base = DPath(new utils.xml.URI("http://cds.omdoc.org/unsorted/uom.omdoc"))
  
  val list = OMS(SPath(MPath(base, LocalPath(List("lists"))), LocalPath(List("list"))))
  val nil = OMS(SPath(MPath(base, LocalPath(List("lists"))), LocalPath(List("nil"))))
  val cons = OMS(SPath(MPath(base, LocalPath(List("lists"))), LocalPath(List("cons"))))
  val append = OMS(SPath(MPath(base, LocalPath(List("lists"))), LocalPath(List("append"))))

   // UOM start http://cds.omdoc.org/unsorted/uom.omdoc?lists?append_*
   def append_*(l: Term, m: Term) : Term = {
    
   }
   // UOM end
}

class lists_ext {
    private val base = DPath(new utils.xml.URI("http://cds.omdoc.org/unsorted/uom.omdoc"))
    
    val lists = new lists
    val append_many = OMS(SPath(MPath(base, LocalPath(List("lists_ext"))), LocalPath(List("append_many"))))

    // UOM start http://cds.omdoc.org/unsorted/uom.omdoc?lists_ext?append_*
    def append_many_*(l: Term*) : Term = {
          null
    //    l.toList match {
    //        case lists.nil => lists.nil
    //        case OMA(lists.cons, List(hd)) => hd
    //        case OMA(lists.cons, hd :: tl) => lists.append_*(hd, append_many_*(tl))
    //     }

    }
   // UOM end
}

object Run
{
  def main(args : Array[String]) {
    val obj = new lists_ext
    val result = obj.append_many_*()
    if (result == null)
      println("Result is null")
    else
      println(result.toString)
  }
}
